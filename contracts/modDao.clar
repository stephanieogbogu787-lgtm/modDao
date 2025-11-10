
;; title: mod-dao
;; version: 1.0.0
;; summary: Decentralized content moderation network with stake-weighted decisions
;; description: A crowdsourced content moderation system where moderators stake tokens
;; on violation classifications, with transparent appeals process and reputation tracking

;; traits
;;

;; token definitions
;; (SIP-010 fungible token would be integrated for staking)

;; constants
;;
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-status (err u103))
(define-constant err-insufficient-stake (err u104))
(define-constant err-already-voted (err u105))
(define-constant err-appeal-period-expired (err u106))
(define-constant err-invalid-classification (err u107))
(define-constant err-already-appealed (err u108))

(define-constant status-pending u0)
(define-constant status-under-review u1)
(define-constant status-approved u2)
(define-constant status-rejected u3)
(define-constant status-appealed u4)

(define-constant min-moderator-stake u1000)
(define-constant min-appeal-stake u2000)
(define-constant appeal-window-blocks u144) ;; ~24 hours

;; Violation classifications
(define-constant violation-spam u1)
(define-constant violation-harassment u2)
(define-constant violation-hate-speech u3)
(define-constant violation-misinformation u4)
(define-constant violation-explicit u5)
(define-constant violation-none u0)

;; data vars
;;
(define-data-var next-request-id uint u0)
(define-data-var platform-count uint u0)

;; data maps
;;

;; Platform registration
(define-map platforms
  principal
  {
    name: (string-ascii 50),
    active: bool,
    total-requests: uint
  }
)

;; Moderation requests
(define-map moderation-requests
  uint
  {
    platform: principal,
    content-hash: (buff 32),
    submitter: principal,
    status: uint,
    created-at: uint,
    final-classification: uint,
    total-stake-for: uint,
    total-stake-against: uint,
    appeal-deadline: uint
  }
)

;; Moderator votes
(define-map moderator-votes
  { request-id: uint, moderator: principal }
  {
    classification: uint,
    stake-amount: uint,
    voted-at: uint,
    is-appeal: bool
  }
)

;; Moderator reputation
(define-map moderator-reputation
  principal
  {
    total-decisions: uint,
    correct-decisions: uint,
    total-stake: uint,
    regional-weight: uint ;; 1-100, higher for regional expertise
  }
)

;; Regional context weights (moderator -> region code)
(define-map regional-expertise
  { moderator: principal, region: (string-ascii 10) }
  uint ;; expertise level 1-100
)

;; Appeal records
(define-map appeals
  uint
  {
    appellant: principal,
    appeal-stake: uint,
    appealed-at: uint,
    original-classification: uint,
    new-classification: uint
  }
)

;; public functions
;;

;; Platform registration
(define-public (register-platform (name (string-ascii 50)))
  (let
    (
      (platform-data {
        name: name,
        active: true,
        total-requests: u0
      })
    )
    (ok (map-set platforms tx-sender platform-data))
  )
)

;; Submit content for moderation
(define-public (submit-moderation-request (content-hash (buff 32)))
  (let
    (
      (request-id (var-get next-request-id))
      (platform-info (unwrap! (map-get? platforms tx-sender) err-unauthorized))
    )
    (asserts! (get active platform-info) err-unauthorized)
    (map-set moderation-requests request-id {
      platform: tx-sender,
      content-hash: content-hash,
      submitter: tx-sender,
      status: status-pending,
      created-at: block-height,
      final-classification: violation-none,
      total-stake-for: u0,
      total-stake-against: u0,
      appeal-deadline: u0
    })
    (map-set platforms tx-sender 
      (merge platform-info { total-requests: (+ (get total-requests platform-info) u1) })
    )
    (var-set next-request-id (+ request-id u1))
    (ok request-id)
  )
)

;; Moderator votes on content
(define-public (vote-on-content 
  (request-id uint) 
  (classification uint) 
  (stake-amount uint)
  (region (string-ascii 10)))
  (let
    (
      (request (unwrap! (map-get? moderation-requests request-id) err-not-found))
      (existing-vote (map-get? moderator-votes { request-id: request-id, moderator: tx-sender }))
      (moderator-rep (default-to 
        { total-decisions: u0, correct-decisions: u0, total-stake: u0, regional-weight: u50 }
        (map-get? moderator-reputation tx-sender)
      ))
      (regional-weight (default-to u50 
        (map-get? regional-expertise { moderator: tx-sender, region: region })
      ))
    )
    (asserts! (is-none existing-vote) err-already-voted)
    (asserts! (>= stake-amount min-moderator-stake) err-insufficient-stake)
    (asserts! (or (is-eq (get status request) status-pending) 
                  (is-eq (get status request) status-under-review)) 
              err-invalid-status)
    (asserts! (and (>= classification violation-none) (<= classification violation-explicit)) 
              err-invalid-classification)
    
    ;; Record vote
    (map-set moderator-votes 
      { request-id: request-id, moderator: tx-sender }
      {
        classification: classification,
        stake-amount: (* stake-amount regional-weight),
        voted-at: block-height,
        is-appeal: false
      }
    )
    
    ;; Update request with weighted stake
    (map-set moderation-requests request-id
      (merge request {
        status: status-under-review,
        total-stake-for: (if (> classification violation-none)
          (+ (get total-stake-for request) (* stake-amount regional-weight))
          (get total-stake-for request)
        ),
        total-stake-against: (if (is-eq classification violation-none)
          (+ (get total-stake-against request) (* stake-amount regional-weight))
          (get total-stake-against request)
        )
      })
    )
    
    ;; Update moderator reputation
    (map-set moderator-reputation tx-sender
      (merge moderator-rep {
        total-decisions: (+ (get total-decisions moderator-rep) u1),
        total-stake: (+ (get total-stake moderator-rep) stake-amount)
      })
    )
    
    (ok true)
  )
)

;; Finalize moderation decision
(define-public (finalize-decision (request-id uint))
  (let
    (
      (request (unwrap! (map-get? moderation-requests request-id) err-not-found))
    )
    (asserts! (is-eq (get status request) status-under-review) err-invalid-status)
    
    (let
      (
        (final-class (if (> (get total-stake-for request) (get total-stake-against request))
          violation-harassment ;; Simplified: would use most-voted classification
          violation-none
        ))
      )
      (map-set moderation-requests request-id
        (merge request {
          status: status-approved,
          final-classification: final-class,
          appeal-deadline: (+ block-height appeal-window-blocks)
        })
      )
      (ok final-class)
    )
  )
)

;; Submit appeal
(define-public (submit-appeal (request-id uint) (appeal-stake uint))
  (let
    (
      (request (unwrap! (map-get? moderation-requests request-id) err-not-found))
      (existing-appeal (map-get? appeals request-id))
    )
    (asserts! (is-none existing-appeal) err-already-appealed)
    (asserts! (is-eq (get status request) status-approved) err-invalid-status)
    (asserts! (<= block-height (get appeal-deadline request)) err-appeal-period-expired)
    (asserts! (>= appeal-stake min-appeal-stake) err-insufficient-stake)
    
    (map-set appeals request-id {
      appellant: tx-sender,
      appeal-stake: appeal-stake,
      appealed-at: block-height,
      original-classification: (get final-classification request),
      new-classification: violation-none
    })
    
    (map-set moderation-requests request-id
      (merge request { status: status-appealed })
    )
    
    (ok true)
  )
)

;; Vote on appeal (higher stakes required)
(define-public (vote-on-appeal 
  (request-id uint) 
  (classification uint) 
  (stake-amount uint))
  (let
    (
      (request (unwrap! (map-get? moderation-requests request-id) err-not-found))
      (appeal (unwrap! (map-get? appeals request-id) err-not-found))
      (existing-vote (map-get? moderator-votes { request-id: request-id, moderator: tx-sender }))
    )
    (asserts! (is-none existing-vote) err-already-voted)
    (asserts! (is-eq (get status request) status-appealed) err-invalid-status)
    (asserts! (>= stake-amount min-appeal-stake) err-insufficient-stake)
    
    (map-set moderator-votes 
      { request-id: request-id, moderator: tx-sender }
      {
        classification: classification,
        stake-amount: stake-amount,
        voted-at: block-height,
        is-appeal: true
      }
    )
    
    (ok true)
  )
)

;; Set regional expertise
(define-public (set-regional-expertise (region (string-ascii 10)) (weight uint))
  (begin
    (asserts! (and (>= weight u1) (<= weight u100)) err-invalid-classification)
    (ok (map-set regional-expertise 
      { moderator: tx-sender, region: region }
      weight
    ))
  )
)

;; Deactivate platform
(define-public (deactivate-platform)
  (let
    (
      (platform-info (unwrap! (map-get? platforms tx-sender) err-not-found))
    )
    (ok (map-set platforms tx-sender
      (merge platform-info { active: false })
    ))
  )
)

;; read only functions
;;

;; Get moderation request details
(define-read-only (get-moderation-request (request-id uint))
  (ok (map-get? moderation-requests request-id))
)

;; Get moderator reputation
(define-read-only (get-moderator-reputation (moderator principal))
  (ok (map-get? moderator-reputation moderator))
)

;; Get platform info
(define-read-only (get-platform-info (platform principal))
  (ok (map-get? platforms platform))
)

;; Get moderator vote
(define-read-only (get-moderator-vote (request-id uint) (moderator principal))
  (ok (map-get? moderator-votes { request-id: request-id, moderator: moderator }))
)

;; Get appeal details
(define-read-only (get-appeal (request-id uint))
  (ok (map-get? appeals request-id))
)

;; Get regional expertise
(define-read-only (get-regional-expertise (moderator principal) (region (string-ascii 10)))
  (ok (map-get? regional-expertise { moderator: moderator, region: region }))
)

;; Calculate moderator accuracy rate
(define-read-only (get-moderator-accuracy (moderator principal))
  (let
    (
      (rep (map-get? moderator-reputation moderator))
    )
    (match rep
      reputation
        (if (> (get total-decisions reputation) u0)
          (ok (/ (* (get correct-decisions reputation) u100) (get total-decisions reputation)))
          (ok u0)
        )
      (ok u0)
    )
  )
)

;; Get next request ID
(define-read-only (get-next-request-id)
  (ok (var-get next-request-id))
)

;; private functions
;;

;; Helper to calculate weighted stake
(define-private (calculate-weighted-stake (stake uint) (weight uint))
  (/ (* stake weight) u100)
)