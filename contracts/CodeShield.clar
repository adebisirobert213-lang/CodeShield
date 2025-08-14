
;; title: CodeShield
;; version: 1.0.0
;; summary: Decentralized bug bounty platform with automated payouts
;; description: Smart contract system for posting bounties, linking GitHub issues, 
;;              automatic payouts on PR merge, and dispute resolution

;; traits
(define-trait sip-010-trait
  (
    (transfer (uint principal principal (optional (buff 34))) (response bool uint))
    (get-name () (response (string-ascii 32) uint))
    (get-symbol () (response (string-ascii 32) uint))
    (get-decimals () (response uint uint))
    (get-balance (principal) (response uint uint))
    (get-total-supply () (response uint uint))
    (get-token-uri (uint) (response (optional (string-utf8 256)) uint))
  )
)

;; token definitions
(define-fungible-token codeshield-token)

;; constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-bounty-closed (err u104))
(define-constant err-bounty-expired (err u105))
(define-constant err-already-claimed (err u106))
(define-constant err-insufficient-balance (err u107))
(define-constant err-dispute-period-active (err u108))
(define-constant err-no-dispute (err u109))
(define-constant err-invalid-tier (err u110))

(define-constant bounty-status-open u1)
(define-constant bounty-status-claimed u2)
(define-constant bounty-status-completed u3)
(define-constant bounty-status-disputed u4)
(define-constant bounty-status-cancelled u5)

(define-constant tier-low u1)
(define-constant tier-medium u2)
(define-constant tier-high u3)
(define-constant tier-critical u4)

(define-constant dispute-period u144) ;; ~24 hours in blocks
(define-constant max-bounty-duration u1008) ;; ~7 days in blocks

;; data vars
(define-data-var next-bounty-id uint u1)
(define-data-var platform-fee-rate uint u250) ;; 2.5% in basis points
(define-data-var total-bounties uint u0)
(define-data-var total-volume uint u0)

;; data maps
(define-map bounties
  { bounty-id: uint }
  {
    creator: principal,
    amount: uint,
    tier: uint,
    github-issue-url: (string-ascii 256),
    github-repo: (string-ascii 128),
    title: (string-ascii 128),
    description: (string-ascii 512),
    status: uint,
    hunter: (optional principal),
    created-at: uint,
    expires-at: uint,
    claimed-at: (optional uint),
    completed-at: (optional uint),
    pr-url: (optional (string-ascii 256))
  }
)

(define-map user-stats
  { user: principal }
  {
    bounties-created: uint,
    bounties-completed: uint,
    total-earned: uint,
    reputation-score: uint
  }
)

(define-map disputes
  { bounty-id: uint }
  {
    disputer: principal,
    reason: (string-ascii 256),
    created-at: uint,
    resolved: bool,
    resolution: (optional (string-ascii 256))
  }
)

(define-map tier-multipliers
  { tier: uint }
  { multiplier: uint }
)

;; Initialize tier multipliers
(map-set tier-multipliers { tier: tier-low } { multiplier: u100 })
(map-set tier-multipliers { tier: tier-medium } { multiplier: u150 })
(map-set tier-multipliers { tier: tier-high } { multiplier: u200 })
(map-set tier-multipliers { tier: tier-critical } { multiplier: u300 })


;; public functions

;; Create a new bounty with STX escrow
(define-public (create-bounty 
  (amount uint)
  (tier uint)
  (github-issue-url (string-ascii 256))
  (github-repo (string-ascii 128))
  (title (string-ascii 128))
  (description (string-ascii 512))
  (duration-blocks uint))
  (let 
    (
      (bounty-id (var-get next-bounty-id))
      (expires-at (+ block-height duration-blocks))
    )
    ;; Input validation
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (and (>= tier tier-low) (<= tier tier-critical)) err-invalid-tier)
    (asserts! (<= duration-blocks max-bounty-duration) err-invalid-amount)
    (asserts! (validate-string github-issue-url) err-invalid-amount)
    (asserts! (validate-string github-repo) err-invalid-amount)
    (asserts! (validate-string title) err-invalid-amount)
    (asserts! (validate-string description) err-invalid-amount)
    
    ;; Transfer STX to contract as escrow
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    ;; Create bounty record
    (map-set bounties
      { bounty-id: bounty-id }
      {
        creator: tx-sender,
        amount: amount,
        tier: tier,
        github-issue-url: github-issue-url,
        github-repo: github-repo,
        title: title,
        description: description,
        status: bounty-status-open,
        hunter: none,
        created-at: block-height,
        expires-at: expires-at,
        claimed-at: none,
        completed-at: none,
        pr-url: none
      }
    )
    
    ;; Update user stats
    (update-user-bounties-created tx-sender)
    
    ;; Update global stats
    (var-set next-bounty-id (+ bounty-id u1))
    (var-set total-bounties (+ (var-get total-bounties) u1))
    (var-set total-volume (+ (var-get total-volume) amount))
    
    (ok bounty-id)
  )
)

;; Claim a bounty (hunter expresses intent to work on it)
(define-public (claim-bounty (bounty-id uint))
  (let 
    (
      (bounty (unwrap! (map-get? bounties { bounty-id: bounty-id }) err-not-found))
    )
    ;; Validate bounty exists and is claimable
    (asserts! (validate-bounty-id bounty-id) err-not-found)
    (asserts! (is-eq (get status bounty) bounty-status-open) err-bounty-closed)
    (asserts! (< block-height (get expires-at bounty)) err-bounty-expired)
    (asserts! (is-none (get hunter bounty)) err-already-claimed)
    
    ;; Update bounty with hunter
    (map-set bounties
      { bounty-id: bounty-id }
      (merge bounty {
        status: bounty-status-claimed,
        hunter: (some tx-sender),
        claimed-at: (some block-height)
      })
    )
    
    (ok true)
  )
)

;; Complete bounty with PR URL (automatic payout simulation)
(define-public (complete-bounty (bounty-id uint) (pr-url (string-ascii 256)))
  (let 
    (
      (bounty (unwrap! (map-get? bounties { bounty-id: bounty-id }) err-not-found))
      (hunter (unwrap! (get hunter bounty) err-unauthorized))
      (fee-amount (calculate-platform-fee (get amount bounty)))
      (payout-amount (- (get amount bounty) fee-amount))
      (tier-mult (unwrap! (get multiplier (map-get? tier-multipliers { tier: (get tier bounty) })) err-invalid-tier))
      (reputation-bonus (/ (* payout-amount tier-mult) u10000))
    )
    ;; Validate inputs and conditions
    (asserts! (validate-bounty-id bounty-id) err-not-found)
    (asserts! (validate-string pr-url) err-invalid-amount)
    (asserts! (is-eq tx-sender hunter) err-unauthorized)
    (asserts! (is-eq (get status bounty) bounty-status-claimed) err-bounty-closed)
    (asserts! (< block-height (get expires-at bounty)) err-bounty-expired)
    
    ;; Update bounty status
    (map-set bounties
      { bounty-id: bounty-id }
      (merge bounty {
        status: bounty-status-completed,
        completed-at: (some block-height),
        pr-url: (some pr-url)
      })
    )
    
    ;; Transfer payout to hunter
    (try! (as-contract (stx-transfer? payout-amount tx-sender hunter)))
    
    ;; Transfer fee to contract owner
    (try! (as-contract (stx-transfer? fee-amount tx-sender contract-owner)))
    
    ;; Update hunter stats
    (update-user-completion-stats hunter payout-amount reputation-bonus)
    
    (ok payout-amount)
  )
)

;; Initiate dispute
(define-public (create-dispute (bounty-id uint) (reason (string-ascii 256)))
  (let 
    (
      (bounty (unwrap! (map-get? bounties { bounty-id: bounty-id }) err-not-found))
    )
    ;; Validate inputs and conditions
    (asserts! (validate-bounty-id bounty-id) err-not-found)
    (asserts! (validate-string reason) err-invalid-amount)
    (asserts! (is-eq (get status bounty) bounty-status-completed) err-bounty-closed)
    (asserts! (is-eq tx-sender (get creator bounty)) err-unauthorized)
    (asserts! (< (- block-height (unwrap! (get completed-at bounty) err-not-found)) dispute-period) err-dispute-period-active)
    
    ;; Create dispute record
    (map-set disputes
      { bounty-id: bounty-id }
      {
        disputer: tx-sender,
        reason: reason,
        created-at: block-height,
        resolved: false,
        resolution: none
      }
    )
    
    ;; Update bounty status
    (map-set bounties
      { bounty-id: bounty-id }
      (merge bounty { status: bounty-status-disputed })
    )
    
    (ok true)
  )
)

;; Resolve dispute (owner only)
(define-public (resolve-dispute 
  (bounty-id uint) 
  (resolution (string-ascii 256))
  (refund-creator bool))
  (let 
    (
      (bounty (unwrap! (map-get? bounties { bounty-id: bounty-id }) err-not-found))
      (dispute (unwrap! (map-get? disputes { bounty-id: bounty-id }) err-no-dispute))
      (creator (get creator bounty))
      (hunter (unwrap! (get hunter bounty) err-not-found))
    )
    ;; Validate inputs and conditions
    (asserts! (validate-bounty-id bounty-id) err-not-found)
    (asserts! (validate-string resolution) err-invalid-amount)
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (is-eq (get status bounty) bounty-status-disputed) err-bounty-closed)
    (asserts! (not (get resolved dispute)) err-no-dispute)
    
    ;; Update dispute as resolved
    (map-set disputes
      { bounty-id: bounty-id }
      (merge dispute {
        resolved: true,
        resolution: (some resolution)
      })
    )
    
    ;; Handle refund if necessary
    (if refund-creator
      (begin
        ;; Refund creator (amount was already paid to hunter, so owner covers it)
        (try! (stx-transfer? (get amount bounty) contract-owner creator))
        ;; Update bounty status to cancelled
        (map-set bounties
          { bounty-id: bounty-id }
          (merge bounty { status: bounty-status-cancelled })
        )
      )
      ;; Update bounty status back to completed
      (map-set bounties
        { bounty-id: bounty-id }
        (merge bounty { status: bounty-status-completed })
      )
    )
    
    (ok true)
  )
)

;; Cancel bounty (creator only, if not claimed)
(define-public (cancel-bounty (bounty-id uint))
  (let 
    (
      (bounty (unwrap! (map-get? bounties { bounty-id: bounty-id }) err-not-found))
    )
    ;; Validate inputs and conditions
    (asserts! (validate-bounty-id bounty-id) err-not-found)
    (asserts! (is-eq tx-sender (get creator bounty)) err-unauthorized)
    (asserts! (is-eq (get status bounty) bounty-status-open) err-bounty-closed)
    
    ;; Refund escrow to creator
    (try! (as-contract (stx-transfer? (get amount bounty) tx-sender (get creator bounty))))
    
    ;; Update bounty status
    (map-set bounties
      { bounty-id: bounty-id }
      (merge bounty { status: bounty-status-cancelled })
    )
    
    (ok true)
  )
)

;; Set platform fee (owner only)
(define-public (set-platform-fee (new-fee-rate uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-fee-rate u1000) err-invalid-amount) ;; Max 10%
    (var-set platform-fee-rate new-fee-rate)
    (ok true)
  )
)

;; read only functions

;; Get bounty details
(define-read-only (get-bounty (bounty-id uint))
  ;; Safe to return none if bounty doesn't exist
  (map-get? bounties { bounty-id: bounty-id })
)

;; Get user stats
(define-read-only (get-user-stats (user principal))
  (default-to 
    { bounties-created: u0, bounties-completed: u0, total-earned: u0, reputation-score: u0 }
    (map-get? user-stats { user: user })
  )
)

;; Get dispute details
(define-read-only (get-dispute (bounty-id uint))
  ;; Safe to return none if dispute doesn't exist
  (map-get? disputes { bounty-id: bounty-id })
)

;; Get platform stats
(define-read-only (get-platform-stats)
  {
    total-bounties: (var-get total-bounties),
    total-volume: (var-get total-volume),
    platform-fee-rate: (var-get platform-fee-rate),
    next-bounty-id: (var-get next-bounty-id)
  }
)

;; Check if bounty is claimable
(define-read-only (is-bounty-claimable (bounty-id uint))
  (match (map-get? bounties { bounty-id: bounty-id })
    bounty (and 
      (is-eq (get status bounty) bounty-status-open)
      (< block-height (get expires-at bounty))
      (is-none (get hunter bounty))
    )
    false
  )
)

;; Get active bounties count
(define-read-only (get-active-bounties-count)
  (len (filter is-bounty-active (list 
    u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 ;; This would need to be dynamic in production
  )))
)

;; Calculate platform fee
(define-read-only (calculate-platform-fee (amount uint))
  (/ (* amount (var-get platform-fee-rate)) u10000)
)

;; private functions

;; Validate string input to prevent common attacks
(define-private (validate-string (input (string-ascii 512)))
  (and 
    (not (is-eq input "")) ;; Not empty
    (not (is-eq input "null")) ;; Not null string
    (not (is-eq input "undefined")) ;; Not undefined string
    true
  )
)

;; Validate bounty ID exists
(define-private (validate-bounty-id (bounty-id uint))
  (is-some (map-get? bounties { bounty-id: bounty-id }))
)

;; Update user bounties created count
(define-private (update-user-bounties-created (user principal))
  (let 
    (
      (current-stats (get-user-stats user))
    )
    (map-set user-stats
      { user: user }
      (merge current-stats {
        bounties-created: (+ (get bounties-created current-stats) u1)
      })
    )
  )
)

;; Update user completion stats
(define-private (update-user-completion-stats (user principal) (earned uint) (reputation-bonus uint))
  (let 
    (
      (current-stats (get-user-stats user))
    )
    (map-set user-stats
      { user: user }
      (merge current-stats {
        bounties-completed: (+ (get bounties-completed current-stats) u1),
        total-earned: (+ (get total-earned current-stats) earned),
        reputation-score: (+ (get reputation-score current-stats) reputation-bonus)
      })
    )
  )
)

;; Helper function for filtering active bounties
(define-private (is-bounty-active (bounty-id uint))
  (match (map-get? bounties { bounty-id: bounty-id })
    bounty (or 
      (is-eq (get status bounty) bounty-status-open)
      (is-eq (get status bounty) bounty-status-claimed)
    )
    false
  )
)
