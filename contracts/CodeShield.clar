
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