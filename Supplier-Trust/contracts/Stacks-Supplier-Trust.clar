;; Decentralized Supplier Performance Management System
;; A blockchain-based reputation scoring platform that enables transparent 
;; performance tracking for suppliers through multi-dimensional metrics.
;; Authorized reviewers submit evaluations across delivery, quality, and 
;; communication dimensions, with weighted scoring algorithms calculating 
;; overall supplier reputation scores in a trustless, immutable manner.

;; ERROR CONSTANTS
(define-constant ERR-UNAUTHORIZED-ACCESS (err u100))
(define-constant ERR-SUPPLIER-NOT-FOUND (err u101))
(define-constant ERR-SUPPLIER-ALREADY-REGISTERED (err u102))
(define-constant ERR-INVALID-SCORE-VALUE (err u103))
(define-constant ERR-REVIEWER-NOT-AUTHORIZED (err u104))
(define-constant ERR-INVALID-RANGE-PARAMETERS (err u105))
(define-constant ERR-REVIEW-NOT-FOUND (err u106))
(define-constant ERR-INVALID-BUSINESS-NAME (err u107))
(define-constant ERR-INVALID-PRINCIPAL (err u108))

;; CONFIGURATION CONSTANTS
(define-constant contract-administrator tx-sender)
(define-constant default-initial-reputation u50)
(define-constant delivery-performance-weight u40)
(define-constant product-quality-weight u40)
(define-constant communication-effectiveness-weight u20)
(define-constant percentage-divisor u100)

;; CONFIGURATION VARIABLES
(define-data-var minimum-score-threshold uint u0)
(define-data-var maximum-score-threshold uint u100)

;; DATA STRUCTURES
;; Comprehensive supplier profile storage
(define-map supplier-profiles
  principal
  {
    business-name: (string-ascii 50),
    overall-reputation-score: uint,
    total-reviews-received: uint,
    delivery-performance-average: uint,
    product-quality-average: uint,
    communication-effectiveness-average: uint,
    account-active-status: bool,
    registration-block-height: uint
  }
)

;; Individual review records with composite key
(define-map performance-evaluations
  { 
    evaluator-address: principal, 
    supplier-address: principal, 
    evaluation-sequence-number: uint 
  }
  {
    delivery-timeliness-rating: uint,
    product-quality-rating: uint,
    communication-quality-rating: uint,
    reviewer-comments: (string-utf8 500),
    submission-block-height: uint
  }
)

;; Track review count per supplier for unique IDs
(define-map supplier-evaluation-counter
  principal
  uint
)

;; Authorization registry for permitted reviewers
(define-map reviewer-authorization-registry
  principal
  bool
)

;; READ-ONLY QUERY FUNCTIONS
(define-read-only (get-supplier-profile (supplier-address principal))
  (ok (map-get? supplier-profiles supplier-address))
)

(define-read-only (get-performance-evaluation 
  (evaluator-address principal) 
  (supplier-address principal) 
  (evaluation-sequence-number uint))
  (ok (map-get? performance-evaluations { 
    evaluator-address: evaluator-address, 
    supplier-address: supplier-address, 
    evaluation-sequence-number: evaluation-sequence-number 
  }))
)

(define-read-only (get-overall-reputation (supplier-address principal))
  (match (map-get? supplier-profiles supplier-address)
    profile-data (ok (get overall-reputation-score profile-data))
    ERR-SUPPLIER-NOT-FOUND
  )
)

(define-read-only (get-detailed-metrics (supplier-address principal))
  (match (map-get? supplier-profiles supplier-address)
    profile-data (ok {
      delivery: (get delivery-performance-average profile-data),
      quality: (get product-quality-average profile-data),
      communication: (get communication-effectiveness-average profile-data),
      total-reviews: (get total-reviews-received profile-data)
    })
    ERR-SUPPLIER-NOT-FOUND
  )
)

(define-read-only (check-reviewer-authorization (reviewer-address principal))
  (ok (default-to false (map-get? reviewer-authorization-registry reviewer-address)))
)

(define-read-only (get-score-range-configuration)
  (ok { 
    minimum: (var-get minimum-score-threshold), 
    maximum: (var-get maximum-score-threshold) 
  })
)

(define-read-only (get-evaluation-count (supplier-address principal))
  (ok (default-to u0 (map-get? supplier-evaluation-counter supplier-address)))
)

(define-read-only (compute-weighted-reputation 
  (delivery-score uint) 
  (quality-score uint) 
  (communication-score uint))
  (let
    (
      (weighted-delivery-component (/ (* delivery-score delivery-performance-weight) percentage-divisor))
      (weighted-quality-component (/ (* quality-score product-quality-weight) percentage-divisor))
      (weighted-communication-component (/ (* communication-score communication-effectiveness-weight) percentage-divisor))
      (total-weighted-score (+ weighted-delivery-component (+ weighted-quality-component weighted-communication-component)))
    )
    (ok total-weighted-score)
  )
)

;; VALIDATION HELPER FUNCTIONS
(define-private (validate-score-within-range (score-value uint))
  (and 
    (>= score-value (var-get minimum-score-threshold))
    (<= score-value (var-get maximum-score-threshold))
  )
)

(define-private (verify-reviewer-authorized (reviewer-address principal))
  (default-to false (map-get? reviewer-authorization-registry reviewer-address))
)

(define-private (verify-administrator-access)
  (is-eq tx-sender contract-administrator)
)

(define-private (validate-business-name (name (string-ascii 50)))
  (> (len name) u0)
)

(define-private (validate-comments (comments (string-utf8 500)))
  ;; Comments validation - ensure they're not empty
  (> (len comments) u0)
)

(define-private (validate-principal-not-contract (address principal))
  ;; Additional validation - principals are always valid in Clarity,
  ;; but we can check they're not the zero address equivalent
  (not (is-eq address contract-administrator))
)

;; SCORE CALCULATION FUNCTIONS
(define-private (calculate-rolling-average 
  (current-average uint)
  (current-count uint)
  (new-value uint))
  (let
    (
      (total-accumulated-value (* current-average current-count))
      (updated-count (+ current-count u1))
      (new-total-value (+ total-accumulated-value new-value))
      (new-average-value (/ new-total-value updated-count))
    )
    new-average-value
  )
)

(define-private (update-all-supplier-metrics
  (supplier-address principal)
  (delivery-rating uint)
  (quality-rating uint)
  (communication-rating uint)
  (computed-weighted-score uint))
  (match (map-get? supplier-profiles supplier-address)
    existing-profile
      (let
        (
          (current-review-count (get total-reviews-received existing-profile))
          (updated-delivery-avg (calculate-rolling-average 
            (get delivery-performance-average existing-profile)
            current-review-count
            delivery-rating))
          (updated-quality-avg (calculate-rolling-average 
            (get product-quality-average existing-profile)
            current-review-count
            quality-rating))
          (updated-communication-avg (calculate-rolling-average 
            (get communication-effectiveness-average existing-profile)
            current-review-count
            communication-rating))
          (incremented-review-count (+ current-review-count u1))
        )
        (ok (map-set supplier-profiles supplier-address
          (merge existing-profile {
            overall-reputation-score: computed-weighted-score,
            total-reviews-received: incremented-review-count,
            delivery-performance-average: updated-delivery-avg,
            product-quality-average: updated-quality-avg,
            communication-effectiveness-average: updated-communication-avg
          })
        ))
      )
    ERR-SUPPLIER-NOT-FOUND
  )
)

;; SUPPLIER MANAGEMENT FUNCTIONS
(define-public (register-as-supplier (business-name (string-ascii 50)))
  (let
    (
      (supplier-address tx-sender)
      (existing-profile (map-get? supplier-profiles supplier-address))
      (validated-name business-name)
    )
    ;; Validate business name
    (asserts! (validate-business-name validated-name) ERR-INVALID-BUSINESS-NAME)
    (asserts! (is-none existing-profile) ERR-SUPPLIER-ALREADY-REGISTERED)
    (ok (map-set supplier-profiles supplier-address {
      business-name: validated-name,
      overall-reputation-score: default-initial-reputation,
      total-reviews-received: u0,
      delivery-performance-average: default-initial-reputation,
      product-quality-average: default-initial-reputation,
      communication-effectiveness-average: default-initial-reputation,
      account-active-status: true,
      registration-block-height: stacks-block-height
    }))
  )
)

(define-public (toggle-supplier-status (supplier-address principal) (active-status bool))
  (let
    (
      ;; Validate the supplier address first
      (validated-address supplier-address)
    )
    (asserts! (verify-administrator-access) ERR-UNAUTHORIZED-ACCESS)
    ;; Additional validation - ensure it's not the contract administrator
    (asserts! (not (is-eq validated-address contract-administrator)) ERR-INVALID-PRINCIPAL)
    
    (let
      (
        (supplier-data (unwrap! (map-get? supplier-profiles validated-address) ERR-SUPPLIER-NOT-FOUND))
      )
      ;; Update supplier status using the validated address
      (ok (map-set supplier-profiles validated-address 
        (merge supplier-data { account-active-status: active-status })))
    )
  )
)

;; REVIEWER AUTHORIZATION FUNCTIONS
(define-public (grant-reviewer-authorization (reviewer-address principal))
  (let
    (
      (validated-address reviewer-address)
    )
    (asserts! (verify-administrator-access) ERR-UNAUTHORIZED-ACCESS)
    ;; Validate the reviewer address
    (asserts! (not (is-eq validated-address contract-administrator)) ERR-INVALID-PRINCIPAL)
    (ok (map-set reviewer-authorization-registry validated-address true))
  )
)

(define-public (revoke-reviewer-authorization (reviewer-address principal))
  (let
    (
      (validated-address reviewer-address)
    )
    (asserts! (verify-administrator-access) ERR-UNAUTHORIZED-ACCESS)
    ;; Validate the reviewer address exists in registry
    (asserts! (is-some (map-get? reviewer-authorization-registry validated-address)) ERR-REVIEWER-NOT-AUTHORIZED)
    (ok (map-set reviewer-authorization-registry validated-address false))
  )
)

;; REVIEW SUBMISSION FUNCTIONS
(define-public (submit-supplier-evaluation
  (supplier-address principal)
  (delivery-timeliness-rating uint)
  (product-quality-rating uint)
  (communication-quality-rating uint)
  (reviewer-comments (string-utf8 500)))
  (let
    (
      (evaluator-address tx-sender)
      (validated-supplier-address supplier-address)
      (current-evaluation-count (default-to u0 (map-get? supplier-evaluation-counter validated-supplier-address)))
      (next-evaluation-number (+ current-evaluation-count u1))
    )
    ;; Validation checks
    (asserts! (verify-reviewer-authorized evaluator-address) ERR-REVIEWER-NOT-AUTHORIZED)
    (asserts! (is-some (map-get? supplier-profiles validated-supplier-address)) ERR-SUPPLIER-NOT-FOUND)
    (asserts! (validate-score-within-range delivery-timeliness-rating) ERR-INVALID-SCORE-VALUE)
    (asserts! (validate-score-within-range product-quality-rating) ERR-INVALID-SCORE-VALUE)
    (asserts! (validate-score-within-range communication-quality-rating) ERR-INVALID-SCORE-VALUE)
    (asserts! (validate-comments reviewer-comments) ERR-INVALID-BUSINESS-NAME)
    
    ;; Store the evaluation with validated comments
    (let
      (
        (evaluation-record {
          delivery-timeliness-rating: delivery-timeliness-rating,
          product-quality-rating: product-quality-rating,
          communication-quality-rating: communication-quality-rating,
          reviewer-comments: reviewer-comments,
          submission-block-height: stacks-block-height
        })
      )
      (map-set performance-evaluations 
        { 
          evaluator-address: evaluator-address, 
          supplier-address: validated-supplier-address, 
          evaluation-sequence-number: next-evaluation-number 
        }
        evaluation-record
      )
    )
    
    ;; Update evaluation counter
    (map-set supplier-evaluation-counter validated-supplier-address next-evaluation-number)
    
    ;; Calculate and update metrics
    (let
      (
        (computed-score (unwrap-panic (compute-weighted-reputation delivery-timeliness-rating product-quality-rating communication-quality-rating)))
      )
      (update-all-supplier-metrics 
        validated-supplier-address 
        delivery-timeliness-rating 
        product-quality-rating 
        communication-quality-rating 
        computed-score)
    )
  )
)

;; CONFIGURATION MANAGEMENT FUNCTIONS
(define-public (configure-score-range (minimum-value uint) (maximum-value uint))
  (begin
    (asserts! (verify-administrator-access) ERR-UNAUTHORIZED-ACCESS)
    (asserts! (< minimum-value maximum-value) ERR-INVALID-RANGE-PARAMETERS)
    (var-set minimum-score-threshold minimum-value)
    (var-set maximum-score-threshold maximum-value)
    (ok true)
  )
)