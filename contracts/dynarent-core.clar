;; dynarent-core
;; This contract manages the entire rental agreement lifecycle for the DynaRent protocol
;; Enables peer-to-peer rental agreements with automated payment processing and term enforcement
;; Handles listing creation, rental initiation, payments, extensions, terminations and disputes

;; ========================================
;; Error codes
;; ========================================
(define-constant ERR-NOT-AUTHORIZED (err u1001))
(define-constant ERR-LISTING-NOT-FOUND (err u1002))
(define-constant ERR-LISTING-UNAVAILABLE (err u1003))
(define-constant ERR-INVALID-RENTAL-TERMS (err u1004))
(define-constant ERR-INSUFFICIENT-FUNDS (err u1005))
(define-constant ERR-RENTAL-NOT-FOUND (err u1006))
(define-constant ERR-RENTAL-NOT-ACTIVE (err u1007))
(define-constant ERR-INVALID-DURATION (err u1008))
(define-constant ERR-ALREADY-RENTED (err u1009))
(define-constant ERR-DISPUTE-EXISTS (err u1010))
(define-constant ERR-UNAUTHORIZED-TERMINATION (err u1011))
(define-constant ERR-INVALID-EXTENSION (err u1012))
(define-constant ERR-INVALID-PAYMENT (err u1013))
(define-constant ERR-DISPUTE-NOT-FOUND (err u1014))
(define-constant ERR-INVALID-RATING (err u1015))
(define-constant ERR-ALREADY-RATED (err u1016))
(define-constant ERR-INVALID-STATUS (err u1017))

;; ========================================
;; Data definitions
;; ========================================

;; Status enums
(define-constant STATUS-AVAILABLE u1)
(define-constant STATUS-RENTED u2)
(define-constant STATUS-MAINTENANCE u3)
(define-constant STATUS-INACTIVE u4)

(define-constant RENTAL-STATUS-ACTIVE u1)
(define-constant RENTAL-STATUS-COMPLETED u2)
(define-constant RENTAL-STATUS-TERMINATED u3)
(define-constant RENTAL-STATUS-DISPUTED u4)

(define-constant PAYMENT-SCHEDULE-HOURLY u1)
(define-constant PAYMENT-SCHEDULE-DAILY u2)
(define-constant PAYMENT-SCHEDULE-WEEKLY u3)

;; Tracks all rental listings in the system
(define-map rental-listings
  { listing-id: uint }
  {
    owner: principal,
    name: (string-ascii 100),
    description: (string-utf8 500),
    image-url: (optional (string-utf8 200)),
    rate-amount: uint,
    rate-schedule: uint,
    deposit-amount: uint,
    insurance-amount: (optional uint),
    min-duration: uint,
    max-duration: uint,
    terms: (string-utf8 1000),
    status: uint,
    created-at: uint
  }
)

;; Tracks all active and historical rental agreements
(define-map rental-agreements
  { rental-id: uint }
  {
    listing-id: uint,
    owner: principal,
    renter: principal,
    start-time: uint,
    end-time: uint,
    total-amount: uint,
    deposit-amount: uint,
    insurance-amount: (optional uint),
    status: uint,
    last-payment-time: uint,
    next-payment-due: uint,
    payments-made: uint,
    is-disputed: bool,
    dispute-reason: (optional (string-utf8 500)),
    created-at: uint,
    updated-at: uint
  }
)

;; Tracks all payments related to rental agreements
(define-map rental-payments
  { payment-id: uint }
  {
    rental-id: uint,
    amount: uint,
    payer: principal,
    recipient: principal,
    payment-type: (string-ascii 20), ;; "rent", "deposit", "refund", "penalty"
    timestamp: uint
  }
)

;; Tracks reputation data for users
(define-map user-reputation
  { user: principal }
  {
    total-rentals: uint,
    total-listings: uint,
    completed-rentals: uint,
    terminated-rentals: uint,
    disputes-opened: uint,
    disputes-lost: uint,
    avg-rating: uint,
    rating-count: uint
  }
)

;; Tracks ratings for completed rentals
(define-map rental-ratings
  { rental-id: uint, rater: principal }
  {
    rating: uint, ;; 1-5 rating scale
    review: (optional (string-utf8 500)),
    timestamp: uint
  }
)

;; Contract data variables
(define-data-var next-listing-id uint u1)
(define-data-var next-rental-id uint u1)
(define-data-var next-payment-id uint u1)
(define-data-var contract-admin principal tx-sender)
(define-data-var protocol-fee-percent uint u2) ;; 2% default fee

;; ========================================
;; Private functions
;; ========================================

;; Helper to get the current time (block height as a proxy for time)
(define-private (get-current-time)
  block-height
)

;; Helper to calculate rental cost based on duration and rate
(define-private (calculate-rental-cost (rate uint) (schedule uint) (duration uint))
  (let ((multiplier (cond
                      ((is-eq schedule PAYMENT-SCHEDULE-HOURLY) u1)
                      ((is-eq schedule PAYMENT-SCHEDULE-DAILY) u24)
                      ((is-eq schedule PAYMENT-SCHEDULE-WEEKLY) u168)
                      (true u1))))
    (* rate (/ duration multiplier))
  )
)

;; Helper to record a payment
(define-private (record-payment (rental-id uint) (amount uint) (payer principal) (recipient principal) (payment-type (string-ascii 20)))
  (let ((payment-id (var-get next-payment-id)))
    (map-set rental-payments 
      { payment-id: payment-id }
      {
        rental-id: rental-id,
        amount: amount,
        payer: payer,
        recipient: recipient,
        payment-type: payment-type,
        timestamp: (get-current-time)
      }
    )
    (var-set next-payment-id (+ payment-id u1))
    (ok payment-id)
  )
)

;; Helper to update user reputation when a rental is completed
(define-private (update-reputation (user principal) (is-owner bool) (completed bool))
  (let ((current-rep (default-to 
          {
            total-rentals: u0,
            total-listings: u0,
            completed-rentals: u0,
            terminated-rentals: u0,
            disputes-opened: u0,
            disputes-lost: u0,
            avg-rating: u0,
            rating-count: u0
          }
          (map-get? user-reputation { user: user }))))
    (if is-owner
      ;; Update owner's reputation
      (map-set user-reputation
        { user: user }
        (merge current-rep {
          total-listings: (+ (get total-listings current-rep) u1)
        })
      )
      ;; Update renter's reputation
      (map-set user-reputation
        { user: user }
        (merge current-rep {
          total-rentals: (+ (get total-rentals current-rep) u1),
          completed-rentals: (if completed 
                              (+ (get completed-rentals current-rep) u1)
                              (get completed-rentals current-rep)),
          terminated-rentals: (if (not completed)
                              (+ (get terminated-rentals current-rep) u1)
                              (get terminated-rentals current-rep))
        })
      )
    )
    (ok true)
  )
)

;; Calculates refund amount based on rental status and time remaining
(define-private (calculate-refund (rental-id uint))
  (let (
    (rental (unwrap! (map-get? rental-agreements { rental-id: rental-id }) ERR-RENTAL-NOT-FOUND))
    (current-time (get-current-time))
    (time-remaining (if (> (get end-time rental) current-time)
                      (- (get end-time rental) current-time)
                      u0))
    (total-duration (- (get end-time rental) (get start-time rental)))
    (portion-remaining (if (> total-duration u0)
                        (/ (* time-remaining u100) total-duration)
                        u0))
    ;; Refund 80% of the remaining time's value
    (refund-percentage u80)
    (refund-amount (/ (* (get total-amount rental) portion-remaining refund-percentage) u10000))
  )
    refund-amount
  )
)

;; ========================================
;; Read-only functions
;; ========================================

;; Gets a specific rental listing by ID
(define-read-only (get-listing (listing-id uint))
  (map-get? rental-listings { listing-id: listing-id })
)

;; Gets a specific rental agreement by ID
(define-read-only (get-rental-agreement (rental-id uint))
  (map-get? rental-agreements { rental-id: rental-id })
)

;; Gets user reputation data
(define-read-only (get-user-reputation (user principal))
  (default-to 
    {
      total-rentals: u0,
      total-listings: u0,
      completed-rentals: u0,
      terminated-rentals: u0,
      disputes-opened: u0,
      disputes-lost: u0,
      avg-rating: u0,
      rating-count: u0
    }
    (map-get? user-reputation { user: user })
  )
)

;; Gets rating for a specific rental
(define-read-only (get-rental-rating (rental-id uint) (rater principal))
  (map-get? rental-ratings { rental-id: rental-id, rater: rater })
)

;; Gets payment record by ID
(define-read-only (get-payment (payment-id uint))
  (map-get? rental-payments { payment-id: payment-id })
)

;; ========================================
;; Public functions
;; ========================================

;; Create a new rental listing
(define-public (create-listing 
    (name (string-ascii 100))
    (description (string-utf8 500))
    (image-url (optional (string-utf8 200)))
    (rate-amount uint)
    (rate-schedule uint)
    (deposit-amount uint)
    (insurance-amount (optional uint))
    (min-duration uint)
    (max-duration uint)
    (terms (string-utf8 1000))
  )
  (let ((listing-id (var-get next-listing-id)))
    ;; Validate inputs
    (asserts! (and (> rate-amount u0) (> deposit-amount u0)) ERR-INVALID-RENTAL-TERMS)
    (asserts! (and (> min-duration u0) (>= max-duration min-duration)) ERR-INVALID-DURATION)
    (asserts! (or 
                (is-eq rate-schedule PAYMENT-SCHEDULE-HOURLY)
                (is-eq rate-schedule PAYMENT-SCHEDULE-DAILY)
                (is-eq rate-schedule PAYMENT-SCHEDULE-WEEKLY)) 
              ERR-INVALID-RENTAL-TERMS)
    
    ;; Create the new listing
    (map-set rental-listings
      { listing-id: listing-id }
      {
        owner: tx-sender,
        name: name,
        description: description,
        image-url: image-url,
        rate-amount: rate-amount,
        rate-schedule: rate-schedule,
        deposit-amount: deposit-amount,
        insurance-amount: insurance-amount,
        min-duration: min-duration,
        max-duration: max-duration,
        terms: terms,
        status: STATUS-AVAILABLE,
        created-at: (get-current-time)
      }
    )
    
    ;; Update owner reputation
    (try! (update-reputation tx-sender true false))
    
    ;; Increment listing ID counter
    (var-set next-listing-id (+ listing-id u1))
    
    (ok listing-id)
  )
)

;; Update a listing's details
(define-public (update-listing
    (listing-id uint)
    (name (optional (string-ascii 100)))
    (description (optional (string-utf8 500)))
    (image-url (optional (string-utf8 200)))
    (rate-amount (optional uint))
    (rate-schedule (optional uint))
    (deposit-amount (optional uint))
    (insurance-amount (optional uint))
    (min-duration (optional uint))
    (max-duration (optional uint))
    (terms (optional (string-utf8 1000)))
    (status (optional uint))
  )
  (let ((listing (unwrap! (map-get? rental-listings { listing-id: listing-id }) ERR-LISTING-NOT-FOUND)))
    ;; Check authorization
    (asserts! (is-eq tx-sender (get owner listing)) ERR-NOT-AUTHORIZED)
    
    ;; Validate inputs if provided
    (asserts! (or (is-none rate-amount) (> (default-to u1 rate-amount) u0)) ERR-INVALID-RENTAL-TERMS)
    (asserts! (or (is-none deposit-amount) (> (default-to u1 deposit-amount) u0)) ERR-INVALID-RENTAL-TERMS)
    (asserts! (or (is-none min-duration) (> (default-to u1 min-duration) u0)) ERR-INVALID-DURATION)
    (asserts! (or 
                (is-none rate-schedule) 
                (is-eq (default-to u1 rate-schedule) PAYMENT-SCHEDULE-HOURLY)
                (is-eq (default-to u1 rate-schedule) PAYMENT-SCHEDULE-DAILY)
                (is-eq (default-to u1 rate-schedule) PAYMENT-SCHEDULE-WEEKLY)) 
              ERR-INVALID-RENTAL-TERMS)
    
    ;; Ensure that if both min and max duration are provided, max >= min
    (asserts! (or 
                (is-none min-duration) 
                (is-none max-duration) 
                (>= (default-to u1 max-duration) (default-to u1 min-duration))) 
              ERR-INVALID-DURATION)
    
    ;; Cannot change status from RENTED to anything else through this function
    (asserts! (or 
                (is-none status)
                (not (is-eq (get status listing) STATUS-RENTED))
                (is-eq (default-to u1 status) STATUS-RENTED)) 
              ERR-ALREADY-RENTED)
    
    ;; Update the listing with new values, keeping old values when not provided
    (map-set rental-listings
      { listing-id: listing-id }
      (merge listing {
        name: (default-to (get name listing) name),
        description: (default-to (get description listing) description),
        image-url: (if (is-some image-url) image-url (get image-url listing)),
        rate-amount: (default-to (get rate-amount listing) rate-amount),
        rate-schedule: (default-to (get rate-schedule listing) rate-schedule),
        deposit-amount: (default-to (get deposit-amount listing) deposit-amount),
        insurance-amount: (if (is-some insurance-amount) insurance-amount (get insurance-amount listing)),
        min-duration: (default-to (get min-duration listing) min-duration),
        max-duration: (default-to (get max-duration listing) max-duration),
        terms: (default-to (get terms listing) terms),
        status: (default-to (get status listing) status)
      })
    )
    
    (ok true)
  )
)

;; Initiate a rental agreement as a renter
(define-public (rent-asset 
    (listing-id uint)
    (duration uint)
  )
  (let (
    (listing (unwrap! (map-get? rental-listings { listing-id: listing-id }) ERR-LISTING-NOT-FOUND))
    (rental-id (var-get next-rental-id))
    (current-time (get-current-time))
    (end-time (+ current-time duration))
    (total-cost (calculate-rental-cost 
                  (get rate-amount listing) 
                  (get rate-schedule listing) 
                  duration))
    (deposit (get deposit-amount listing))
    (total-payment (+ total-cost deposit))
  )
    ;; Validate the rental terms
    (asserts! (is-eq (get status listing) STATUS-AVAILABLE) ERR-LISTING-UNAVAILABLE)
    (asserts! (not (is-eq tx-sender (get owner listing))) ERR-NOT-AUTHORIZED)
    (asserts! (and (>= duration (get min-duration listing))
                  (<= duration (get max-duration listing))) ERR-INVALID-DURATION)
                  
    ;; Check that renter has sufficient funds 
    (asserts! (>= (stx-get-balance tx-sender) total-payment) ERR-INSUFFICIENT-FUNDS)
    
    ;; Process payment - first is combined payment of deposit + first payment
    (try! (stx-transfer? total-payment tx-sender (get owner listing)))
    (try! (record-payment rental-id total-payment tx-sender (get owner listing) "initial"))
    
    ;; Update listing status to rented
    (try! (update-listing listing-id 
                         none 
                         none 
                         none 
                         none 
                         none 
                         none 
                         none 
                         none 
                         none 
                         none 
                         (some STATUS-RENTED)))
    
    ;; Create rental agreement
    (map-set rental-agreements
      { rental-id: rental-id }
      {
        listing-id: listing-id,
        owner: (get owner listing),
        renter: tx-sender,
        start-time: current-time,
        end-time: end-time,
        total-amount: total-cost,
        deposit-amount: deposit,
        insurance-amount: (get insurance-amount listing),
        status: RENTAL-STATUS-ACTIVE,
        last-payment-time: current-time,
        next-payment-due: current-time, ;; Initial payment made at start
        payments-made: u1,
        is-disputed: false,
        dispute-reason: none,
        created-at: current-time,
        updated-at: current-time
      }
    )
    
    ;; Update renter reputation
    (try! (update-reputation tx-sender false false))
    
    ;; Increment rental ID counter
    (var-set next-rental-id (+ rental-id u1))
    
    (ok rental-id)
  )
)

;; Complete a rental - can be called by either party after end time
(define-public (complete-rental (rental-id uint))
  (let (
    (rental (unwrap! (map-get? rental-agreements { rental-id: rental-id }) ERR-RENTAL-NOT-FOUND))
    (current-time (get-current-time))
    (listing-id (get listing-id rental))
    (listing (unwrap! (map-get? rental-listings { listing-id: listing-id }) ERR-LISTING-NOT-FOUND))
  )
    ;; Ensure caller is either the renter or owner
    (asserts! (or 
                (is-eq tx-sender (get owner rental))
                (is-eq tx-sender (get renter rental))) 
              ERR-NOT-AUTHORIZED)
    
    ;; Ensure rental is active and either past end time or both parties agree to completion
    (asserts! (is-eq (get status rental) RENTAL-STATUS-ACTIVE) ERR-RENTAL-NOT-ACTIVE)
    (asserts! (or 
                (>= current-time (get end-time rental))
                (and 
                  (is-eq tx-sender (get owner rental)) 
                  (is-some (map-get? rental-ratings { rental-id: rental-id, rater: (get renter rental) })))
                (and
                  (is-eq tx-sender (get renter rental))
                  (is-some (map-get? rental-ratings { rental-id: rental-id, rater: (get owner rental) }))))
              ERR-UNAUTHORIZED-TERMINATION)
    
    ;; Update rental status to completed
    (map-set rental-agreements
      { rental-id: rental-id }
      (merge rental {
        status: RENTAL-STATUS-COMPLETED,
        updated-at: current-time
      })
    )
    
    ;; Update listing status back to available
    (try! (update-listing listing-id 
                         none 
                         none 
                         none 
                         none 
                         none 
                         none 
                         none 
                         none 
                         none 
                         none 
                         (some STATUS-AVAILABLE)))
    
    ;; Refund the deposit to the renter if no dispute exists
    (if (not (get is-disputed rental))
      (begin
        (try! (stx-transfer? (get deposit-amount rental) (get owner rental) (get renter rental)))
        (try! (record-payment rental-id (get deposit-amount rental) (get owner rental) (get renter rental) "deposit-refund"))
        (ok true)
      )
      (ok false)
    )
  )
)

;; Terminate a rental early
(define-public (terminate-rental (rental-id uint) (reason (string-utf8 500)))
  (let (
    (rental (unwrap! (map-get? rental-agreements { rental-id: rental-id }) ERR-RENTAL-NOT-FOUND))
    (current-time (get-current-time))
    (listing-id (get listing-id rental))
    (listing (unwrap! (map-get? rental-listings { listing-id: listing-id }) ERR-LISTING-NOT-FOUND))
    (refund-amount (calculate-refund rental-id))
  )
    ;; Ensure caller is either the renter or owner
    (asserts! (or 
                (is-eq tx-sender (get owner rental))
                (is-eq tx-sender (get renter rental))) 
              ERR-NOT-AUTHORIZED)
    
    ;; Ensure rental is active
    (asserts! (is-eq (get status rental) RENTAL-STATUS-ACTIVE) ERR-RENTAL-NOT-ACTIVE)
    
    ;; Update rental status to terminated
    (map-set rental-agreements
      { rental-id: rental-id }
      (merge rental {
        status: RENTAL-STATUS-TERMINATED,
        updated-at: current-time,
        end-time: current-time ;; End time is now the termination time
      })
    )
    
    ;; Update listing status back to available
    (try! (update-listing listing-id none none none none none none none none none none (some STATUS-AVAILABLE)))
    
    ;; Update reputation for both parties
    (try! (update-reputation (get owner rental) true false))
    (try! (update-reputation (get renter rental) false false))
    
    ;; If terminated by owner, process refund to renter
    (if (is-eq tx-sender (get owner rental))
      (begin
        ;; Refund remaining rent + deposit
        (let ((total-refund (+ refund-amount (get deposit-amount rental))))
          (try! (stx-transfer? total-refund tx-sender (get renter rental)))
          (try! (record-payment rental-id total-refund tx-sender (get renter rental) "termination-refund"))
        )
        (ok true)
      )
      ;; If terminated by renter, process partial refund based on time remaining
      (begin
        ;; Owner keeps a portion of the remaining rent but returns deposit
        (try! (stx-transfer? (get deposit-amount rental) (get owner rental) tx-sender))
        (try! (record-payment rental-id (get deposit-amount rental) (get owner rental) tx-sender "deposit-refund"))
        
        ;; Only refund remaining time if it's substantial
        (if (> refund-amount u0)
          (begin
            (try! (stx-transfer? refund-amount (get owner rental) tx-sender))
            (try! (record-payment rental-id refund-amount (get owner rental) tx-sender "partial-refund"))
            (ok true)
          )
          (ok true)
        )
      )
    )
  )
)

;; Extend a rental
(define-public (extend-rental (rental-id uint) (additional-time uint))
  (let (
    (rental (unwrap! (map-get? rental-agreements { rental-id: rental-id }) ERR-RENTAL-NOT-FOUND))
    (current-time (get-current-time))
    (listing-id (get listing-id rental))
    (listing (unwrap! (map-get? rental-listings { listing-id: listing-id }) ERR-LISTING-NOT-FOUND))
    (additional-cost (calculate-rental-cost 
                      (get rate-amount listing) 
                      (get rate-schedule listing) 
                      additional-time))
    (new-end-time (+ (get end-time rental) additional-time))
  )
    ;; Validate the extension
    (asserts! (is-eq tx-sender (get renter rental)) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status rental) RENTAL-STATUS-ACTIVE) ERR-RENTAL-NOT-ACTIVE)
    (asserts! (> additional-time u0) ERR-INVALID-EXTENSION)
    (asserts! (>= (stx-get-balance tx-sender) additional-cost) ERR-INSUFFICIENT-FUNDS)
    
    ;; Process payment for extension
    (try! (stx-transfer? additional-cost tx-sender (get owner rental)))
    (try! (record-payment rental-id additional-cost tx-sender (get owner rental) "extension"))
    
    ;; Update rental agreement with new end time and total amount
    (map-set rental-agreements
      { rental-id: rental-id }
      (merge rental {
        end-time: new-end-time,
        total-amount: (+ (get total-amount rental) additional-cost),
        updated-at: current-time
      })
    )
    
    (ok true)
  )
)

;; Open a dispute about a rental
(define-public (open-dispute (rental-id uint) (reason (string-utf8 500)))
  (let (
    (rental (unwrap! (map-get? rental-agreements { rental-id: rental-id }) ERR-RENTAL-NOT-FOUND))
    (current-time (get-current-time))
  )
    ;; Validate the dispute
    (asserts! (or 
                (is-eq tx-sender (get owner rental))
                (is-eq tx-sender (get renter rental))) 
              ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status rental) RENTAL-STATUS-ACTIVE) ERR-RENTAL-NOT-ACTIVE)
    (asserts! (not (get is-disputed rental)) ERR-DISPUTE-EXISTS)
    
    ;; Update rental agreement with dispute info
    (map-set rental-agreements
      { rental-id: rental-id }
      (merge rental {
        is-disputed: true,
        dispute-reason: (some reason),
        status: RENTAL-STATUS-DISPUTED,
        updated-at: current-time
      })
    )
    
    ;; Update disputer's reputation
    (let ((user-rep (get-user-reputation tx-sender)))
      (map-set user-reputation
        { user: tx-sender }
        (merge user-rep {
          disputes-opened: (+ (get disputes-opened user-rep) u1)
        })
      )
    )
    
    (ok true)
  )
)

;; Resolve a dispute - only contract admin can do this
(define-public (resolve-dispute 
    (rental-id uint) 
    (winner principal) 
    (deposit-to-renter uint)
    (refund-amount uint)
  )
  (let (
    (rental (unwrap! (map-get? rental-agreements { rental-id: rental-id }) ERR-RENTAL-NOT-FOUND))
    (current-time (get-current-time))
    (owner (get owner rental))
    (renter (get renter rental))
    (listing-id (get listing-id rental))
  )
    ;; Only contract admin can resolve disputes
    (asserts! (is-eq tx-sender (var-get contract-admin)) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status rental) RENTAL-STATUS-DISPUTED) ERR-INVALID-STATUS)
    (asserts! (get is-disputed rental) ERR-DISPUTE-NOT-FOUND)
    
    ;; Validate the winner is either owner or renter
    (asserts! (or (is-eq winner owner) (is-eq winner renter)) ERR-NOT-AUTHORIZED)
    
    ;; Ensure deposit refund is valid
    (asserts! (<= deposit-to-renter (get deposit-amount rental)) ERR-INVALID-PAYMENT)
    
    ;; Process refunds if any
    (if (> deposit-to-renter u0)
      (begin
        (try! (stx-transfer? deposit-to-renter owner renter))
        (try! (record-payment rental-id deposit-to-renter owner renter "dispute-refund"))
      )
      true
    )
    
    (if (> refund-amount u0)
      (begin
        (try! (stx-transfer? refund-amount owner renter))
        (try! (record-payment rental-id refund-amount owner renter "dispute-compensation"))
      )
      true
    )
    
    ;; Update rental status
    (map-set rental-agreements
      { rental-id: rental-id }
      (merge rental {
        status: RENTAL-STATUS-COMPLETED,
        is-disputed: false,
        updated-at: current-time
      })
    )
    
    ;; Update listing status back to available
    (try! (update-listing listing-id none none none none none none none none none none (some STATUS-AVAILABLE)))
    
    ;; Update loser's reputation
    (let (
      (loser (if (is-eq winner owner) renter owner))
      (loser-rep (get-user-reputation loser))
    )
      (map-set user-reputation
        { user: loser }
        (merge loser-rep {
          disputes-lost: (+ (get disputes-lost loser-rep) u1)
        })
      )
    )
    
    (ok true)
  )
)

;; Rate a completed rental
(define-public (rate-rental (rental-id uint) (rating uint) (review (optional (string-utf8 500))))
  (let (
    (rental (unwrap! (map-get? rental-agreements { rental