;; Event Ticketing Smart Contract
;; A comprehensive ticketing system with event management, sales, and validation

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-EVENT-NOT-FOUND (err u101))
(define-constant ERR-TICKET-NOT-FOUND (err u102))
(define-constant ERR-INSUFFICIENT-FUNDS (err u103))
(define-constant ERR-EVENT-SOLD-OUT (err u104))
(define-constant ERR-EVENT-ENDED (err u105))
(define-constant ERR-TICKET-ALREADY-USED (err u106))
(define-constant ERR-INVALID-PRICE (err u107))
(define-constant ERR-EVENT-NOT-ACTIVE (err u108))
(define-constant ERR-REFUND-NOT-ALLOWED (err u109))
;; Added new error constant for invalid input validation
(define-constant ERR-INVALID-INPUT (err u110))

;; Contract owner
(define-constant CONTRACT-OWNER tx-sender)

;; Data structures
(define-map events
  { event-id: uint }
  {
    organizer: principal,
    name: (string-ascii 100),
    description: (string-ascii 500),
    venue: (string-ascii 200),
    event-date: uint,
    ticket-price: uint,
    total-tickets: uint,
    sold-tickets: uint,
    is-active: bool,
    refund-enabled: bool
  }
)

(define-map tickets
  { ticket-id: uint }
  {
    event-id: uint,
    owner: principal,
    purchase-date: uint,
    is-used: bool,
    purchase-price: uint
  }
)

(define-map user-tickets
  { user: principal, event-id: uint }
  { ticket-count: uint }
)

;; Data variables
(define-data-var next-event-id uint u1)
(define-data-var next-ticket-id uint u1)
(define-data-var platform-fee-rate uint u250) ;; 2.5% in basis points

;; Read-only functions
(define-read-only (get-event (event-id uint))
  (map-get? events { event-id: event-id })
)

(define-read-only (get-ticket (ticket-id uint))
  (map-get? tickets { ticket-id: ticket-id })
)

(define-read-only (get-user-ticket-count (user principal) (event-id uint))
  (default-to u0 (get ticket-count (map-get? user-tickets { user: user, event-id: event-id })))
)

(define-read-only (get-platform-fee (amount uint))
  (/ (* amount (var-get platform-fee-rate)) u10000)
)

(define-read-only (is-event-active (event-id uint))
  (match (get-event event-id)
    event-data (and 
      (get is-active event-data)
      (> (get event-date event-data) block-height)
      (< (get sold-tickets event-data) (get total-tickets event-data))
    )
    false
  )
)

;; Private functions
(define-private (is-event-organizer (event-id uint) (user principal))
  (match (get-event event-id)
    event-data (is-eq (get organizer event-data) user)
    false
  )
)

;; Create individual tickets (helper function)
;; Updated to use counter-based approach instead of slice?
(define-private (create-single-ticket (index uint) (context { event-id: uint, ticket-price: uint, success: bool, created: uint, target: uint }))
  (if (and (get success context) (< (get created context) (get target context)))
    (let ((ticket-id (var-get next-ticket-id)))
      (map-set tickets
        { ticket-id: ticket-id }
        {
          event-id: (get event-id context),
          owner: tx-sender,
          purchase-date: block-height,
          is-used: false,
          purchase-price: (get ticket-price context)
        }
      )
      (var-set next-ticket-id (+ ticket-id u1))
      (merge context { created: (+ (get created context) u1) })
    )
    context
  )
)

;; Replaced slice? with counter-based fold approach
(define-private (create-tickets (event-id uint) (quantity uint) (ticket-price uint))
  (let ((ticket-indices (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19 u20)))
    (asserts! (<= quantity u20) ERR-INVALID-PRICE) ;; Limit to 20 tickets per transaction
    (asserts! (> quantity u0) ERR-INVALID-PRICE)
    
    (fold create-single-ticket 
      ticket-indices
      { event-id: event-id, ticket-price: ticket-price, success: true, created: u0, target: quantity }
    )
    (ok true)
  )
)

;; Public functions

;; Create a new event
(define-public (create-event 
  (name (string-ascii 100))
  (description (string-ascii 500))
  (venue (string-ascii 200))
  (event-date uint)
  (ticket-price uint)
  (total-tickets uint)
  (refund-enabled bool)
)
  (let ((event-id (var-get next-event-id)))
    ;; Added input validation to fix compiler warnings
    (asserts! (> (len name) u0) ERR-INVALID-INPUT)
    (asserts! (> (len description) u0) ERR-INVALID-INPUT)
    (asserts! (> (len venue) u0) ERR-INVALID-INPUT)
    (asserts! (> ticket-price u0) ERR-INVALID-PRICE)
    (asserts! (> total-tickets u0) ERR-INVALID-PRICE)
    (asserts! (> event-date block-height) ERR-EVENT-ENDED)
    
    (map-set events
      { event-id: event-id }
      {
        organizer: tx-sender,
        name: name,
        description: description,
        venue: venue,
        event-date: event-date,
        ticket-price: ticket-price,
        total-tickets: total-tickets,
        sold-tickets: u0,
        is-active: true,
        refund-enabled: refund-enabled
      }
    )
    
    (var-set next-event-id (+ event-id u1))
    (ok event-id)
  )
)

;; Purchase tickets
(define-public (purchase-ticket (event-id uint) (quantity uint))
  (let (
    (event-data (unwrap! (get-event event-id) ERR-EVENT-NOT-FOUND))
    (total-cost (* (get ticket-price event-data) quantity))
    (platform-fee (get-platform-fee total-cost))
    (organizer-amount (- total-cost platform-fee))
    (current-sold (get sold-tickets event-data))
    (user-current-tickets (get-user-ticket-count tx-sender event-id))
  )
    (asserts! (is-event-active event-id) ERR-EVENT-NOT-ACTIVE)
    (asserts! (<= (+ current-sold quantity) (get total-tickets event-data)) ERR-EVENT-SOLD-OUT)
    (asserts! (> quantity u0) ERR-INVALID-PRICE)
    
    ;; Transfer payment
    (try! (stx-transfer? total-cost tx-sender (as-contract tx-sender)))
    (try! (as-contract (stx-transfer? organizer-amount tx-sender (get organizer event-data))))
    
    ;; Pass ticket price to avoid circular dependency
    (try! (create-tickets event-id quantity (get ticket-price event-data)))
    
    ;; Update event sold tickets
    (map-set events
      { event-id: event-id }
      (merge event-data { sold-tickets: (+ current-sold quantity) })
    )
    
    ;; Update user ticket count
    (map-set user-tickets
      { user: tx-sender, event-id: event-id }
      { ticket-count: (+ user-current-tickets quantity) }
    )
    
    (ok quantity)
  )
)

;; Validate and use ticket
(define-public (use-ticket (ticket-id uint))
  (let ((ticket-data (unwrap! (get-ticket ticket-id) ERR-TICKET-NOT-FOUND)))
    (asserts! (is-eq (get owner ticket-data) tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (not (get is-used ticket-data)) ERR-TICKET-ALREADY-USED)
    
    (map-set tickets
      { ticket-id: ticket-id }
      (merge ticket-data { is-used: true })
    )
    
    (ok true)
  )
)

;; Transfer ticket ownership
(define-public (transfer-ticket (ticket-id uint) (new-owner principal))
  (let ((ticket-data (unwrap! (get-ticket ticket-id) ERR-TICKET-NOT-FOUND)))
    ;; Added validation for new-owner parameter to fix compiler warning
    (asserts! (not (is-eq new-owner tx-sender)) ERR-INVALID-INPUT)
    (asserts! (is-eq (get owner ticket-data) tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (not (get is-used ticket-data)) ERR-TICKET-ALREADY-USED)
    
    (map-set tickets
      { ticket-id: ticket-id }
      (merge ticket-data { owner: new-owner })
    )
    
    ;; Update user ticket counts
    (let (
      (event-id (get event-id ticket-data))
      (old-owner-count (get-user-ticket-count tx-sender event-id))
      (new-owner-count (get-user-ticket-count new-owner event-id))
    )
      (map-set user-tickets
        { user: tx-sender, event-id: event-id }
        { ticket-count: (- old-owner-count u1) }
      )
      (map-set user-tickets
        { user: new-owner, event-id: event-id }
        { ticket-count: (+ new-owner-count u1) }
      )
    )
    
    (ok true)
  )
)

;; Request refund (if enabled)
(define-public (request-refund (ticket-id uint))
  (let (
    (ticket-data (unwrap! (get-ticket ticket-id) ERR-TICKET-NOT-FOUND))
    (event-data (unwrap! (get-event (get event-id ticket-data)) ERR-EVENT-NOT-FOUND))
    (refund-amount (get purchase-price ticket-data))
  )
    (asserts! (is-eq (get owner ticket-data) tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (not (get is-used ticket-data)) ERR-TICKET-ALREADY-USED)
    (asserts! (get refund-enabled event-data) ERR-REFUND-NOT-ALLOWED)
    (asserts! (> (get event-date event-data) block-height) ERR-EVENT-ENDED)
    
    ;; Process refund
    (try! (as-contract (stx-transfer? refund-amount tx-sender tx-sender)))
    
    ;; Mark ticket as used (to prevent double refund)
    (map-set tickets
      { ticket-id: ticket-id }
      (merge ticket-data { is-used: true })
    )
    
    ;; Update sold tickets count
    (map-set events
      { event-id: (get event-id ticket-data) }
      (merge event-data { sold-tickets: (- (get sold-tickets event-data) u1) })
    )
    
    (ok refund-amount)
  )
)

;; Event organizer functions
(define-public (toggle-event-status (event-id uint))
  (let ((event-data (unwrap! (get-event event-id) ERR-EVENT-NOT-FOUND)))
    ;; Added validation for event-id parameter to fix compiler warning
    (asserts! (> event-id u0) ERR-INVALID-INPUT)
    (asserts! (is-event-organizer event-id tx-sender) ERR-NOT-AUTHORIZED)
    
    (map-set events
      { event-id: event-id }
      (merge event-data { is-active: (not (get is-active event-data)) })
    )
    
    (ok (not (get is-active event-data)))
  )
)

;; Withdraw platform fees (contract owner only)
(define-public (withdraw-fees (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (try! (as-contract (stx-transfer? amount tx-sender CONTRACT-OWNER)))
    (ok amount)
  )
)

;; Update platform fee rate (contract owner only)
(define-public (set-platform-fee-rate (new-rate uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (<= new-rate u1000) ERR-INVALID-PRICE) ;; Max 10%
    (var-set platform-fee-rate new-rate)
    (ok new-rate)
  )
)
