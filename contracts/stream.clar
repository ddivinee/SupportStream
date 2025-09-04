;; SupportStream - Customer Service Automation System
;; A decentralized customer service automation platform on Stacks

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_TICKET_NOT_FOUND (err u101))
(define-constant ERR_INVALID_STATUS (err u102))
(define-constant ERR_INVALID_PRIORITY (err u103))
(define-constant ERR_AGENT_NOT_FOUND (err u104))
(define-constant ERR_INSUFFICIENT_PAYMENT (err u105))
(define-constant ERR_TICKET_ALREADY_CLOSED (err u106))
(define-constant ERR_INVALID_INPUT (err u107))

;; Ticket status enum
(define-constant STATUS_OPEN u1)
(define-constant STATUS_IN_PROGRESS u2)
(define-constant STATUS_RESOLVED u3)
(define-constant STATUS_CLOSED u4)

;; Priority levels
(define-constant PRIORITY_LOW u1)
(define-constant PRIORITY_MEDIUM u2)
(define-constant PRIORITY_HIGH u3)
(define-constant PRIORITY_URGENT u4)

;; Data structures
(define-map tickets
  { ticket-id: uint }
  {
    customer: principal,
    agent: (optional principal),
    title: (string-ascii 100),
    description: (string-ascii 500),
    status: uint,
    priority: uint,
    created-at: uint,
    updated-at: uint,
    resolution: (optional (string-ascii 500))
  }
)

(define-map agents
  { agent-id: principal }
  {
    name: (string-ascii 50),
    specialization: (string-ascii 100),
    rating: uint,
    active-tickets: uint,
    total-resolved: uint,
    joined-at: uint
  }
)

(define-map ticket-messages
  { ticket-id: uint, message-id: uint }
  {
    sender: principal,
    message: (string-ascii 500),
    timestamp: uint,
    is-internal: bool
  }
)

(define-map service-fees
  { priority: uint }
  { fee: uint }
)

;; Data variables
(define-data-var next-ticket-id uint u1)
(define-data-var next-message-id uint u1)
(define-data-var platform-fee-percentage uint u5) ;; 5% platform fee
(define-data-var contract-paused bool false)

;; Initialize service fees
(map-set service-fees { priority: PRIORITY_LOW } { fee: u1000000 }) ;; 1 STX
(map-set service-fees { priority: PRIORITY_MEDIUM } { fee: u2000000 }) ;; 2 STX
(map-set service-fees { priority: PRIORITY_HIGH } { fee: u5000000 }) ;; 5 STX
(map-set service-fees { priority: PRIORITY_URGENT } { fee: u10000000 }) ;; 10 STX

;; Private functions
(define-private (is-valid-status (status uint))
  (or (is-eq status STATUS_OPEN)
      (is-eq status STATUS_IN_PROGRESS)
      (is-eq status STATUS_RESOLVED)
      (is-eq status STATUS_CLOSED))
)

(define-private (is-valid-priority (priority uint))
  (or (is-eq priority PRIORITY_LOW)
      (is-eq priority PRIORITY_MEDIUM)
      (is-eq priority PRIORITY_HIGH)
      (is-eq priority PRIORITY_URGENT))
)

(define-private (calculate-agent-payment (total-fee uint))
  (let ((platform-fee (/ (* total-fee (var-get platform-fee-percentage)) u100)))
    (- total-fee platform-fee))
)

;; Public functions

;; Register as a customer service agent
(define-public (register-agent (agent-name (string-ascii 50)) (agent-specialization (string-ascii 100)))
  (begin
    (asserts! (not (var-get contract-paused)) ERR_NOT_AUTHORIZED)
    (asserts! (> (len agent-name) u0) ERR_INVALID_INPUT)
    (asserts! (> (len agent-specialization) u0) ERR_INVALID_INPUT)
    (map-set agents
      { agent-id: tx-sender }
      {
        name: agent-name,
        specialization: agent-specialization,
        rating: u5, ;; Start with 5/10 rating
        active-tickets: u0,
        total-resolved: u0,
        joined-at: block-height
      }
    )
    (ok true)
  )
)

;; Create a new support ticket
(define-public (create-ticket 
  (ticket-title (string-ascii 100))
  (ticket-description (string-ascii 500))
  (ticket-priority uint))
  (let ((new-ticket-id (var-get next-ticket-id))
        (service-fee (default-to u0 (get fee (map-get? service-fees { priority: ticket-priority })))))
    (begin
      (asserts! (not (var-get contract-paused)) ERR_NOT_AUTHORIZED)
      (asserts! (is-valid-priority ticket-priority) ERR_INVALID_PRIORITY)
      (asserts! (> (len ticket-title) u0) ERR_INVALID_INPUT)
      (asserts! (> (len ticket-description) u0) ERR_INVALID_INPUT)
      (asserts! (>= (stx-get-balance tx-sender) service-fee) ERR_INSUFFICIENT_PAYMENT)
      
      ;; Transfer service fee to contract
      (try! (stx-transfer? service-fee tx-sender (as-contract tx-sender)))
      
      ;; Create ticket
      (map-set tickets
        { ticket-id: new-ticket-id }
        {
          customer: tx-sender,
          agent: none,
          title: ticket-title,
          description: ticket-description,
          status: STATUS_OPEN,
          priority: ticket-priority,
          created-at: block-height,
          updated-at: block-height,
          resolution: none
        }
      )
      
      ;; Increment ticket counter
      (var-set next-ticket-id (+ new-ticket-id u1))
      
      (ok new-ticket-id)
    )
  )
)

;; Assign ticket to agent
(define-public (assign-ticket (target-ticket-id uint))
  (let ((ticket-data (unwrap! (map-get? tickets { ticket-id: target-ticket-id }) ERR_TICKET_NOT_FOUND))
        (agent-data (unwrap! (map-get? agents { agent-id: tx-sender }) ERR_AGENT_NOT_FOUND)))
    (begin
      (asserts! (not (var-get contract-paused)) ERR_NOT_AUTHORIZED)
      (asserts! (> target-ticket-id u0) ERR_TICKET_NOT_FOUND)
      (asserts! (is-eq (get status ticket-data) STATUS_OPEN) ERR_INVALID_STATUS)
      
      ;; Update ticket with agent and status
      (map-set tickets
        { ticket-id: target-ticket-id }
        (merge ticket-data {
          agent: (some tx-sender),
          status: STATUS_IN_PROGRESS,
          updated-at: block-height
        })
      )
      
      ;; Update agent's active ticket count
      (map-set agents
        { agent-id: tx-sender }
        (merge agent-data {
          active-tickets: (+ (get active-tickets agent-data) u1)
        })
      )
      
      (ok true)
    )
  )
)

;; Add message to ticket
(define-public (add-message 
  (target-ticket-id uint)
  (msg-content (string-ascii 500))
  (is-internal bool))
  (let ((ticket-data (unwrap! (map-get? tickets { ticket-id: target-ticket-id }) ERR_TICKET_NOT_FOUND))
        (new-message-id (var-get next-message-id)))
    (begin
      (asserts! (not (var-get contract-paused)) ERR_NOT_AUTHORIZED)
      (asserts! (> target-ticket-id u0) ERR_TICKET_NOT_FOUND)
      (asserts! (> (len msg-content) u0) ERR_INVALID_INPUT)
      (asserts! (or (is-eq tx-sender (get customer ticket-data))
                    (is-eq (some tx-sender) (get agent ticket-data))) ERR_NOT_AUTHORIZED)
      (asserts! (not (is-eq (get status ticket-data) STATUS_CLOSED)) ERR_TICKET_ALREADY_CLOSED)
      
      ;; Add message
      (map-set ticket-messages
        { ticket-id: target-ticket-id, message-id: new-message-id }
        {
          sender: tx-sender,
          message: msg-content,
          timestamp: block-height,
          is-internal: is-internal
        }
      )
      
      ;; Update ticket timestamp
      (map-set tickets
        { ticket-id: target-ticket-id }
        (merge ticket-data { updated-at: block-height })
      )
      
      ;; Increment message counter
      (var-set next-message-id (+ new-message-id u1))
      
      (ok new-message-id)
    )
  )
)

;; Resolve ticket
(define-public (resolve-ticket 
  (target-ticket-id uint)
  (ticket-resolution (string-ascii 500)))
  (let ((ticket-data (unwrap! (map-get? tickets { ticket-id: target-ticket-id }) ERR_TICKET_NOT_FOUND))
        (agent-data (unwrap! (map-get? agents { agent-id: tx-sender }) ERR_AGENT_NOT_FOUND)))
    (begin
      (asserts! (not (var-get contract-paused)) ERR_NOT_AUTHORIZED)
      (asserts! (> target-ticket-id u0) ERR_TICKET_NOT_FOUND)
      (asserts! (> (len ticket-resolution) u0) ERR_INVALID_INPUT)
      (asserts! (is-eq (some tx-sender) (get agent ticket-data)) ERR_NOT_AUTHORIZED)
      (asserts! (is-eq (get status ticket-data) STATUS_IN_PROGRESS) ERR_INVALID_STATUS)
      
      ;; Update ticket status and resolution
      (map-set tickets
        { ticket-id: target-ticket-id }
        (merge ticket-data {
          status: STATUS_RESOLVED,
          updated-at: block-height,
          resolution: (some ticket-resolution)
        })
      )
      
      (ok true)
    )
  )
)

;; Close ticket and pay agent
(define-public (close-ticket (target-ticket-id uint) (agent-rating uint))
  (let ((ticket-data (unwrap! (map-get? tickets { ticket-id: target-ticket-id }) ERR_TICKET_NOT_FOUND))
        (agent-principal (unwrap! (get agent ticket-data) ERR_AGENT_NOT_FOUND))
        (agent-data (unwrap! (map-get? agents { agent-id: agent-principal }) ERR_AGENT_NOT_FOUND))
        (service-fee (default-to u0 (get fee (map-get? service-fees { priority: (get priority ticket-data) }))))
        (agent-payment (calculate-agent-payment service-fee)))
    (begin
      (asserts! (not (var-get contract-paused)) ERR_NOT_AUTHORIZED)
      (asserts! (> target-ticket-id u0) ERR_TICKET_NOT_FOUND)
      (asserts! (is-eq tx-sender (get customer ticket-data)) ERR_NOT_AUTHORIZED)
      (asserts! (is-eq (get status ticket-data) STATUS_RESOLVED) ERR_INVALID_STATUS)
      (asserts! (and (>= agent-rating u1) (<= agent-rating u10)) ERR_NOT_AUTHORIZED)
      
      ;; Pay agent
      (try! (as-contract (stx-transfer? agent-payment tx-sender agent-principal)))
      
      ;; Update ticket status
      (map-set tickets
        { ticket-id: target-ticket-id }
        (merge ticket-data {
          status: STATUS_CLOSED,
          updated-at: block-height
        })
      )
      
      ;; Update agent statistics
      (map-set agents
        { agent-id: agent-principal }
        (merge agent-data {
          active-tickets: (- (get active-tickets agent-data) u1),
          total-resolved: (+ (get total-resolved agent-data) u1),
          rating: (/ (+ (* (get rating agent-data) (get total-resolved agent-data)) agent-rating)
                     (+ (get total-resolved agent-data) u1))
        })
      )
      
      (ok true)
    )
  )
)

;; Read-only functions

;; Get ticket details
(define-read-only (get-ticket (ticket-id uint))
  (map-get? tickets { ticket-id: ticket-id })
)

;; Get agent details
(define-read-only (get-agent (agent-id principal))
  (map-get? agents { agent-id: agent-id })
)

;; Get ticket message
(define-read-only (get-message (ticket-id uint) (message-id uint))
  (map-get? ticket-messages { ticket-id: ticket-id, message-id: message-id })
)

;; Get service fee for priority level
(define-read-only (get-service-fee (priority uint))
  (map-get? service-fees { priority: priority })
)

;; Get next ticket ID
(define-read-only (get-next-ticket-id)
  (var-get next-ticket-id)
)

;; Admin functions

;; Update service fee (admin only)
(define-public (update-service-fee (fee-priority uint) (updated-fee uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (is-valid-priority fee-priority) ERR_INVALID_PRIORITY)
    (asserts! (> updated-fee u0) ERR_NOT_AUTHORIZED)
    (map-set service-fees { priority: fee-priority } { fee: updated-fee })
    (ok true)
  )
)

;; Update platform fee percentage (admin only)
(define-public (update-platform-fee (new-percentage uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (<= new-percentage u20) ERR_NOT_AUTHORIZED) ;; Max 20%
    (var-set platform-fee-percentage new-percentage)
    (ok true)
  )
)

;; Pause/unpause contract (admin only)
(define-public (set-contract-pause (paused bool))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (var-set contract-paused paused)
    (ok true)
  )
)

;; Withdraw platform fees (admin only)
(define-public (withdraw-fees (withdrawal-amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (> withdrawal-amount u0) ERR_NOT_AUTHORIZED)
    (asserts! (>= (stx-get-balance (as-contract tx-sender)) withdrawal-amount) ERR_INSUFFICIENT_PAYMENT)
    (as-contract (stx-transfer? withdrawal-amount tx-sender CONTRACT_OWNER))
  )
)