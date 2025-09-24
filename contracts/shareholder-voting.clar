;; Corporate Governance Voting Smart Contract
;; Manages shareholder voting with transparent ballot counting and proxy management

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-unauthorized (err u100))
(define-constant err-not-found (err u101))
(define-constant err-invalid-input (err u102))
(define-constant err-voting-closed (err u103))
(define-constant err-already-voted (err u104))
(define-constant err-insufficient-shares (err u105))
(define-constant err-invalid-proposal (err u106))

;; Data variables
(define-data-var proposal-counter uint u0)
(define-data-var shareholder-counter uint u0)

;; Vote types
(define-constant vote-for u1)
(define-constant vote-against u2)
(define-constant vote-abstain u3)

;; Proposal status
(define-constant status-active "ACTIVE")
(define-constant status-closed "CLOSED")
(define-constant status-executed "EXECUTED")

;; Shareholder registry
(define-map shareholders
  { shareholder: principal }
  {
    shares-owned: uint,
    share-class: (string-ascii 10),
    voting-power: uint,
    verified: bool,
    registered-at: uint
  }
)

;; Proxy delegations
(define-map proxy-delegations
  { delegator: principal }
  {
    proxy: principal,
    delegated-at: uint,
    active: bool,
    revocable: bool
  }
)

;; Proposals
(define-map proposals
  { proposal-id: uint }
  {
    title: (string-ascii 100),
    description-hash: (buff 32),
    proposer: principal,
    proposal-type: (string-ascii 20),
    voting-start: uint,
    voting-end: uint,
    quorum-required: uint,
    approval-threshold: uint,
    status: (string-ascii 20),
    votes-for: uint,
    votes-against: uint,
    votes-abstain: uint,
    total-voting-power: uint
  }
)

;; Vote records
(define-map votes
  { proposal-id: uint, voter: principal }
  {
    vote-choice: uint,
    voting-power-used: uint,
    voted-at: uint,
    vote-hash: (buff 32),
    proxy-vote: bool
  }
)

;; Board of directors
(define-map board-members
  { member: principal }
  {
    position: (string-ascii 30),
    appointed-at: uint,
    active: bool,
    voting-rights: bool
  }
)

;; Voting history for analytics
(define-map voting-statistics
  { proposal-id: uint }
  {
    participation-rate: uint,
    institutional-votes: uint,
    retail-votes: uint,
    proxy-votes: uint,
    direct-votes: uint
  }
)

;; Authorization functions
(define-private (is-contract-owner)
  (is-eq tx-sender contract-owner)
)

(define-private (is-board-member)
  (match (map-get? board-members { member: tx-sender })
    member (and (get active member) (get voting-rights member))
    false
  )
)

(define-private (is-verified-shareholder (shareholder principal))
  (match (map-get? shareholders { shareholder: shareholder })
    holder (get verified holder)
    false
  )
)

(define-private (get-voting-power (shareholder principal))
  (match (map-get? shareholders { shareholder: shareholder })
    holder (get voting-power holder)
    u0
  )
)

;; Shareholder management
(define-public (register-shareholder
  (shareholder principal)
  (shares-owned uint)
  (share-class (string-ascii 10))
  (voting-power uint)
)
  (begin
    (asserts! (is-contract-owner) err-unauthorized)
    (asserts! (> shares-owned u0) err-invalid-input)
    
    (map-set shareholders
      { shareholder: shareholder }
      {
        shares-owned: shares-owned,
        share-class: share-class,
        voting-power: voting-power,
        verified: true,
        registered-at: u1
      }
    )
    (ok true)
  )
)

(define-public (update-share-ownership
  (shareholder principal)
  (new-shares uint)
  (new-voting-power uint)
)
  (begin
    (asserts! (is-contract-owner) err-unauthorized)
    
    (match (map-get? shareholders { shareholder: shareholder })
      holder
      (begin
        (map-set shareholders
          { shareholder: shareholder }
          (merge holder {
            shares-owned: new-shares,
            voting-power: new-voting-power
          })
        )
        (ok true)
      )
      err-not-found
    )
  )
)

;; Board management
(define-public (add-board-member
  (member principal)
  (position (string-ascii 30))
  (voting-rights bool)
)
  (begin
    (asserts! (is-contract-owner) err-unauthorized)
    
    (map-set board-members
      { member: member }
      {
        position: position,
        appointed-at: u1,
        active: true,
        voting-rights: voting-rights
      }
    )
    (ok true)
  )
)

;; Proxy management
(define-public (delegate-proxy
  (proxy principal)
  (revocable bool)
)
  (begin
    (asserts! (is-verified-shareholder tx-sender) err-unauthorized)
    (asserts! (is-verified-shareholder proxy) err-invalid-input)
    
    (map-set proxy-delegations
      { delegator: tx-sender }
      {
        proxy: proxy,
        delegated-at: u1,
        active: true,
        revocable: revocable
      }
    )
    (ok true)
  )
)

(define-public (revoke-proxy)
  (begin
    (match (map-get? proxy-delegations { delegator: tx-sender })
      delegation
      (begin
        (asserts! (get revocable delegation) err-unauthorized)
        (map-set proxy-delegations
          { delegator: tx-sender }
          (merge delegation { active: false })
        )
        (ok true)
      )
      err-not-found
    )
  )
)

;; Proposal management
(define-public (create-proposal
  (title (string-ascii 100))
  (description-hash (buff 32))
  (proposal-type (string-ascii 20))
  (voting-duration uint)
  (quorum-required uint)
  (approval-threshold uint)
)
  (begin
    (asserts! (or (is-contract-owner) (is-board-member)) err-unauthorized)
    (asserts! (> voting-duration u0) err-invalid-input)
    (asserts! (<= approval-threshold u10000) err-invalid-input)
    
    (let 
      (
        (proposal-id (+ (var-get proposal-counter) u1))
        (current-time u1)
      )
      (map-set proposals
        { proposal-id: proposal-id }
        {
          title: title,
          description-hash: description-hash,
          proposer: tx-sender,
          proposal-type: proposal-type,
          voting-start: current-time,
          voting-end: (+ current-time voting-duration),
          quorum-required: quorum-required,
          approval-threshold: approval-threshold,
          status: status-active,
          votes-for: u0,
          votes-against: u0,
          votes-abstain: u0,
          total-voting-power: u0
        }
      )
      
      (var-set proposal-counter proposal-id)
      (ok proposal-id)
    )
  )
)

(define-public (close-proposal (proposal-id uint))
  (begin
    (asserts! (is-contract-owner) err-unauthorized)
    
    (match (map-get? proposals { proposal-id: proposal-id })
      proposal
      (begin
        (map-set proposals
          { proposal-id: proposal-id }
          (merge proposal { status: status-closed })
        )
        (ok true)
      )
      err-not-found
    )
  )
)

;; Voting functions
(define-public (cast-vote
  (proposal-id uint)
  (vote-choice uint)
  (vote-hash (buff 32))
)
  (begin
    (asserts! (is-verified-shareholder tx-sender) err-unauthorized)
    (asserts! (>= vote-choice vote-for) err-invalid-input)
    (asserts! (<= vote-choice vote-abstain) err-invalid-input)
    
    ;; Check if already voted
    (asserts! (is-none (map-get? votes { proposal-id: proposal-id, voter: tx-sender })) err-already-voted)
    
    (match (map-get? proposals { proposal-id: proposal-id })
      proposal
      (begin
        (asserts! (is-eq (get status proposal) status-active) err-voting-closed)
        
        (let ((voting-power (get-voting-power tx-sender)))
          (asserts! (> voting-power u0) err-insufficient-shares)
          
          ;; Record vote
          (map-set votes
            { proposal-id: proposal-id, voter: tx-sender }
            {
              vote-choice: vote-choice,
              voting-power-used: voting-power,
              voted-at: u1,
              vote-hash: vote-hash,
              proxy-vote: false
            }
          )
          
          ;; Update proposal vote counts
          (let 
            (
              (updated-proposal
                (if (is-eq vote-choice vote-for)
                  (merge proposal { 
                    votes-for: (+ (get votes-for proposal) voting-power),
                    total-voting-power: (+ (get total-voting-power proposal) voting-power)
                  })
                  (if (is-eq vote-choice vote-against)
                    (merge proposal {
                      votes-against: (+ (get votes-against proposal) voting-power),
                      total-voting-power: (+ (get total-voting-power proposal) voting-power)
                    })
                    (merge proposal {
                      votes-abstain: (+ (get votes-abstain proposal) voting-power),
                      total-voting-power: (+ (get total-voting-power proposal) voting-power)
                    })
                  )
                )
              )
            )
            (map-set proposals { proposal-id: proposal-id } updated-proposal)
            (ok voting-power)
          )
        )
      )
      err-not-found
    )
  )
)

(define-public (cast-proxy-vote
  (proposal-id uint)
  (delegator principal)
  (vote-choice uint)
  (vote-hash (buff 32))
)
  (begin
    (asserts! (is-verified-shareholder tx-sender) err-unauthorized)
    
    ;; Verify proxy authorization
    (match (map-get? proxy-delegations { delegator: delegator })
      delegation
      (begin
        (asserts! (is-eq (get proxy delegation) tx-sender) err-unauthorized)
        (asserts! (get active delegation) err-unauthorized)
        
        ;; Check if delegator already voted
        (asserts! (is-none (map-get? votes { proposal-id: proposal-id, voter: delegator })) err-already-voted)
        
        (let ((voting-power (get-voting-power delegator)))
          (asserts! (> voting-power u0) err-insufficient-shares)
          
          ;; Record proxy vote
          (map-set votes
            { proposal-id: proposal-id, voter: delegator }
            {
              vote-choice: vote-choice,
              voting-power-used: voting-power,
              voted-at: u1,
              vote-hash: vote-hash,
              proxy-vote: true
            }
          )
          (ok voting-power)
        )
      )
      err-not-found
    )
  )
)

;; Read-only functions
(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals { proposal-id: proposal-id })
)

(define-read-only (get-shareholder (shareholder principal))
  (map-get? shareholders { shareholder: shareholder })
)

(define-read-only (get-vote (proposal-id uint) (voter principal))
  (map-get? votes { proposal-id: proposal-id, voter: voter })
)

(define-read-only (get-proxy-delegation (delegator principal))
  (map-get? proxy-delegations { delegator: delegator })
)

(define-read-only (get-board-member (member principal))
  (map-get? board-members { member: member })
)

(define-read-only (get-proposal-counter)
  (var-get proposal-counter)
)

(define-read-only (calculate-proposal-result (proposal-id uint))
  (match (map-get? proposals { proposal-id: proposal-id })
    proposal
    (let 
      (
        (total-votes (+ (get votes-for proposal) (get votes-against proposal)))
        (approval-rate (if (> total-votes u0)
                        (/ (* (get votes-for proposal) u10000) total-votes)
                        u0))
      )
      (some {
        total-votes: total-votes,
        votes-for: (get votes-for proposal),
        votes-against: (get votes-against proposal),
        votes-abstain: (get votes-abstain proposal),
        approval-rate: approval-rate,
        quorum-met: (>= total-votes (get quorum-required proposal)),
        proposal-passed: (and 
          (>= total-votes (get quorum-required proposal))
          (>= approval-rate (get approval-threshold proposal))
        )
      })
    )
    none
  )
)
