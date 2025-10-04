(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_PROPOSAL_NOT_FOUND (err u101))
(define-constant ERR_ALREADY_VOTED (err u102))
(define-constant ERR_PROPOSAL_ENDED (err u103))
(define-constant ERR_INSUFFICIENT_STAKE (err u104))
(define-constant ERR_INVALID_TITLE (err u105))
(define-constant ERR_INVALID_DESCRIPTION (err u106))
(define-constant ERR_INVALID_DURATION (err u107))
(define-constant ERR_INVALID_PROPOSAL_TYPE (err u108))
(define-constant ERR_PROPOSAL_ACTIVE (err u109))
(define-constant ERR_NO_QUORUM (err u110))
(define-constant ERR_ALREADY_EXECUTED (err u111))
(define-constant ERR_INVALID_STAKE_AMOUNT (err u112))
(define-constant ERR_MEMBER_NOT_FOUND (err u113))
(define-constant ERR_INSUFFICIENT_BALANCE (err u114))
(define-constant ERR_TRANSFER_FAILED (err u115))
(define-constant ERR_INVALID_RATE (err u116))
(define-constant ERR_UPDATE_NOT_ALLOWED (err u117))
(define-constant ERR_INVALID_THRESHOLD (err u118))
(define-constant ERR_MAX_PROPOSALS_EXCEEDED (err u119))
(define-constant ERR_INVALID_CONTENT_ID (err u120))
(define-constant ERR_INVALID_ROYALTY_RATE (err u121))
(define-constant ERR_INVALID_UPGRADE_PARAM (err u122))

(define-data-var proposal-count uint u0)
(define-data-var min-stake uint u1000)
(define-data-var max-proposals uint u1000)
(define-data-var quorum-threshold uint u50)
(define-data-var royalty-rate uint u10)
(define-data-var treasury principal tx-sender)

(define-map proposals
  { proposal-id: uint }
  {
    creator: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    start-block: uint,
    end-block: uint,
    votes-for: uint,
    votes-against: uint,
    executed: bool,
    proposal-type: (string-ascii 20),
    param: (optional uint)
  }
)

(define-map votes
  { proposal-id: uint, voter: principal }
  { vote: bool }
)

(define-map member-stakes
  { member: principal }
  { stake: uint }
)

(define-map total-votes
  { proposal-id: uint }
  { total: uint }
)

(define-read-only (get-proposal (id uint))
  (map-get? proposals { proposal-id: id })
)

(define-read-only (get-vote (id uint) (voter principal))
  (map-get? votes { proposal-id: id, voter: voter })
)

(define-read-only (get-member-stake (member principal))
  (default-to u0 (get stake (map-get? member-stakes { member: member })))
)

(define-read-only (get-total-votes (id uint))
  (default-to u0 (get total (map-get? total-votes { proposal-id: id })))
)

(define-private (validate-title (title (string-ascii 100)))
  (if (and (> (len title) u0) (<= (len title) u100))
      (ok true)
      ERR_INVALID_TITLE)
)

(define-private (validate-description (desc (string-ascii 500)))
  (if (and (> (len desc) u0) (<= (len desc) u500))
      (ok true)
      ERR_INVALID_DESCRIPTION)
)

(define-private (validate-duration (dur uint))
  (if (and (>= dur u100) (<= dur u10000))
      (ok true)
      ERR_INVALID_DURATION)
)

(define-private (validate-proposal-type (ptype (string-ascii 20)))
  (if (or (is-eq ptype "content-approval") (is-eq ptype "royalty-rate") (is-eq ptype "platform-upgrade") (is-eq ptype "quorum-change"))
      (ok true)
      ERR_INVALID_PROPOSAL_TYPE)
)

(define-private (validate-stake-amount (amount uint))
  (if (>= amount (var-get min-stake))
      (ok true)
      ERR_INVALID_STAKE_AMOUNT)
)

(define-private (validate-rate (rate uint))
  (if (and (> rate u0) (<= rate u50))
      (ok true)
      ERR_INVALID_RATE)
)

(define-private (validate-threshold (thresh uint))
  (if (and (>= thresh u10) (<= thresh u90))
      (ok true)
      ERR_INVALID_THRESHOLD)
)

(define-public (set-min-stake (new-min uint))
  (begin
    (asserts! (is-eq tx-sender (var-get treasury)) ERR_NOT_AUTHORIZED)
    (try! (validate-stake-amount new-min))
    (var-set min-stake new-min)
    (ok true)
  )
)

(define-public (set-quorum-threshold (new-thresh uint))
  (begin
    (asserts! (is-eq tx-sender (var-get treasury)) ERR_NOT_AUTHORIZED)
    (try! (validate-threshold new-thresh))
    (var-set quorum-threshold new-thresh)
    (ok true)
  )
)

(define-public (set-royalty-rate (new-rate uint))
  (begin
    (asserts! (is-eq tx-sender (var-get treasury)) ERR_NOT_AUTHORIZED)
    (try! (validate-rate new-rate))
    (var-set royalty-rate new-rate)
    (ok true)
  )
)

(define-public (join-dao (stake-amount uint))
  (let ((current-stake (get-member-stake tx-sender)))
    (try! (validate-stake-amount stake-amount))
    (asserts! (>= (stx-get-balance tx-sender) stake-amount) ERR_INSUFFICIENT_BALANCE)
    (try! (stx-transfer? stake-amount tx-sender (as-contract tx-sender)))
    (map-set member-stakes { member: tx-sender } { stake: (+ current-stake stake-amount) })
    (ok true)
  )
)

(define-public (leave-dao)
  (let ((stake (get-member-stake tx-sender)))
    (asserts! (> stake u0) ERR_MEMBER_NOT_FOUND)
    (try! (as-contract (stx-transfer? stake tx-sender tx-sender)))
    (map-delete member-stakes { member: tx-sender })
    (ok true)
  )
)

(define-public (create-proposal (title (string-ascii 100)) (description (string-ascii 500)) (duration uint) (ptype (string-ascii 20)) (param (optional uint)))
  (let
    (
      (proposal-id (+ (var-get proposal-count) u1))
      (creator tx-sender)
      (stake (get-member-stake creator))
    )
    (asserts! (>= stake (var-get min-stake)) ERR_INSUFFICIENT_STAKE)
    (asserts! (< (var-get proposal-count) (var-get max-proposals)) ERR_MAX_PROPOSALS_EXCEEDED)
    (try! (validate-title title))
    (try! (validate-description description))
    (try! (validate-duration duration))
    (try! (validate-proposal-type ptype))
    (map-set proposals
      { proposal-id: proposal-id }
      {
        creator: creator,
        title: title,
        description: description,
        start-block: block-height,
        end-block: (+ block-height duration),
        votes-for: u0,
        votes-against: u0,
        executed: false,
        proposal-type: ptype,
        param: param
      }
    )
    (map-set total-votes { proposal-id: proposal-id } { total: u0 })
    (var-set proposal-count proposal-id)
    (ok proposal-id)
  )
)

(define-public (vote (proposal-id uint) (vote-for bool))
  (let
    (
      (proposal (unwrap! (get-proposal proposal-id) ERR_PROPOSAL_NOT_FOUND))
      (voter tx-sender)
      (stake (get-member-stake voter))
      (current-total (get-total-votes proposal-id))
    )
    (asserts! (>= stake (var-get min-stake)) ERR_INSUFFICIENT_STAKE)
    (asserts! (> (get end-block proposal) block-height) ERR_PROPOSAL_ENDED)
    (asserts! (is-none (get-vote proposal-id voter)) ERR_ALREADY_VOTED)
    (map-set votes { proposal-id: proposal-id, voter: voter } { vote: vote-for })
    (if vote-for
        (map-set proposals { proposal-id: proposal-id }
          (merge proposal { votes-for: (+ (get votes-for proposal) stake) }))
        (map-set proposals { proposal-id: proposal-id }
          (merge proposal { votes-against: (+ (get votes-against proposal) stake) })))
    (map-set total-votes { proposal-id: proposal-id } { total: (+ current-total stake) })
    (ok true)
  )
)

(define-public (execute-proposal (proposal-id uint))
  (let
    (
      (proposal (unwrap! (get-proposal proposal-id) ERR_PROPOSAL_NOT_FOUND))
      (total (get-total-votes proposal-id))
      (votes-for (get votes-for proposal))
    )
    (asserts! (<= (get end-block proposal) block-height) ERR_PROPOSAL_ACTIVE)
    (asserts! (not (get executed proposal)) ERR_ALREADY_EXECUTED)
    (asserts! (>= (/ (* votes-for u100) total) (var-get quorum-threshold)) ERR_NO_QUORUM)
    (map-set proposals { proposal-id: proposal-id } (merge proposal { executed: true }))
    (match (get proposal-type proposal)
      "content-approval" (ok true)
      "royalty-rate" (begin
                       (try! (set-royalty-rate (unwrap! (get param proposal) ERR_INVALID_ROYALTY_RATE)))
                       (ok true))
      "platform-upgrade" (ok true)
      "quorum-change" (begin
                        (try! (set-quorum-threshold (unwrap! (get param proposal) ERR_INVALID_THRESHOLD)))
                        (ok true))
      (err ERR_INVALID_PROPOSAL_TYPE))
  )
)

(define-public (get-proposal-count)
  (ok (var-get proposal-count))
)

(define-public (is-member (who principal))
  (ok (> (get-member-stake who) u0))
)