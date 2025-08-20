;; ------------------------------------------------------------
;; Decentralized Insurance Pool
;; ------------------------------------------------------------
;; - Members deposit STX into a pooled treasury.
;; - Users can purchase coverage (premium paid to pool).
;; - Users file claims tied to their coverage.
;; - Pool members vote; simple majority approves.
;; - If approved and pool has funds, payout is sent to claimant.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; Errors / Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(define-constant ERR-NOT-MEMBER (err u100))
(define-constant ERR-ALREADY-MEMBER (err u101))
(define-constant ERR-INSUFFICIENT-FUNDS (err u102))
(define-constant ERR-NO-COVERAGE (err u103))
(define-constant ERR-CLAIM-NOT-FOUND (err u104))
(define-constant ERR-CLAIM-ALREADY-RESOLVED (err u105))
(define-constant ERR-ALREADY-VOTED (err u106))
(define-constant ERR-BAD-PAYOUT (err u107))
(define-constant ERR-BAD-AMOUNT (err u108))
(define-constant ERR-COVERAGE-EXISTS (err u109))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; State
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(define-data-var contract-owner principal tx-sender)
(define-data-var pool-balance uint u0)
(define-data-var next-claim-id uint u1)
(define-data-var next-coverage-id uint u1)

(define-map members
    principal
    { contribution: uint }
)

(define-map coverages
    {
        user: principal,
        coverage-id: uint,
    }
    {
        premium: uint,
        payout: uint,
        active: bool,
    }
)

(define-map claims
    uint
    {
        user: principal,
        coverage-id: uint,
        description: (string-ascii 200),
        votes-for: uint,
        votes-against: uint,
        resolved: bool,
        approved: bool,
    }
)

(define-map votes
    {
        claim-id: uint,
        voter: principal,
    }
    bool
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; Event Logging (using print statements)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(define-private (log-member-joined
        (user principal)
        (amount uint)
    )
    (print {
        event: "member-joined",
        user: user,
        amount: amount,
    })
)

(define-private (log-coverage-purchased
        (user principal)
        (coverage-id uint)
        (premium uint)
        (payout uint)
    )
    (print {
        event: "coverage-purchased",
        user: user,
        coverage-id: coverage-id,
        premium: premium,
        payout: payout,
    })
)

(define-private (log-claim-filed
        (user principal)
        (claim-id uint)
    )
    (print {
        event: "claim-filed",
        user: user,
        claim-id: claim-id,
    })
)

(define-private (log-claim-voted
        (voter principal)
        (claim-id uint)
        (vote bool)
    )
    (print {
        event: "claim-voted",
        voter: voter,
        claim-id: claim-id,
        vote: vote,
    })
)

(define-private (log-claim-resolved
        (claim-id uint)
        (approved bool)
        (payout uint)
    )
    (print {
        event: "claim-resolved",
        claim-id: claim-id,
        approved: approved,
        payout: payout,
    })
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(define-private (is-member (who principal))
    (is-some (map-get? members who))
)

(define-private (only-member)
    (if (is-member tx-sender)
        (ok true)
        ERR-NOT-MEMBER
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; Public: Pool joining / coverage purchase
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(define-public (join-pool (amount uint))
    (begin
        (asserts! (> amount u0) ERR-BAD-AMOUNT)
        (asserts! (is-none (map-get? members tx-sender)) ERR-ALREADY-MEMBER)
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (map-set members tx-sender { contribution: amount })
        (var-set pool-balance (+ (var-get pool-balance) amount))
        (log-member-joined tx-sender amount)
        (ok true)
    )
)

(define-public (purchase-coverage
        (premium uint)
        (payout uint)
    )
    (let ((coverage-id (var-get next-coverage-id)))
        (begin
            (asserts! (and (> premium u0) (> payout u0)) ERR-BAD-AMOUNT)
            (asserts!
                (is-none (map-get? coverages {
                    user: tx-sender,
                    coverage-id: coverage-id,
                }))
                ERR-COVERAGE-EXISTS
            )
            (try! (stx-transfer? premium tx-sender (as-contract tx-sender)))
            (map-set coverages {
                user: tx-sender,
                coverage-id: coverage-id,
            } {
                premium: premium,
                payout: payout,
                active: true,
            })
            (var-set pool-balance (+ (var-get pool-balance) premium))
            (var-set next-coverage-id (+ coverage-id u1))
            (log-coverage-purchased tx-sender coverage-id premium payout)
            (ok coverage-id)
        )
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; Public: Claim lifecycle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(define-public (file-claim
        (coverage-id uint)
        (description (string-ascii 200))
    )
    (match (map-get? coverages {
        user: tx-sender,
        coverage-id: coverage-id,
    })
        coverage (if (get active coverage)
            (let ((claim-id (var-get next-claim-id)))
                (map-set claims claim-id {
                    user: tx-sender,
                    coverage-id: coverage-id,
                    description: description,
                    votes-for: u0,
                    votes-against: u0,
                    resolved: false,
                    approved: false,
                })
                (var-set next-claim-id (+ claim-id u1))
                (log-claim-filed tx-sender claim-id)
                (ok claim-id)
            )
            ERR-NO-COVERAGE
        )
        ERR-NO-COVERAGE
    )
)

(define-public (vote-claim
        (claim-id uint)
        (approve bool)
    )
    (begin
        (try! (only-member))
        (match (map-get? claims claim-id)
            claim (if (get resolved claim)
                ERR-CLAIM-ALREADY-RESOLVED
                (if (is-some (map-get? votes {
                        claim-id: claim-id,
                        voter: tx-sender,
                    }))
                    ERR-ALREADY-VOTED
                    (begin
                        (map-set votes {
                            claim-id: claim-id,
                            voter: tx-sender,
                        }
                            true
                        )
                        (map-set claims claim-id {
                            user: (get user claim),
                            coverage-id: (get coverage-id claim),
                            description: (get description claim),
                            votes-for: (if approve
                                (+ (get votes-for claim) u1)
                                (get votes-for claim)
                            ),
                            votes-against: (if (not approve)
                                (+ (get votes-against claim) u1)
                                (get votes-against claim)
                            ),
                            resolved: false,
                            approved: false,
                        })
                        (log-claim-voted tx-sender claim-id approve)
                        (ok true)
                    )
                )
            )
            ERR-CLAIM-NOT-FOUND
        )
    )
)

(define-public (resolve-claim (claim-id uint))
    (match (map-get? claims claim-id)
        claim (if (get resolved claim)
            ERR-CLAIM-ALREADY-RESOLVED
            (let ((approved (> (get votes-for claim) (get votes-against claim))))
                (match (map-get? coverages {
                    user: (get user claim),
                    coverage-id: (get coverage-id claim),
                })
                    coverage (let ((payout (get payout coverage)))
                        (asserts! (> payout u0) ERR-BAD-PAYOUT)
                        (if approved
                            (begin
                                (asserts! (>= (var-get pool-balance) payout)
                                    ERR-INSUFFICIENT-FUNDS
                                )
                                (try! (as-contract (stx-transfer? payout tx-sender (get user claim))))
                                (var-set pool-balance
                                    (- (var-get pool-balance) payout)
                                )
                                (map-set claims claim-id {
                                    user: (get user claim),
                                    coverage-id: (get coverage-id claim),
                                    description: (get description claim),
                                    votes-for: (get votes-for claim),
                                    votes-against: (get votes-against claim),
                                    resolved: true,
                                    approved: true,
                                })
                                (log-claim-resolved claim-id true payout)
                                (ok true)
                            )
                            (begin
                                (map-set claims claim-id {
                                    user: (get user claim),
                                    coverage-id: (get coverage-id claim),
                                    description: (get description claim),
                                    votes-for: (get votes-for claim),
                                    votes-against: (get votes-against claim),
                                    resolved: true,
                                    approved: false,
                                })
                                (log-claim-resolved claim-id false u0)
                                (ok false)
                            )
                        )
                    )
                    ERR-NO-COVERAGE
                )
            )
        )
        ERR-CLAIM-NOT-FOUND
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; Administrative functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(define-public (deactivate-coverage (coverage-id uint))
    (match (map-get? coverages {
        user: tx-sender,
        coverage-id: coverage-id,
    })
        coverage (begin
            (map-set coverages {
                user: tx-sender,
                coverage-id: coverage-id,
            }
                (merge coverage { active: false })
            )
            (ok true)
        )
        ERR-NO-COVERAGE
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; Read-only views
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(define-read-only (get-pool-balance)
    (var-get pool-balance)
)

(define-read-only (get-member (user principal))
    (map-get? members user)
)

(define-read-only (get-coverage
        (user principal)
        (coverage-id uint)
    )
    (map-get? coverages {
        user: user,
        coverage-id: coverage-id,
    })
)

(define-read-only (get-claim (claim-id uint))
    (map-get? claims claim-id)
)

(define-read-only (get-vote
        (claim-id uint)
        (voter principal)
    )
    (map-get? votes {
        claim-id: claim-id,
        voter: voter,
    })
)

(define-read-only (get-next-coverage-id)
    (var-get next-coverage-id)
)

(define-read-only (get-next-claim-id)
    (var-get next-claim-id)
)

(define-read-only (is-pool-member (user principal))
    (is-member user)
)
