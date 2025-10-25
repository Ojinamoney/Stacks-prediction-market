;; prediction-market
;; A decentralized prediction market contract allowing users to create markets,
;; trade shares on outcomes, provide liquidity, and resolve markets via oracles

;; constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-params (err u103))
(define-constant err-insufficient-funds (err u104))
(define-constant err-market-closed (err u105))
(define-constant err-market-not-resolved (err u106))
(define-constant err-market-already-resolved (err u107))
(define-constant err-dispute-period-active (err u108))
(define-constant err-dispute-period-expired (err u109))
(define-constant err-already-claimed (err u110))
(define-constant err-no-winnings (err u111))
(define-constant err-market-not-finalized (err u112))

;; data maps and vars
(define-data-var contract-initialized bool false)
(define-data-var next-market-id uint u1)
(define-data-var protocol-fee-percentage uint u100) ;; 1% default
(define-data-var min-dispute-stake uint u1000000) ;; 1 STX default
(define-data-var default-dispute-period-length uint u144) ;; ~24 hours in blocks

;; Oracle management
(define-map oracles principal { reputation: uint, is-active: bool })
(define-data-var oracle-list (list 100 principal) (list))

;; Market data structure
(define-map markets uint {
    creator: principal,
    description: (string-utf8 500),
    category: (string-ascii 50),
    outcomes: (list 10 (string-utf8 100)),
    resolution-time: uint,
    closing-time: uint,
    fee-percentage: uint,
    oracle: principal,
    oracle-fee: uint,
    min-trade-amount: uint,
    additional-data: (optional (string-utf8 1000)),
    status: (string-ascii 20), ;; "active", "closed", "resolved", "disputed", "finalized"
    resolved-outcome: (optional uint),
    resolution-block: (optional uint),
    total-liquidity: uint,
    outcome-reserves: (list 10 uint),
    dispute-deadline: (optional uint),
    disputed-outcome: (optional uint),
    dispute-stake: uint
})

;; Liquidity positions
(define-map liquidity-positions { market-id: uint, provider: principal } {
    shares: uint,
    share-percentage: uint
})

;; User positions for each outcome
(define-map user-positions { market-id: uint, user: principal, outcome: uint } {
    shares: uint,
    claimed: bool
})

;; Dispute stakes 
(define-map dispute-stakes { market-id: uint, disputer: principal } uint)

;; private functions

;; Check if caller is contract owner
(define-private (is-owner)
    (is-eq tx-sender CONTRACT-OWNER))

;; Check if oracle is authorized and active
(define-private (is-authorized-oracle (oracle principal))
    (match (map-get? oracles oracle)
        oracle-data (get is-active oracle-data)
        false))

;; public functions

;; Initialize the contract with a list of oracles
(define-public (initialize (initial-oracles (list 100 principal)))
    (begin
        (asserts! (is-owner) err-owner-only)
        (asserts! (not (var-get contract-initialized)) (err u113))
        
        ;; Add initial oracles
        (map add-oracle-internal initial-oracles)
        (let ((oracle-list-result (fold append-oracle-to-list initial-oracles (list))))
            (var-set oracle-list oracle-list-result))
        (var-set contract-initialized true)
        (ok true)))

;; Helper function for folding oracles into list
(define-private (append-oracle-to-list (oracle principal) (acc (list 100 principal)))
    (unwrap-panic (as-max-len? (append acc oracle) u100)))

;; Internal function to add oracle
(define-private (add-oracle-internal (oracle principal))
    (map-set oracles oracle { reputation: u100, is-active: true }))

;; Add a new oracle (owner only)
(define-public (add-oracle (oracle principal))
    (begin
        (asserts! (is-owner) err-owner-only)
        (map-set oracles oracle { reputation: u100, is-active: true })
        (let ((current-list (var-get oracle-list)))
            (match (as-max-len? (append current-list oracle) u100)
                new-list (begin
                    (var-set oracle-list new-list)
                    (ok true))
                err-invalid-params))))

;; Update oracle reputation (owner only)
(define-public (update-oracle-reputation (oracle principal) (reputation uint))
    (begin
        (asserts! (is-owner) err-owner-only)
        (asserts! (<= reputation u100) err-invalid-params)
        (match (map-get? oracles oracle)
            oracle-data (begin
                (map-set oracles oracle { reputation: reputation, is-active: (get is-active oracle-data) })
                (ok true))
            err-not-found)))

;; Governance functions
(define-public (set-protocol-fee-percentage (fee-percentage uint))
    (begin
        (asserts! (is-owner) err-owner-only)
        (asserts! (<= fee-percentage u1000) err-invalid-params) ;; Max 10%
        (var-set protocol-fee-percentage fee-percentage)
        (ok true)))

(define-public (set-min-dispute-stake (amount uint))
    (begin
        (asserts! (is-owner) err-owner-only)
        (asserts! (> amount u0) err-invalid-params)
        (var-set min-dispute-stake amount)
        (ok true)))

(define-public (set-default-dispute-period-length (blocks uint))
    (begin
        (asserts! (is-owner) err-owner-only)
        (asserts! (> blocks u0) err-invalid-params)
        (var-set default-dispute-period-length blocks)
        (ok true)))

;; Create a new prediction market
(define-public (create-market 
    (description (string-utf8 500))
    (category (string-ascii 50))
    (outcomes (list 10 (string-utf8 100)))
    (resolution-time uint)
    (closing-time uint)
    (fee-percentage uint)
    (oracle principal)
    (oracle-fee uint)
    (min-trade-amount uint)
    (additional-data (optional (string-utf8 1000))))
    (let ((market-id (var-get next-market-id)))
        (begin
            (asserts! (var-get contract-initialized) err-unauthorized)
            (asserts! (> resolution-time block-height) err-invalid-params)
            (asserts! (> resolution-time closing-time) err-invalid-params)
            (asserts! (> closing-time block-height) err-invalid-params)
            (asserts! (<= fee-percentage u1000) err-invalid-params) ;; Max 10%
            (asserts! (is-authorized-oracle oracle) err-unauthorized)
            (asserts! (> (len outcomes) u1) err-invalid-params)
            (asserts! (<= (len outcomes) u10) err-invalid-params)
            
            ;; Create initial reserves (all zeros)
            (let ((zero-reserves (list u0 u0 u0 u0 u0 u0 u0 u0 u0 u0)))
                (map-set markets market-id {
                    creator: tx-sender,
                    description: description,
                    category: category,
                    outcomes: outcomes,
                    resolution-time: resolution-time,
                    closing-time: closing-time,
                    fee-percentage: fee-percentage,
                    oracle: oracle,
                    oracle-fee: oracle-fee,
                    min-trade-amount: min-trade-amount,
                    additional-data: additional-data,
                    status: "active",
                    resolved-outcome: none,
                    resolution-block: none,
                    total-liquidity: u0,
                    outcome-reserves: zero-reserves,
                    dispute-deadline: none,
                    disputed-outcome: none,
                    dispute-stake: u0
                }))
            
            (var-set next-market-id (+ market-id u1))
            (ok market-id))))

;; Add liquidity to a market
(define-public (add-liquidity (market-id uint) (amount uint))
    (let ((market (unwrap! (map-get? markets market-id) err-not-found)))
        (begin
            (asserts! (is-eq (get status market) "active") err-market-closed)
            (asserts! (> amount u0) err-invalid-params)
            (asserts! (>= (stx-get-balance tx-sender) amount) err-insufficient-funds)
            
            ;; Transfer STX to contract
            (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
            
            ;; Calculate share percentage
            (let ((total-liquidity (get total-liquidity market))
                  (share-percentage (if (is-eq total-liquidity u0)
                                        u10000 ;; 100% in basis points
                                        (/ (* amount u10000) (+ total-liquidity amount)))))
                
                ;; Update market liquidity
                (map-set markets market-id (merge market { 
                    total-liquidity: (+ total-liquidity amount) 
                }))
                
                ;; Update user's liquidity position
                (map-set liquidity-positions { market-id: market-id, provider: tx-sender } {
                    shares: amount,
                    share-percentage: share-percentage
                })
                
                (ok { share-percentage: share-percentage })))))

;; Private helper functions for calculations
(define-private (calculate-price-impact (reserves uint) (amount uint))
    ;; Simple constant product formula approximation
    (if (is-eq reserves u0)
        amount ;; Initial pricing
        (/ (* amount u10000) (+ reserves amount))))

;; Get market price for an outcome
(define-read-only (get-outcome-price (market-id uint) (outcome-id uint))
    (match (map-get? markets market-id)
        market (let ((reserves (get outcome-reserves market))
                     (total-liquidity (get total-liquidity market)))
                (if (is-eq total-liquidity u0)
                    (ok u5000) ;; 50% default price
                    (match (element-at reserves outcome-id)
                        reserve (ok (/ (* reserve u10000) total-liquidity))
                        err-not-found)))
        err-not-found))

;; Buy shares in a market outcome
(define-public (buy-shares (market-id uint) (outcome-id uint) (amount uint))
    (let ((market (unwrap! (map-get? markets market-id) err-not-found)))
        (begin
            (asserts! (is-eq (get status market) "active") err-market-closed)
            (asserts! (< block-height (get closing-time market)) err-market-closed)
            (asserts! (>= amount (get min-trade-amount market)) err-invalid-params)
            (asserts! (>= (stx-get-balance tx-sender) amount) err-insufficient-funds)
            (asserts! (< outcome-id (len (get outcomes market))) err-invalid-params)
            
            ;; Transfer STX to contract
            (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
            
            ;; Calculate shares based on current price
            (let ((reserves (get outcome-reserves market))
                  (total-liquidity (get total-liquidity market))
                  (shares-received (calculate-shares-for-amount amount total-liquidity outcome-id reserves)))
                
                ;; Update user position
                (let ((current-position (default-to { shares: u0, claimed: false }
                                                   (map-get? user-positions { market-id: market-id, user: tx-sender, outcome: outcome-id }))))
                    (map-set user-positions { market-id: market-id, user: tx-sender, outcome: outcome-id } {
                        shares: (+ (get shares current-position) shares-received),
                        claimed: false
                    }))
                
                ;; Update market reserves
                (let ((updated-reserves (update-reserves-for-buy reserves outcome-id amount)))
                    (map-set markets market-id (merge market { 
                        outcome-reserves: updated-reserves,
                        total-liquidity: (+ total-liquidity amount)
                    })))
                
                (ok { shares: shares-received, cost: amount })))))

;; Sell shares in a market outcome
(define-public (sell-shares (market-id uint) (outcome-id uint) (shares uint))
    (let ((market (unwrap! (map-get? markets market-id) err-not-found))
          (user-position (unwrap! (map-get? user-positions { market-id: market-id, user: tx-sender, outcome: outcome-id }) err-not-found)))
        (begin
            (asserts! (is-eq (get status market) "active") err-market-closed)
            (asserts! (< block-height (get closing-time market)) err-market-closed)
            (asserts! (> shares u0) err-invalid-params)
            (asserts! (>= (get shares user-position) shares) err-insufficient-funds)
            
            ;; Calculate payout based on current reserves
            (let ((reserves (get outcome-reserves market))
                  (total-liquidity (get total-liquidity market))
                  (payout (calculate-payout-for-shares shares total-liquidity outcome-id reserves)))
                
                ;; Update user position
                (map-set user-positions { market-id: market-id, user: tx-sender, outcome: outcome-id } {
                    shares: (- (get shares user-position) shares),
                    claimed: false
                })
                
                ;; Update market reserves
                (let ((updated-reserves (update-reserves-for-sell reserves outcome-id payout)))
                    (map-set markets market-id (merge market { 
                        outcome-reserves: updated-reserves,
                        total-liquidity: (- total-liquidity payout)
                    })))
                
                ;; Transfer STX back to user
                (try! (as-contract (stx-transfer? payout tx-sender tx-sender)))
                
                (ok { shares: shares, payout: payout })))))

;; Helper functions for trading calculations
(define-private (calculate-shares-for-amount (amount uint) (total-liquidity uint) (outcome-id uint) (reserves (list 10 uint)))
    ;; Simplified calculation - in production, use more sophisticated AMM formula
    (if (is-eq total-liquidity u0)
        amount ;; Initial shares = amount
        (let ((current-reserve (unwrap-panic (element-at reserves outcome-id))))
            (if (is-eq current-reserve u0)
                amount
                (/ (* amount current-reserve) (+ current-reserve amount))))))

(define-private (calculate-payout-for-shares (shares uint) (total-liquidity uint) (outcome-id uint) (reserves (list 10 uint)))
    ;; Simplified calculation
    (let ((current-reserve (unwrap-panic (element-at reserves outcome-id))))
        (if (is-eq current-reserve u0)
            u0
            (/ (* shares current-reserve) (+ current-reserve shares)))))

(define-private (update-reserves-for-buy (reserves (list 10 uint)) (outcome-id uint) (amount uint))
    ;; Add amount to the specific outcome reserve
    (map update-reserve-at-index 
         (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9)
         reserves
         (list outcome-id outcome-id outcome-id outcome-id outcome-id outcome-id outcome-id outcome-id outcome-id outcome-id)
         (list amount amount amount amount amount amount amount amount amount amount)))

(define-private (update-reserves-for-sell (reserves (list 10 uint)) (outcome-id uint) (amount uint))
    ;; Subtract amount from the specific outcome reserve
    (map subtract-reserve-at-index 
         (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9)
         reserves
         (list outcome-id outcome-id outcome-id outcome-id outcome-id outcome-id outcome-id outcome-id outcome-id outcome-id)
         (list amount amount amount amount amount amount amount amount amount amount)))

(define-private (update-reserve-at-index (index uint) (current-reserve uint) (target-outcome uint) (amount uint))
    (if (is-eq index target-outcome)
        (+ current-reserve amount)
        current-reserve))

(define-private (subtract-reserve-at-index (index uint) (current-reserve uint) (target-outcome uint) (amount uint))
    (if (is-eq index target-outcome)
        (if (>= current-reserve amount)
            (- current-reserve amount)
            u0)
        current-reserve))

;; Oracle resolves market
(define-public (resolve-market (market-id uint) (outcome-id uint))
    (let ((market (unwrap! (map-get? markets market-id) err-not-found)))
        (begin
            (asserts! (is-eq (get oracle market) tx-sender) err-unauthorized)
            (asserts! (is-eq (get status market) "active") err-market-already-resolved)
            (asserts! (>= block-height (get closing-time market)) err-invalid-params)
            (asserts! (< outcome-id (len (get outcomes market))) err-invalid-params)
            
            ;; Set dispute deadline
            (let ((dispute-deadline (+ block-height (var-get default-dispute-period-length))))
                (map-set markets market-id (merge market {
                    status: "resolved",
                    resolved-outcome: (some outcome-id),
                    resolution-block: (some block-height),
                    dispute-deadline: (some dispute-deadline)
                })))
            
            (ok true))))

;; Dispute market resolution
(define-public (dispute-resolution (market-id uint) (proposed-outcome uint) (stake-amount uint))
    (let ((market (unwrap! (map-get? markets market-id) err-not-found)))
        (begin
            (asserts! (is-eq (get status market) "resolved") err-market-not-resolved)
            (asserts! (< block-height (unwrap! (get dispute-deadline market) err-dispute-period-expired)) err-dispute-period-expired)
            (asserts! (>= stake-amount (var-get min-dispute-stake)) err-insufficient-funds)
            (asserts! (>= (stx-get-balance tx-sender) stake-amount) err-insufficient-funds)
            (asserts! (< proposed-outcome (len (get outcomes market))) err-invalid-params)
            
            ;; Transfer dispute stake
            (try! (stx-transfer? stake-amount tx-sender (as-contract tx-sender)))
            
            ;; Record dispute
            (map-set dispute-stakes { market-id: market-id, disputer: tx-sender } stake-amount)
            
            ;; Update market status
            (map-set markets market-id (merge market {
                status: "disputed",
                disputed-outcome: (some proposed-outcome),
                dispute-stake: (+ (get dispute-stake market) stake-amount)
            }))
            
            (ok true))))

;; Finalize market after dispute period
(define-public (finalize-market (market-id uint))
    (let ((market (unwrap! (map-get? markets market-id) err-not-found)))
        (begin
            (asserts! (or (is-eq (get status market) "resolved") (is-eq (get status market) "disputed")) err-invalid-params)
            (asserts! (>= block-height (unwrap! (get dispute-deadline market) err-dispute-period-expired)) err-dispute-period-active)
            
            ;; Update market status to finalized
            (map-set markets market-id (merge market { status: "finalized" }))
            
            ;; Pay oracle fee if resolution wasn't disputed successfully
            (if (is-eq (get status market) "resolved")
                (let ((oracle-fee (get oracle-fee market))
                      (oracle (get oracle market)))
                    (and (> oracle-fee u0)
                         (try! (as-contract (stx-transfer? oracle-fee tx-sender oracle)))))
                true)
            
            (ok true))))

;; Claim winnings after market is finalized
(define-public (claim-winnings (market-id uint))
    (let ((market (unwrap! (map-get? markets market-id) err-not-found)))
        (begin
            (asserts! (is-eq (get status market) "finalized") err-market-not-finalized)
            
            (let ((winning-outcome (unwrap! (get resolved-outcome market) err-market-not-resolved))
                  (user-position (map-get? user-positions { market-id: market-id, user: tx-sender, outcome: winning-outcome })))
                
                (match user-position
                    position (begin
                        (asserts! (not (get claimed position)) err-already-claimed)
                        (asserts! (> (get shares position) u0) err-no-winnings)
                        
                        ;; Calculate winnings based on shares and total liquidity
                        (let ((total-liquidity (get total-liquidity market))
                              (winning-shares (get shares position))
                              (winnings (calculate-winnings winning-shares total-liquidity market-id winning-outcome)))
                            
                            ;; Mark as claimed
                            (map-set user-positions { market-id: market-id, user: tx-sender, outcome: winning-outcome } {
                                shares: winning-shares,
                                claimed: true
                            })
                            
                            ;; Transfer winnings
                            (try! (as-contract (stx-transfer? winnings tx-sender tx-sender)))
                            
                            (ok winnings)))
                    err-no-winnings)))))

;; Remove liquidity after market is finalized
(define-public (remove-liquidity (market-id uint))
    (let ((market (unwrap! (map-get? markets market-id) err-not-found))
          (lp-position (unwrap! (map-get? liquidity-positions { market-id: market-id, provider: tx-sender }) err-not-found)))
        (begin
            (asserts! (is-eq (get status market) "finalized") err-market-not-finalized)
            
            ;; Calculate LP share of remaining liquidity
            (let ((share-percentage (get share-percentage lp-position))
                  (remaining-liquidity (get total-liquidity market))
                  (lp-share (/ (* remaining-liquidity share-percentage) u10000)))
                
                ;; Remove LP position
                (map-delete liquidity-positions { market-id: market-id, provider: tx-sender })
                
                ;; Transfer LP share
                (try! (as-contract (stx-transfer? lp-share tx-sender tx-sender)))
                
                (ok lp-share)))))

;; Helper function to calculate winnings
(define-private (calculate-winnings (winning-shares uint) (total-liquidity uint) (market-id uint) (winning-outcome uint))
    ;; Simplified calculation - proportional to shares
    ;; In production, this would account for the AMM reserves and trading fees
    (if (is-eq total-liquidity u0)
        u0
        (let ((total-winning-shares (get-total-winning-shares market-id winning-outcome)))
            (if (is-eq total-winning-shares u0)
                u0
                (/ (* winning-shares total-liquidity) total-winning-shares)))))

(define-private (get-total-winning-shares (market-id uint) (winning-outcome uint))
    ;; This would need to iterate through all positions - simplified for now
    u100000000) ;; Placeholder

;; Read-only functions
(define-read-only (get-market (market-id uint))
    (map-get? markets market-id))

(define-read-only (get-user-position (market-id uint) (user principal) (outcome uint))
    (map-get? user-positions { market-id: market-id, user: user, outcome: outcome }))

(define-read-only (get-liquidity-position (market-id uint) (provider principal))
    (map-get? liquidity-positions { market-id: market-id, provider: provider }))

(define-read-only (get-oracle-info (oracle principal))
    (map-get? oracles oracle))

(define-read-only (get-contract-info)
    {
        initialized: (var-get contract-initialized),
        next-market-id: (var-get next-market-id),
        protocol-fee: (var-get protocol-fee-percentage),
        min-dispute-stake: (var-get min-dispute-stake),
        dispute-period: (var-get default-dispute-period-length)
    })
