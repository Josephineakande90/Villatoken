(define-fungible-token villatoken)

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-token-owner (err u101))
(define-constant err-insufficient-balance (err u102))
(define-constant err-item-not-found (err u103))
(define-constant err-item-not-for-sale (err u104))
(define-constant err-insufficient-payment (err u105))
(define-constant err-invalid-amount (err u106))
(define-constant err-self-transfer (err u107))
(define-constant err-marketplace-disabled (err u108))
(define-constant err-invalid-price (err u109))
(define-constant err-item-already-listed (err u110))
(define-constant err-escrow-not-found (err u111))
(define-constant err-escrow-already-exists (err u112))
(define-constant err-invalid-escrow-status (err u113))
(define-constant err-not-escrow-participant (err u114))
(define-constant err-escrow-expired (err u115))
(define-constant err-escrow-not-expired (err u116))
(define-constant err-dispute-timeout (err u117))
(define-constant err-already-confirmed (err u118))

(define-data-var token-name (string-ascii 32) "Villatoken")
(define-data-var token-symbol (string-ascii 10) "VILLA")
(define-data-var token-decimals uint u6)
(define-data-var total-supply uint u0)
(define-data-var marketplace-enabled bool true)
(define-data-var marketplace-fee-rate uint u250)
(define-data-var next-item-id uint u1)
(define-data-var next-escrow-id uint u1)
(define-data-var escrow-timeout-blocks uint u1440)

(define-map token-balances principal uint)
(define-map allowed-minters principal bool)
(define-map marketplace-items uint {
    seller: principal,
    item-name: (string-ascii 50),
    description: (string-ascii 200),
    price: uint,
    category: (string-ascii 20),
    is-active: bool,
    created-at: uint
})
(define-map user-items principal (list 50 uint))
(define-map item-sales uint {
    buyer: principal,
    seller: principal,
    price: uint,
    timestamp: uint
})
(define-map user-reputation principal {
    total-sales: uint,
    total-purchases: uint,
    rating: uint
})
(define-map escrow-agreements uint {
    buyer: principal,
    seller: principal,
    item-id: uint,
    amount: uint,
    status: (string-ascii 20),
    created-at: uint,
    expires-at: uint,
    buyer-confirmed: bool,
    seller-confirmed: bool,
    disputed: bool
})
(define-map user-escrows principal (list 50 uint))

(define-read-only (get-name)
    (ok (var-get token-name))
)

(define-read-only (get-symbol)
    (ok (var-get token-symbol))
)

(define-read-only (get-decimals)
    (ok (var-get token-decimals))
)

(define-read-only (get-balance (who principal))
    (ok (default-to u0 (map-get? token-balances who)))
)

(define-read-only (get-total-supply)
    (ok (var-get total-supply))
)

(define-read-only (get-token-uri)
    (ok none)
)

(define-read-only (get-marketplace-status)
    (var-get marketplace-enabled)
)

(define-read-only (get-marketplace-fee-rate)
    (var-get marketplace-fee-rate)
)

(define-read-only (get-item-details (item-id uint))
    (map-get? marketplace-items item-id)
)

(define-read-only (get-user-items (user principal))
    (default-to (list) (map-get? user-items user))
)

(define-read-only (get-user-reputation (user principal))
    (default-to {total-sales: u0, total-purchases: u0, rating: u0} (map-get? user-reputation user))
)

(define-read-only (get-next-item-id)
    (var-get next-item-id)
)

(define-read-only (get-escrow-details (escrow-id uint))
    (map-get? escrow-agreements escrow-id)
)

(define-read-only (get-user-escrows (user principal))
    (default-to (list) (map-get? user-escrows user))
)

(define-read-only (get-escrow-timeout-blocks)
    (var-get escrow-timeout-blocks)
)

(define-public (transfer (amount uint) (from principal) (to principal) (memo (optional (buff 34))))
    (begin
        (asserts! (> amount u0) err-invalid-amount)
        (asserts! (not (is-eq from to)) err-self-transfer)
        (asserts! (is-eq tx-sender from) err-not-token-owner)
        (asserts! (>= (get-balance-uint from) amount) err-insufficient-balance)
        (unwrap! (update-balance from (- (get-balance-uint from) amount)) 
            err-insufficient-balance)
        (unwrap! (update-balance to (+ (get-balance-uint to) amount))
            err-insufficient-balance)
        (print {type: "transfer", from: from, to: to, amount: amount, memo: memo})
        (ok true)
    )
)

(define-public (mint (amount uint) (to principal))
    (begin
        (asserts! (or (is-eq tx-sender contract-owner) (default-to false (map-get? allowed-minters tx-sender))) err-owner-only)
        (asserts! (> amount u0) err-invalid-amount)
        (unwrap! (update-balance to (+ (get-balance-uint to) amount))
            err-insufficient-balance)
        (var-set total-supply (+ (var-get total-supply) amount))
        (print {type: "mint", to: to, amount: amount})
        (ok true)
    )
)

(define-public (burn (amount uint))
    (begin
        (asserts! (> amount u0) err-invalid-amount)
        (asserts! (>= (get-balance-uint tx-sender) amount) err-insufficient-balance)
        (unwrap! (update-balance tx-sender (- (get-balance-uint tx-sender) amount))
            err-insufficient-balance)
        (var-set total-supply (- (var-get total-supply) amount))
        (print {type: "burn", from: tx-sender, amount: amount})
        (ok true)
    )
)

(define-public (add-minter (minter principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set allowed-minters minter true)
        (ok true)
    )
)

(define-public (remove-minter (minter principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-delete allowed-minters minter)
        (ok true)
    )
)

(define-public (toggle-marketplace)
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set marketplace-enabled (not (var-get marketplace-enabled)))
        (ok (var-get marketplace-enabled))
    )
)

(define-public (set-marketplace-fee (new-fee-rate uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (<= new-fee-rate u1000) err-invalid-amount)
        (var-set marketplace-fee-rate new-fee-rate)
        (ok true)
    )
)

(define-public (list-item (item-name (string-ascii 50)) (description (string-ascii 200)) (price uint) (category (string-ascii 20)))
    (let ((item-id (var-get next-item-id)))
        (asserts! (var-get marketplace-enabled) err-marketplace-disabled)
        (asserts! (> price u0) err-invalid-price)
        (asserts! (> (len item-name) u0) err-invalid-amount)
        (map-set marketplace-items item-id {
            seller: tx-sender,
            item-name: item-name,
            description: description,
            price: price,
            category: category,
            is-active: true,
            created-at: stacks-block-height
        })
        (map-set user-items tx-sender (unwrap-panic (as-max-len? (append (get-user-items tx-sender) item-id) u50)))
        (var-set next-item-id (+ item-id u1))
        (print {type: "item-listed", item-id: item-id, seller: tx-sender, price: price})
        (ok item-id)
    )
)

(define-public (purchase-item (item-id uint))
    (let (
        (item (unwrap! (map-get? marketplace-items item-id) err-item-not-found))
        (seller (get seller item))
        (price (get price item))
        (marketplace-fee (/ (* price (var-get marketplace-fee-rate)) u10000))
        (seller-amount (- price marketplace-fee))
    )
        (asserts! (var-get marketplace-enabled) err-marketplace-disabled)
        (asserts! (get is-active item) err-item-not-for-sale)
        (asserts! (not (is-eq tx-sender seller)) err-self-transfer)
        (asserts! (>= (get-balance-uint tx-sender) price) err-insufficient-payment)

        (unwrap! (update-balance tx-sender (- (get-balance-uint tx-sender) price))
            err-insufficient-balance)
        (unwrap! (update-balance seller (+ (get-balance-uint seller) seller-amount))
            err-insufficient-balance)
        (unwrap! (update-balance contract-owner (+ (get-balance-uint contract-owner) marketplace-fee))
            err-insufficient-balance)

        (map-set marketplace-items item-id (merge item {is-active: false}))
        (map-set item-sales item-id {
            buyer: tx-sender,
            seller: seller,
            price: price,
            timestamp: stacks-block-height
        })
        
        (update-user-reputation seller true)
        (update-user-reputation tx-sender false)
        
        (print {type: "item-purchased", item-id: item-id, buyer: tx-sender, seller: seller, price: price})
        (ok true)
    )
)

(define-public (remove-item-listing (item-id uint))
    (let ((item (unwrap! (map-get? marketplace-items item-id) err-item-not-found)))
        (asserts! (is-eq tx-sender (get seller item)) err-not-token-owner)
        (asserts! (get is-active item) err-item-not-for-sale)
        (map-set marketplace-items item-id (merge item {is-active: false}))
        (print {type: "item-delisted", item-id: item-id, seller: tx-sender})
        (ok true)
    )
)

(define-public (update-item-price (item-id uint) (new-price uint))
    (let ((item (unwrap! (map-get? marketplace-items item-id) err-item-not-found)))
        (asserts! (is-eq tx-sender (get seller item)) err-not-token-owner) 
        (asserts! (get is-active item) err-item-not-for-sale)
        (asserts! (> new-price u0) err-invalid-price)
        (map-set marketplace-items item-id (merge item {price: new-price}))
        (print {type: "price-updated", item-id: item-id, seller: tx-sender, new-price: new-price})
        (ok true)
    )
)

(define-private (get-balance-uint (who principal))
    (default-to u0 (map-get? token-balances who))
)

(define-private (update-balance (who principal) (new-balance uint))
    (begin
        (map-set token-balances who new-balance)
        (ok true)
    )
)

(define-private (update-user-reputation (user principal) (is-seller bool))
    (let ((current-rep (get-user-reputation user)))
        (if is-seller
            (map-set user-reputation user {
                total-sales: (+ (get total-sales current-rep) u1),
                total-purchases: (get total-purchases current-rep),
                rating: (get rating current-rep)
            })
            (map-set user-reputation user {
                total-sales: (get total-sales current-rep),
                total-purchases: (+ (get total-purchases current-rep) u1),
                rating: (get rating current-rep)
            })
        )
    )
)

(define-public (create-escrow (item-id uint))
    (let (
        (item (unwrap! (map-get? marketplace-items item-id) err-item-not-found))
        (seller (get seller item))
        (price (get price item))
        (escrow-id (var-get next-escrow-id))
        (expires-at (+ stacks-block-height (var-get escrow-timeout-blocks)))
    )
        (asserts! (var-get marketplace-enabled) err-marketplace-disabled)
        (asserts! (get is-active item) err-item-not-for-sale)
        (asserts! (not (is-eq tx-sender seller)) err-self-transfer)
        (asserts! (>= (get-balance-uint tx-sender) price) err-insufficient-payment)
        (asserts! (is-none (map-get? escrow-agreements escrow-id)) err-escrow-already-exists)
        
        (unwrap! (update-balance tx-sender (- (get-balance-uint tx-sender) price))
            err-insufficient-balance)
        
        (map-set escrow-agreements escrow-id {
            buyer: tx-sender,
            seller: seller,
            item-id: item-id,
            amount: price,
            status: "active",
            created-at: stacks-block-height,
            expires-at: expires-at,
            buyer-confirmed: false,
            seller-confirmed: false,
            disputed: false
        })
        
        (map-set user-escrows tx-sender (unwrap-panic (as-max-len? (append (get-user-escrows tx-sender) escrow-id) u50)))
        (map-set user-escrows seller (unwrap-panic (as-max-len? (append (get-user-escrows seller) escrow-id) u50)))
        (map-set marketplace-items item-id (merge item {is-active: false}))
        
        (var-set next-escrow-id (+ escrow-id u1))
        (print {type: "escrow-created", escrow-id: escrow-id, buyer: tx-sender, seller: seller, amount: price})
        (ok escrow-id)
    )
)

(define-public (confirm-delivery (escrow-id uint))
    (let ((escrow (unwrap! (map-get? escrow-agreements escrow-id) err-escrow-not-found)))
        (asserts! (is-eq tx-sender (get buyer escrow)) err-not-escrow-participant)
        (asserts! (is-eq (get status escrow) "active") err-invalid-escrow-status)
        (asserts! (not (get buyer-confirmed escrow)) err-already-confirmed)
        (asserts! (<= stacks-block-height (get expires-at escrow)) err-escrow-expired)
        
        (let ((updated-escrow (merge escrow {buyer-confirmed: true})))
            (map-set escrow-agreements escrow-id updated-escrow)
            (if (get seller-confirmed updated-escrow)
                (try! (complete-escrow-transaction escrow-id))
                true
            )
        )
        (print {type: "delivery-confirmed", escrow-id: escrow-id, buyer: tx-sender})
        (ok true)
    )
)

(define-public (confirm-receipt (escrow-id uint))
    (let ((escrow (unwrap! (map-get? escrow-agreements escrow-id) err-escrow-not-found)))
        (asserts! (is-eq tx-sender (get seller escrow)) err-not-escrow-participant)
        (asserts! (is-eq (get status escrow) "active") err-invalid-escrow-status)
        (asserts! (not (get seller-confirmed escrow)) err-already-confirmed)
        (asserts! (<= stacks-block-height (get expires-at escrow)) err-escrow-expired)
        
        (let ((updated-escrow (merge escrow {seller-confirmed: true})))
            (map-set escrow-agreements escrow-id updated-escrow)
            (if (get buyer-confirmed updated-escrow)
                (try! (complete-escrow-transaction escrow-id))
                true
            )
        )
        (print {type: "receipt-confirmed", escrow-id: escrow-id, seller: tx-sender})
        (ok true)
    )
)

(define-public (dispute-escrow (escrow-id uint))
    (let ((escrow (unwrap! (map-get? escrow-agreements escrow-id) err-escrow-not-found)))
        (asserts! (or (is-eq tx-sender (get buyer escrow)) (is-eq tx-sender (get seller escrow))) err-not-escrow-participant)
        (asserts! (is-eq (get status escrow) "active") err-invalid-escrow-status)
        (asserts! (<= stacks-block-height (get expires-at escrow)) err-escrow-expired)
        
        (map-set escrow-agreements escrow-id (merge escrow {disputed: true, status: "disputed"}))
        (print {type: "escrow-disputed", escrow-id: escrow-id, disputer: tx-sender})
        (ok true)
    )
)

(define-public (resolve-dispute (escrow-id uint) (award-to-buyer bool))
    (let ((escrow (unwrap! (map-get? escrow-agreements escrow-id) err-escrow-not-found)))
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (get disputed escrow) err-invalid-escrow-status)
        (asserts! (is-eq (get status escrow) "disputed") err-invalid-escrow-status)
        
        (if award-to-buyer
            (begin
                (unwrap! (update-balance (get buyer escrow) (+ (get-balance-uint (get buyer escrow)) (get amount escrow)))
                    err-insufficient-balance)
                (map-set escrow-agreements escrow-id (merge escrow {status: "refunded"}))
                (print {type: "dispute-resolved", escrow-id: escrow-id, awarded-to: "buyer"})
            )
            (begin
                (try! (complete-escrow-transaction escrow-id))
                (print {type: "dispute-resolved", escrow-id: escrow-id, awarded-to: "seller"})
            )
        )
        (ok true)
    )
)

(define-public (cancel-expired-escrow (escrow-id uint))
    (let ((escrow (unwrap! (map-get? escrow-agreements escrow-id) err-escrow-not-found)))
        (asserts! (> stacks-block-height (get expires-at escrow)) err-escrow-not-expired)
        (asserts! (is-eq (get status escrow) "active") err-invalid-escrow-status)
        (asserts! (not (get disputed escrow)) err-invalid-escrow-status)
        
        (unwrap! (update-balance (get buyer escrow) (+ (get-balance-uint (get buyer escrow)) (get amount escrow)))
            err-insufficient-balance)
        
        (let ((item-id (get item-id escrow)))
            (let ((item (unwrap! (map-get? marketplace-items item-id) err-item-not-found)))
                (map-set marketplace-items item-id (merge item {is-active: true}))
            )
        )
        
        (map-set escrow-agreements escrow-id (merge escrow {status: "expired"}))
        (print {type: "escrow-expired", escrow-id: escrow-id, refunded-to: (get buyer escrow)})
        (ok true)
    )
)

(define-public (set-escrow-timeout (new-timeout uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (> new-timeout u0) err-invalid-amount)
        (var-set escrow-timeout-blocks new-timeout)
        (ok true)
    )
)

(define-private (complete-escrow-transaction (escrow-id uint))
    (let (
        (escrow (unwrap! (map-get? escrow-agreements escrow-id) err-escrow-not-found))
        (seller (get seller escrow))
        (amount (get amount escrow))
        (marketplace-fee (/ (* amount (var-get marketplace-fee-rate)) u10000))
        (seller-amount (- amount marketplace-fee))
    )
        (unwrap! (update-balance seller (+ (get-balance-uint seller) seller-amount))
            err-insufficient-balance)
        (unwrap! (update-balance contract-owner (+ (get-balance-uint contract-owner) marketplace-fee))
            err-insufficient-balance)
        
        (map-set escrow-agreements escrow-id (merge escrow {status: "completed"}))
        (map-set item-sales (get item-id escrow) {
            buyer: (get buyer escrow),
            seller: seller,
            price: amount,
            timestamp: stacks-block-height
        })
        
        (update-user-reputation seller true)
        (update-user-reputation (get buyer escrow) false)
        
        (print {type: "escrow-completed", escrow-id: escrow-id, seller: seller, amount: seller-amount})
        (ok true)
    )
)

(define-public (initialize-contract)
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (try! (mint u1000000 contract-owner))
        (print {type: "contract-initialized", total-supply: u1000000})
        (ok true)
    )
)
