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

(define-data-var token-name (string-ascii 32) "Villatoken")
(define-data-var token-symbol (string-ascii 10) "VILLA")
(define-data-var token-decimals uint u6)
(define-data-var total-supply uint u0)
(define-data-var marketplace-enabled bool true)
(define-data-var marketplace-fee-rate uint u250)
(define-data-var next-item-id uint u1)

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

(define-public (initialize-contract)
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (try! (mint u1000000 contract-owner))
        (print {type: "contract-initialized", total-supply: u1000000})
        (ok true)
    )
)
