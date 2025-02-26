(impl-trait .sip-010-trait.sip-010-trait)

;; Constants
(define-constant TOKEN-NAME "CO2Token")
(define-constant TOKEN-SYMBOL "CO2T")
(define-constant TOKEN-DECIMALS u6)

;; Error Codes
(define-constant ERR-NOT-AUTHORIZED (err u100))

;; Data Variables
(define-data-var contract-owner principal tx-sender)
(define-data-var total-supply uint u0)
(define-map balances principal uint)

;; Authorization function
(define-private (is-admin (caller principal))
    (is-eq caller (var-get contract-owner)))

;; Read-only functions
(define-read-only (get-name) (ok TOKEN-NAME))
(define-read-only (get-symbol) (ok TOKEN-SYMBOL))
(define-read-only (get-decimals) (ok TOKEN-DECIMALS))
(define-read-only (get-total-supply) (ok (var-get total-supply)))
(define-read-only (get-balance (account principal))
    (ok (default-to u0 (map-get? balances account))))
;; Error Constants
(define-constant ERR-INVALID-VALUE (err u101))
(define-constant ERR-INSUFFICIENT-BALANCE (err u102))

;; Transfer function with optimized balance checking
(define-public (transfer (amount uint) (sender principal) (recipient principal) (memo (optional (buff 34))))
    (let ((sender-balance (default-to u0 (map-get? balances sender))))
        (begin
            (asserts! (is-eq tx-sender sender) ERR-NOT-AUTHORIZED)
            (asserts! (>= sender-balance amount) ERR-INSUFFICIENT-BALANCE)
            (map-set balances sender (- sender-balance amount))
            (map-set balances recipient (+ (default-to u0 (map-get? balances recipient)) amount))
            (ok true))))

;; Mint tokens dynamically
(define-private (mint-tokens (amount uint))
    (var-set total-supply (+ (var-get total-supply) amount))
    (map-set balances (var-get contract-owner) 
        (+ (default-to u0 (map-get? balances (var-get contract-owner))) amount))
    (ok amount))

;; Burn tokens to reduce supply
(define-private (burn-tokens (amount uint))
    (let ((owner-balance (default-to u0 (map-get? balances (var-get contract-owner)))))
        (asserts! (>= owner-balance amount) ERR-INSUFFICIENT-BALANCE)
        (var-set total-supply (- (var-get total-supply) amount))
        (map-set balances (var-get contract-owner) (- owner-balance amount))
        (ok amount)))
