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
