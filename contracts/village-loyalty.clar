;; Village Community Loyalty Program
;; Tracks user engagement and rewards community participation with loyalty points and tier benefits

;; Error constants
(define-constant err-not-authorized (err u200))
(define-constant err-insufficient-points (err u201))
(define-constant err-invalid-amount (err u202))
(define-constant err-tier-not-found (err u203))
(define-constant err-already-claimed-today (err u204))

;; Contract owner
(define-constant contract-owner tx-sender)

;; Loyalty tier definitions
(define-constant tier-bronze u0)
(define-constant tier-silver u1)
(define-constant tier-gold u2)

;; Point values for different activities
(define-constant points-item-listed u10)
(define-constant points-item-purchased u15)
(define-constant points-item-sold u20)
(define-constant points-subscription-created u25)
(define-constant points-daily-login u5)

;; Data variables
(define-data-var total-loyalty-points-awarded uint u0)
(define-data-var loyalty-enabled bool true)

;; Maps
(define-map loyalty-profiles principal {
    total-points: uint,
    available-points: uint,
    current-tier: uint,
    total-activities: uint,
    last-activity-block: uint,
    daily-streak: uint,
    last-daily-claim: uint
})

(define-map loyalty-tier-requirements uint {
    min-points: uint,
    min-activities: uint,
    tier-name: (string-ascii 20),
    discount-rate: uint,
    daily-bonus-multiplier: uint
})

(define-map daily-activity-tracker {user: principal, day: uint} bool)

;; Initialize loyalty tiers
(map-set loyalty-tier-requirements tier-bronze {
    min-points: u0,
    min-activities: u0,
    tier-name: "Bronze",
    discount-rate: u0,
    daily-bonus-multiplier: u100
})

(map-set loyalty-tier-requirements tier-silver {
    min-points: u200,
    min-activities: u10,
    tier-name: "Silver", 
    discount-rate: u250,
    daily-bonus-multiplier: u150
})

(map-set loyalty-tier-requirements tier-gold {
    min-points: u500,
    min-activities: u25,
    tier-name: "Gold",
    discount-rate: u500,
    daily-bonus-multiplier: u200
})

;; Read-only functions
(define-read-only (get-loyalty-profile (user principal))
    (default-to {
        total-points: u0,
        available-points: u0,
        current-tier: tier-bronze,
        total-activities: u0,
        last-activity-block: u0,
        daily-streak: u0,
        last-daily-claim: u0
    } (map-get? loyalty-profiles user))
)

(define-read-only (get-tier-requirements (tier-id uint))
    (map-get? loyalty-tier-requirements tier-id)
)

(define-read-only (get-user-discount-rate (user principal))
    (let ((profile (get-loyalty-profile user))
          (tier-id (get current-tier profile)))
        (match (map-get? loyalty-tier-requirements tier-id)
            tier-data (get discount-rate tier-data)
            u0
        )
    )
)

(define-read-only (get-total-loyalty-points-awarded)
    (var-get total-loyalty-points-awarded)
)

(define-read-only (calculate-daily-streak-bonus (user principal))
    (let ((profile (get-loyalty-profile user))
          (tier-id (get current-tier profile))
          (streak (get daily-streak profile))
          (capped-streak (if (> streak u7) u7 streak)))
        (match (map-get? loyalty-tier-requirements tier-id)
            tier-data (* (get daily-bonus-multiplier tier-data) capped-streak)
            u0
        )
    )
)

;; Public functions
(define-public (award-activity-points (user principal) (activity-type (string-ascii 20)) (base-points uint))
    (begin
        (asserts! (var-get loyalty-enabled) err-not-authorized)
        (asserts! (> base-points u0) err-invalid-amount)
        
        (let ((current-profile (get-loyalty-profile user))
              (bonus-points (calculate-daily-streak-bonus user))
              (total-new-points (+ base-points bonus-points)))
            
            (map-set loyalty-profiles user {
                total-points: (+ (get total-points current-profile) total-new-points),
                available-points: (+ (get available-points current-profile) total-new-points),
                current-tier: (get current-tier current-profile),
                total-activities: (+ (get total-activities current-profile) u1),
                last-activity-block: stacks-block-height,
                daily-streak: (get daily-streak current-profile),
                last-daily-claim: (get last-daily-claim current-profile)
            })
            
            (var-set total-loyalty-points-awarded (+ (var-get total-loyalty-points-awarded) total-new-points))
            (unwrap-panic (check-tier-upgrade user))
            
            (print {
                type: "loyalty-points-awarded",
                user: user,
                activity: activity-type,
                base-points: base-points,
                bonus-points: bonus-points,
                total-awarded: total-new-points
            })
            (ok total-new-points)
        )
    )
)

(define-public (claim-daily-bonus)
    (let ((current-day (/ stacks-block-height u144))
          (profile (get-loyalty-profile tx-sender))
          (last-claim-day (/ (get last-daily-claim profile) u144)))
        
        (asserts! (> current-day last-claim-day) err-already-claimed-today)
        (asserts! (var-get loyalty-enabled) err-not-authorized)
        
        (let ((streak-increment (if (is-eq (+ last-claim-day u1) current-day) u1 u0))
              (new-streak (if (is-eq (+ last-claim-day u1) current-day) 
                             (+ (get daily-streak profile) u1) 
                             u1))
              (bonus-points (calculate-daily-streak-bonus tx-sender)))
            
            (map-set daily-activity-tracker {user: tx-sender, day: current-day} true)
            (map-set loyalty-profiles tx-sender (merge profile {
                daily-streak: new-streak,
                last-daily-claim: stacks-block-height,
                available-points: (+ (get available-points profile) points-daily-login),
                total-points: (+ (get total-points profile) points-daily-login)
            }))
            
            (var-set total-loyalty-points-awarded (+ (var-get total-loyalty-points-awarded) points-daily-login))
            (unwrap-panic (check-tier-upgrade tx-sender))
            
            (print {
                type: "daily-bonus-claimed",
                user: tx-sender,
                streak: new-streak,
                points-awarded: points-daily-login
            })
            (ok points-daily-login)
        )
    )
)

(define-public (redeem-points-for-tokens (points-to-redeem uint))
    (let ((profile (get-loyalty-profile tx-sender))
          (token-equivalent (/ points-to-redeem u10))) ;; 10 points = 1 token
        
        (asserts! (>= (get available-points profile) points-to-redeem) err-insufficient-points)
        (asserts! (> points-to-redeem u0) err-invalid-amount)
        (asserts! (var-get loyalty-enabled) err-not-authorized)
        
        (map-set loyalty-profiles tx-sender (merge profile {
            available-points: (- (get available-points profile) points-to-redeem)
        }))
        
        (print {
            type: "loyalty-points-redeemed",
            user: tx-sender,
            points-redeemed: points-to-redeem,
            tokens-equivalent: token-equivalent
        })
        (ok token-equivalent)
    )
)

(define-private (check-tier-upgrade (user principal))
    (let ((profile (get-loyalty-profile user))
          (current-tier (get current-tier profile))
          (total-points (get total-points profile))
          (total-activities (get total-activities profile)))
        
        (let ((new-tier (if (and (>= total-points u500) (>= total-activities u25))
                           tier-gold
                           (if (and (>= total-points u200) (>= total-activities u10))
                               tier-silver
                               tier-bronze))))
            
            (if (> new-tier current-tier)
                (begin
                    (map-set loyalty-profiles user (merge profile {current-tier: new-tier}))
                    (print {
                        type: "tier-upgraded",
                        user: user,
                        old-tier: current-tier,
                        new-tier: new-tier
                    })
                    (ok true)
                )
                (ok false)
            )
        )
    )
)

;; Admin functions
(define-public (toggle-loyalty-program)
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (var-set loyalty-enabled (not (var-get loyalty-enabled)))
        (ok (var-get loyalty-enabled))
    )
)

;; Helper functions for main contract integration
(define-public (on-item-listed (user principal))
    (award-activity-points user "item-listed" points-item-listed)
)

(define-public (on-item-purchased (user principal))
    (award-activity-points user "item-purchased" points-item-purchased)
)

(define-public (on-item-sold (user principal))
    (award-activity-points user "item-sold" points-item-sold)
)

(define-public (on-subscription-created (user principal))
    (award-activity-points user "subscription-created" points-subscription-created)
)
