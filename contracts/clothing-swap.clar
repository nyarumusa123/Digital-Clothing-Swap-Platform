;; ========================================
;; DIGITAL CLOTHING SWAP PLATFORM
;; ========================================
;; A comprehensive community-driven fashion exchange system
;; with size matching, swap credits, and reputation rewards

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-params (err u103))
(define-constant err-item-unavailable (err u104))
(define-constant err-insufficient-credits (err u105))
(define-constant err-same-user (err u106))
(define-constant err-invalid-condition (err u107))
(define-constant err-not-eligible (err u108))
(define-constant err-already-claimed (err u109))
(define-constant err-invalid-tier (err u110))

;; Data Variables
(define-data-var next-item-id uint u1)
(define-data-var next-swap-id uint u1)
(define-data-var platform-fee-percent uint u5) ;; 5% platform fee
(define-data-var min-condition-score uint u3) ;; Minimum condition score (1-5 scale)

;; Item condition constants
(define-constant condition-poor u1)
(define-constant condition-fair u2)
(define-constant condition-good u3)
(define-constant condition-very-good u4)
(define-constant condition-excellent u5)

;; Reward tier thresholds
(define-constant bronze-threshold u5)   ;; 5 successful swaps
(define-constant silver-threshold u20)  ;; 20 successful swaps
(define-constant gold-threshold u50)    ;; 50 successful swaps

;; Data Maps - Core Platform
(define-map items uint {
    owner: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    category: (string-ascii 50),
    size: (string-ascii 10),
    condition: uint,
    credit-value: uint,
    available: bool,
    created-at: uint,
    image-hash: (optional (string-ascii 64))
})

(define-map user-credits principal uint)
(define-map user-stats principal {
    items-posted: uint,
    successful-swaps: uint,
    reputation-score: uint
})

(define-map swap-history uint {
    item-id: uint,
    from-user: principal,
    to-user: principal,
    credits-paid: uint,
    timestamp: uint
})

(define-map size-preferences principal {
    tops: (list 5 (string-ascii 10)),
    bottoms: (list 5 (string-ascii 10)),
    shoes: (string-ascii 10),
    dresses: (list 5 (string-ascii 10))
})

;; Data Maps - Rewards System
(define-map user-badges principal {
    bronze: bool,
    silver: bool,
    gold: bool,
    eco-warrior: bool,
    size-matcher: bool
})

(define-map referral-rewards principal {
    referred-count: uint,
    total-bonus: uint
})

(define-map weekly-stats {week: uint, user: principal} {
    swaps-count: uint,
    credits-earned: uint
})

;; Read-only functions - Core Platform
(define-read-only (get-item (item-id uint))
    (map-get? items item-id)
)

(define-read-only (get-user-credits (user principal))
    (default-to u0 (map-get? user-credits user))
)

(define-read-only (get-user-stats (user principal))
    (default-to
        {items-posted: u0, successful-swaps: u0, reputation-score: u50}
        (map-get? user-stats user)
    )
)

(define-read-only (get-platform-fee-percent)
    (var-get platform-fee-percent)
)

(define-read-only (get-next-item-id)
    (var-get next-item-id)
)

(define-read-only (get-size-preferences (user principal))
    (map-get? size-preferences user)
)

(define-read-only (get-swap-history (swap-id uint))
    (map-get? swap-history swap-id)
)

(define-read-only (is-size-match (user principal) (item-size (string-ascii 10)) (category (string-ascii 50)))
    (match (map-get? size-preferences user)
        prefs
            (if (is-eq category "tops")
                (is-some (index-of (get tops prefs) item-size))
                (if (is-eq category "bottoms")
                    (is-some (index-of (get bottoms prefs) item-size))
                    (if (is-eq category "shoes")
                        (is-eq (get shoes prefs) item-size)
                        (if (is-eq category "dresses")
                            (is-some (index-of (get dresses prefs) item-size))
                            false
                        )
                    )
                )
            )
        false
    )
)

;; Read-only functions - Rewards System
(define-read-only (get-user-badges (user principal))
    (default-to
        {bronze: false, silver: false, gold: false, eco-warrior: false, size-matcher: false}
        (map-get? user-badges user)
    )
)

(define-read-only (get-referral-stats (user principal))
    (default-to
        {referred-count: u0, total-bonus: u0}
        (map-get? referral-rewards user)
    )
)

(define-read-only (calculate-badge-eligibility (user principal))
    (let ((stats (get-user-stats user)))
        {
            bronze-eligible: (>= (get successful-swaps stats) bronze-threshold),
            silver-eligible: (>= (get successful-swaps stats) silver-threshold),
            gold-eligible: (>= (get successful-swaps stats) gold-threshold),
            reputation: (get reputation-score stats)
        }
    )
)

(define-read-only (get-weekly-stats (week uint) (user principal))
    (default-to
        {swaps-count: u0, credits-earned: u0}
        (map-get? weekly-stats {week: week, user: user})
    )
)

;; Private functions
(define-private (calculate-platform-fee (amount uint))
    (/ (* amount (var-get platform-fee-percent)) u100)
)

(define-private (min-uint (a uint) (b uint))
    (if (<= a b) a b)
)

(define-private (get-current-week)
    (/ stacks-block-height u1008) ;; Approximate weekly blocks
)

(define-private (update-user-stats (user principal) (stat-type (string-ascii 20)))
    (let ((current-stats (get-user-stats user)))
        (map-set user-stats user
            (if (is-eq stat-type "post")
                (merge current-stats {items-posted: (+ (get items-posted current-stats) u1)})
                (if (is-eq stat-type "swap")
                    (merge current-stats {
                        successful-swaps: (+ (get successful-swaps current-stats) u1),
                        reputation-score: (min-uint u100 (+ (get reputation-score current-stats) u2))
                    })
                    current-stats
                )
            )
        )
    )
)

(define-private (update-weekly-stats (user principal) (credits-amount uint))
    (let (
        (current-week (get-current-week))
        (current-weekly (get-weekly-stats current-week user))
    )
        (map-set weekly-stats {week: current-week, user: user} {
            swaps-count: (+ (get swaps-count current-weekly) u1),
            credits-earned: (+ (get credits-earned current-weekly) credits-amount)
        })
    )
)

;; Public functions - Core Platform

;; Post a new clothing item
(define-public (post-item
    (title (string-ascii 100))
    (description (string-ascii 500))
    (category (string-ascii 50))
    (size (string-ascii 10))
    (condition uint)
    (credit-value uint)
    (image-hash (optional (string-ascii 64)))
)
    (let ((item-id (var-get next-item-id)))
        (asserts! (and (>= condition condition-poor) (<= condition condition-excellent)) err-invalid-condition)
        (asserts! (>= condition (var-get min-condition-score)) err-invalid-condition)
        (asserts! (> credit-value u0) err-invalid-params)
        (asserts! (< (len title) u100) err-invalid-params)
        (asserts! (< (len description) u500) err-invalid-params)

        (map-set items item-id {
            owner: tx-sender,
            title: title,
            description: description,
            category: category,
            size: size,
            condition: condition,
            credit-value: credit-value,
            available: true,
            created-at: stacks-block-height,
            image-hash: image-hash
        })

        (var-set next-item-id (+ item-id u1))
        (update-user-stats tx-sender "post")

        (ok item-id)
    )
)

;; Update size preferences
(define-public (update-size-preferences
    (tops (list 5 (string-ascii 10)))
    (bottoms (list 5 (string-ascii 10)))
    (shoes (string-ascii 10))
    (dresses (list 5 (string-ascii 10)))
)
    (begin
        (map-set size-preferences tx-sender {
            tops: tops,
            bottoms: bottoms,
            shoes: shoes,
            dresses: dresses
        })
        (ok true)
    )
)

;; Swap items using credits
(define-public (swap-item (item-id uint))
    (let (
        (item (unwrap! (map-get? items item-id) err-not-found))
        (item-owner (get owner item))
        (credit-cost (get credit-value item))
        (buyer-credits (get-user-credits tx-sender))
        (platform-fee (calculate-platform-fee credit-cost))
        (owner-credits (- credit-cost platform-fee))
        (swap-id (var-get next-swap-id))
    )
        (asserts! (not (is-eq tx-sender item-owner)) err-same-user)
        (asserts! (get available item) err-item-unavailable)
        (asserts! (>= buyer-credits credit-cost) err-insufficient-credits)

        ;; Transfer credits
        (map-set user-credits tx-sender (- buyer-credits credit-cost))
        (map-set user-credits item-owner (+ (get-user-credits item-owner) owner-credits))

        ;; Mark item as unavailable
        (map-set items item-id (merge item {available: false}))

        ;; Record swap history
        (map-set swap-history swap-id {
            item-id: item-id,
            from-user: item-owner,
            to-user: tx-sender,
            credits-paid: credit-cost,
            timestamp: stacks-block-height
        })
        (var-set next-swap-id (+ swap-id u1))

        ;; Update stats for both users
        (update-user-stats tx-sender "swap")
        (update-user-stats item-owner "swap")

        ;; Update weekly statistics
        (update-weekly-stats tx-sender credit-cost)
        (update-weekly-stats item-owner owner-credits)

        (ok swap-id)
    )
)

;; Award initial credits to new users
(define-public (claim-welcome-credits)
    (let ((current-credits (get-user-credits tx-sender)))
        (asserts! (is-eq current-credits u0) err-unauthorized)
        (map-set user-credits tx-sender u100) ;; 100 welcome credits
        (ok u100)
    )
)

;; Remove item listing
(define-public (remove-item (item-id uint))
    (let ((item (unwrap! (map-get? items item-id) err-not-found)))
        (asserts! (is-eq tx-sender (get owner item)) err-unauthorized)
        (asserts! (get available item) err-item-unavailable)

        (map-set items item-id (merge item {available: false}))
        (ok true)
    )
)

;; Public functions - Rewards System

;; Claim achievement badge
(define-public (claim-badge (badge-type (string-ascii 20)))
    (let (
        (current-user-stats (get-user-stats tx-sender))
        (current-badges (get-user-badges tx-sender))
        (successful-swaps (get successful-swaps current-user-stats))
        (items-posted (get items-posted current-user-stats))
    )
        (if (is-eq badge-type "bronze")
            (begin
                (asserts! (>= successful-swaps bronze-threshold) err-not-eligible)
                (asserts! (not (get bronze current-badges)) err-already-claimed)
                (map-set user-badges tx-sender (merge current-badges {bronze: true}))
                (map-set user-credits tx-sender (+ (get-user-credits tx-sender) u50))
                (ok true)
            )
            (if (is-eq badge-type "silver")
                (begin
                    (asserts! (>= successful-swaps silver-threshold) err-not-eligible)
                    (asserts! (not (get silver current-badges)) err-already-claimed)
                    (map-set user-badges tx-sender (merge current-badges {silver: true}))
                    (map-set user-credits tx-sender (+ (get-user-credits tx-sender) u150))
                    (ok true)
                )
                (if (is-eq badge-type "gold")
                    (begin
                        (asserts! (>= successful-swaps gold-threshold) err-not-eligible)
                        (asserts! (not (get gold current-badges)) err-already-claimed)
                        (map-set user-badges tx-sender (merge current-badges {gold: true}))
                        (map-set user-credits tx-sender (+ (get-user-credits tx-sender) u300))
                        (ok true)
                    )
                    (if (is-eq badge-type "eco-warrior")
                        (begin
                            (asserts! (>= items-posted u10) err-not-eligible)
                            (asserts! (not (get eco-warrior current-badges)) err-already-claimed)
                            (map-set user-badges tx-sender (merge current-badges {eco-warrior: true}))
                            (map-set user-credits tx-sender (+ (get-user-credits tx-sender) u100))
                            (ok true)
                        )
                        err-invalid-tier
                    )
                )
            )
        )
    )
)

;; Process referral bonus
(define-public (process-referral (referrer principal))
    (let ((current-stats (get-referral-stats referrer)))
        (asserts! (not (is-eq referrer tx-sender)) err-same-user)
        (asserts! (is-eq (get-user-credits tx-sender) u0) err-unauthorized) ;; Only for new users

        (map-set referral-rewards referrer {
            referred-count: (+ (get referred-count current-stats) u1),
            total-bonus: (+ (get total-bonus current-stats) u25)
        })
        (map-set user-credits referrer (+ (get-user-credits referrer) u25))
        (map-set user-credits tx-sender u125) ;; Welcome credits + referral bonus
        (ok true)
    )
)

;; Admin functions

;; Set platform fee (owner only)
(define-public (set-platform-fee (new-fee-percent uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (<= new-fee-percent u20) err-invalid-params) ;; Max 20% fee
        (var-set platform-fee-percent new-fee-percent)
        (ok true)
    )
)

;; Set minimum condition score (owner only)
(define-public (set-min-condition-score (new-min uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (and (>= new-min condition-poor) (<= new-min condition-excellent)) err-invalid-params)
        (var-set min-condition-score new-min)
        (ok true)
    )
)

;; Award bonus credits (owner only)
(define-public (award-bonus-credits (user principal) (amount uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set user-credits user (+ (get-user-credits user) amount))
        (ok true)
    )
)
