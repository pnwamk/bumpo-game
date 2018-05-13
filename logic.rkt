#lang racket/base

(require define-with-spec
         racket/match
         racket/struct
         racket/fixnum
         "loc.rkt")

(provide (all-defined-out))

;; the board has 4 quadrants
;; and a "center location" C
;;       |
;;  3    |   0
;;       |       
;;- - -  C  - - - 
;;       |
;;  2    |   1
;;       |


;; each quadrant has:
;; 12 standard locations (0-11 + q*12),
;; 4 goal locations (1q0-1q3),
;; and 4 home locatinos ('hq0-'hq3)
;; here is quadrant 0 for an example:
;;- - - - - - - - - - - -|
;;        0   h00        |
;;   100  1     h01      |
;;   101  2       h02    |
;;   102  3          h03 |
;;   103  4              |
;;        5 6 7 8 9 10   |
;; 'center          11   |
;;                       |
;;- - - - - - - - - - - -|

(define (die-roll) (add1 (random 6)))

;; player : 0 <= n <= 3
(struct marble (player)
  #:constructor-name make-new-marble-use-sparingly
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (m) 'marble)
      (lambda (m) (list (marble-player m)))))])
(define marbles
  (vector->immutable-vector
   (for*/vector ([player (in-range 4)]
                 [_ (in-range 4)])
     (make-new-marble-use-sparingly player))))

(define-syntax-rule (in-marbles)
  (in-vector marbles))

(define (movement? x)
  (and (fixnum? x)
       (and (fx<= 1 x)
            (or (fx<= x 6)
                (eq? x 10)))))

                  
(struct/spec game-state
             (;; how many ticks have passed?
              [tick exact-nonnegative-integer?]
              ;; Which player is a human? #t is human, #f is AI
              [players (list boolean? boolean? boolean? boolean?)]
              ;; Whose turn is it?
              [turn quadrant?]
              ;; List of die values to be used by the current player
              ;; (there can be more than 1 value after landing on another player)
              [movements (listof movement?)]
              ;; Which marble does the current player have selected?
              [selected (either marble? #f)]
              ;; Are we currently animating a movement for the selected
              ;; marble? if so, what is the current location:
              [active-location (either loc? #f)]
              ;; and what is the current destination:
              [active-destination (either dest? #f)]
              ;; Mapping of locations to marbles:
              [board hash?]
              ;; Mapping of marbles to locations:
              [marble-locations hash?]))

(define (initial-game-state player1 player2 player3 player4)
  ;; build initial marble/location mappings
  (define-values (b m-locs)
    (for/fold ([b (hasheq)]
               [m-locs (hasheq)])
              ([m (in-vector marbles)]
               [i (in-range 16)])
      (define h (home (quotient i 4) (remainder i 4)))
      (values (hash-set b h m) (hash-set m-locs m h))))
  (game-state 0
              (list (and player1 #t)
                    (and player2 #t)
                    (and player3 #t)
                    (and player4 #t))
         0  ;; initial turn is player 0
         (list (die-roll))
         #f ;; start with no marble selected
         #f ;; start with no active marble location
         #f ;; start with no active marble dest
         b
         m-locs))

;; iterates through the marbles for player `player-num`
(define-syntax-rule (in-player-marbles player-num)
  (in-vector marbles
             (fxlshift player-num 2)
             (fx+ (fxlshift player-num 2) 4)))

;; return the marble at the location, or #f if
;; nothing is there
(define/spec (loc-ref s l)
  (-> game-state? loc? (either marble? #f))
  (hash-ref (game-state-board s) l #f))

;; where is a particular marble location?
(define/spec (marble-loc s m)
  (-> game-state? marble? loc?)
  (hash-ref (game-state-marble-locations s)
            m
            (λ () (error 'marble-loc "impossible!"))))

;; whose turn is it?
(define/spec (current-turn s)
  (-> game-state? quadrant?)
  (game-state-turn s))

(define/spec (game-state-die s)
  (-> game-state? movement?)
  (car (game-state-movements s)))

;; which marble is currently selected?
(define/spec (selected-marble s)
  (-> game-state? (either marble? #f))
  (game-state-selected s))


(define/spec (set-selected-marble s maybe-m)
  (-> game-state? (either marble? #f) game-state?)
  (match-define (game-state tick ps turn ms _ _ _ board mlocs) s)
  (game-state (add1 tick) ps turn ms maybe-m #f #f board mlocs))

(define/spec (passes-self? s start dest)
  (-> game-state? loc? dest? boolean?)
  (define player (current-turn s))
  (let loop ([cur start])
    (define next (next-loc player cur dest))
    (cond
      [(not next) #f]
      [else
       (define maybe-marble-at-loc (loc-ref s next))
       (cond
         [(and maybe-marble-at-loc
               (eqv? player (marble-player maybe-marble-at-loc)))
          #t]
         [else (loop next)])])))

(define filter-moves
  (case-lambda
    [(s m start dest)
     (cond
       ;; you can't pass your own marbles
       [(passes-self? s start dest) #f]
       [(loc-ref s dest)
        => (match-lambda
             [(marble other-player)
              (cond
                ;; you can't land on your own marble
                [(eqv? (current-turn s) other-player) #f]
                ;; or on an enemy marble in its safe spot
                [(and (coord? dest)
                      (eqv? other-player (coord->quadrant dest))
                      (zero? (coord->index dest)))
                 #f]
                [else dest])])]
       [else dest])]
    [(s m start dest1 dest2)
     (cond
       [(filter-moves s m start dest1)
        (if (filter-moves s m start dest2)
            (cons dest1 dest2)
            dest1)]
       [else (filter-moves s m start dest2)])]))

;; given state `s`, marble `m`, and
;; movement distance `dist`, calculate
;; possible moves for `m`
(define/spec (possible-moves s m dist)
  (-> game-state? marble? movement?
      (either dest? (cons dest? dest?) #f))
  (define player (current-turn s))
  (cond
    ;; if it's not your marble, you can't move it
    [(not (eqv? player (marble-player m))) #f]
    [else
     (define start (marble-loc s m))
     (match* (start dist)
       [((? home?) 1) (filter-moves s m start (coord player 0))]
       [((? home?) 6) (filter-moves s m start (coord player 5))]
       [((? home?) _) #f]
       [((? center?) 1)
        (filter-moves s m start (coord (predecessor-quadrant player) 5))]
       [((? center?) _) #f]
       [(_ _)
        (cond
          ;; marble is already in either the goal or the location
          ;; 1 before the goal -- check if we can move the marble
          ;; into a valid goal spot.
          [(goal? start)
           (define goal-pos (goal->index start))
           (define n* (+ goal-pos dist))
           (cond
             [(<= n* 3) (filter-moves s m start
                                      (goal player n*))]
             [else #f])]
          [else
           (define-values (q pos) (coord->quadrant/index start))
           ;; each quadrant has 12 non-goal locations, so mod by 12
           (define-values (overflow pos*)
             (quotient/remainder (+ dist pos) 12))
           (cond
             ;; when movement did not result in changing quadrants
             [(zero? overflow)
              (cond
                ;; the special case when a marble may enter
                ;; the center location
                [(and (eqv? q player) (eqv? pos* 6))
                 (filter-moves s m start center (coord player 6))]
                [else
                 (filter-moves s m start (coord q pos*))])]
             ;; when movement did result in overflow but
             ;; we weren't in the predecessor quadrant yet
             ;; (and so we don't have to worry about rounding
             ;; home plate, so to speak.
             [(not (eqv? q (predecessor-quadrant player)))
              (filter-moves s m start (coord (increment-quadrant q) pos*))]
             ;; when movement did result in overlow, we
             ;; were in the predecessor quadrant, but
             ;; the movement was too far.
             [(< 3 pos*) #f]
             ;; when movement did result in overlow, we
             ;; were in the predecessor quadrant, and
             ;; the movement wasn't too far.
             [else (filter-moves s m start (goal player pos*))])])])]))


;; the user clicked on a location -- can they
;; move the selected marble there?
(define/spec (valid-move? s clicked-location)
  (-> game-state? loc? boolean?)
  (define sel-marble (selected-marble s))
  (define dist (game-state-die s))
  (define moves (possible-moves s sel-marble dist))
  (or (equal? clicked-location moves)
      (and (pair? moves)
           (or (equal? (car moves) clicked-location)
               (equal? (cdr moves) clicked-location)))))

(define (inc-turn n)
  (remainder (add1 n) 4))


;; NOTE: `initiate-move` assumes the move is valid!
(define/spec (initiate-move s m dest)
  (-> game-state? marble? dest? game-state?)
  (match-define (game-state tick players turn movements _ _ _ board marble-locations) s)
  (game-state (add1 tick) players turn movements m (marble-loc s m) dest board marble-locations))

(define/spec (marble-moving? s)
  (-> game-state? boolean?)
  (and (game-state-active-location s) #t))


(define/spec (move-marble-one-step s)
  (-> game-state? game-state?)
  (match-define (game-state tick
                            players
                            turn
                            movements
                            selected
                            selected-loc
                            selected-dest
                            board
                            marble-locations)
    s)
  (cond
    [(next-loc turn selected-loc selected-dest)
     => (λ (new-loc) (game-state (add1 tick)
                                 players
                                 turn
                                 movements
                                 selected
                                 new-loc
                                 selected-dest
                                 board
                                 marble-locations))]
    [else
     (move-marble/inc-turn s selected selected-dest)]))

;; given state `s`, move marble `m` to `dest`
;; (adjusting any marble already at `dest` if
;;  necessary)
;; NOTE: `move-marble` assumes the move is valid!
(define/spec (move-marble/inc-turn s m dest)
  (-> game-state? marble? dest? game-state?)
  (match-define (game-state tick players turn movements _ _ _ board marble-locations) s)
  ;; consume the movement, unselect the marble, if there are no movements left
  ;; for this player, roll the dice again and increment the turn
  (define start (hash-ref marble-locations m))
  (set! board (hash-remove board start))
  (define last-roll (car movements))
  (set! movements (cdr movements))
  (when (eqv? 6 last-roll)
    (set! movements (append movements (list (die-roll)))))
  
  (define maybe-dest-marble (hash-ref board dest #f))
  (set! board (hash-set board dest m))
  (set! marble-locations (hash-set marble-locations m dest))
  (when maybe-dest-marble
    ;; if there was a marble at the destination,
    ;; move it to an available home location
    (define color (marble-player maybe-dest-marble))
    (for*/first ([idx (in-range 4)]
                 [h (in-value (home color idx))]
                 #:unless (hash-ref board h #f))
      (set! board (hash-set board h maybe-dest-marble))
      (set! marble-locations (hash-set marble-locations maybe-dest-marble h)))
    ;; award the current player a 10
    (set! movements (cons 10 movements)))
  (when (null? movements)
    (set! turn (inc-turn turn))
    (set! movements (list (die-roll))))
  (game-state (add1 tick) players turn movements #f #f #f board marble-locations))

(define/spec (skip-turn s)
  (-> game-state? game-state?)
  (match-define (game-state tick ps turn movements _ _ _ board m-locs) s)
  (match movements
    [(cons 6 rst)
     (game-state (add1 tick) ps turn (append rst (list (die-roll))) #f #f #f board m-locs)]
    [(cons _ (? pair? rst))
     (game-state (add1 tick) ps turn rst #f #f #f board m-locs)]
    [(cons _ (? null?))
     (game-state (add1 tick) ps (inc-turn turn) (list (die-roll)) #f #f #f board m-locs)]))

(define/spec (loc-add1 l)
  (-> loc? dest?)
  (remainder (add1 l) 48))

;; if a marble is going to move from `start` to `end`,
;; what is the single next step towards that movement,
;; or #f if start == end
(define/spec (next-loc player start end)
  (-> quadrant? loc? dest? (either dest? #f))
  (match start
    [(== end) #f]
    [(? home?) (coord player 0)]
    [(? center?) (coord (predecessor-quadrant player) 5)]
    [(? goal?) (goal player (add1 (goal->index start)))]
    [(? coord?)
     (match end
       [(? coord?) (loc-add1 start)]
       [(? center?) (if (and (eqv? (coord->quadrant start)
                                   player)
                             (eqv? 5 (coord->index start)))
                        center
                        (add1 start))]
       [(? goal?) (if (and (eqv? (coord->quadrant start)
                                 (predecessor-quadrant player))
                           (eqv? 11 (coord->index start)))
                      (goal player 0)
                      (add1 start))])]))


(define/spec (increment-tick-count s)
  (-> game-state? game-state?)
  (match-define (game-state tick ps turn movements sel sel-loc sel-dest board m-locs) s)
  (game-state (add1 tick) ps turn movements sel sel-loc sel-dest board m-locs))
