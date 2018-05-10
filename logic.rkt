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


;; players : (list boolean? boolean? boolean? boolean?)
;;     which player is a human? #t is human, #f is AI
;; turn : (<= 0 n 3)
;;     whose turn is it?
;; movements : (listof (or (<= 1 n 6) 10))
;; selected : marble or #f
;;     which marble is selected?
;; board : (hash loc? marble?)
;;     mapping from locations to marbles
;; marble-locations : (hash marble? loc?)
;;     mapping from barbles to locations                   
(struct/spec game-state
             ([players (list boolean? boolean? boolean? boolean?)]
              [turn quadrant?]
              [movements (listof movement?)]
              [selected (either marble? #f)]
              [board hash?]
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
  (game-state (list (and player1 #t)
               (and player2 #t)
               (and player3 #t)
               (and player4 #t))
         0  ;; initial turn is player 0
         (list (die-roll))
         #f ;; start with no marble selected 
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
  (match-define (game-state ps turn ms _ b mlocs) s)
  (game-state ps turn ms maybe-m b mlocs))

(define filter-moves
  (case-lambda
    [(s m start dest)
     (define player (current-turn s))
     (match (loc-ref s dest)
       [(marble other-player)
        (cond
          ;; you can't land on your own marble
          [(eqv? player other-player) #f]
          [else
           (cond
             ;; or on an enemy marble in its safe spot
             [(and (coord? dest)
                   (eqv? other-player (coord->quadrant dest))
                   (zero? (coord->index dest)))
              #f]
             [else dest])])]
       [#f
        ;; you can't pass your own marbles
        (cond
          [(for*/or ([m* (in-player-marbles player)]
                     #:when (not (eq? m m*))
                     [loc* (in-value (marble-loc s m*))])
             (and (loc< start loc*)
                  (loc< loc* dest)))
           #f]
          [else dest])])]
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
  (modulo (add1 n) 4))

;; given state `s`, move marble `m` to `dest`
;; (adjusting any marble already at `dest` if
;;  necessary)
;; NOTE: `move-marble` assumes the move is valid!
(define/spec (move-marble s m dest)
  (-> game-state? marble? dest? game-state?)
  (match-define (game-state players turn movements selected board marble-locations) s)
  ;; consume the movement, unselect the marble, if there are no movements left
  ;; for this player, roll the dice again and increment the turn
  (define start (hash-ref marble-locations m))
  (set! board (hash-remove board start))
  (define last-roll (car movements))
  (set! movements (cdr movements))
  (when (eqv? 6 last-roll)
    (set! movements (append movements (list (die-roll)))))
  (set! selected #f)
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
  (game-state players turn movements selected board marble-locations))

(define/spec (next-turn s)
  (-> game-state? game-state?)
  (match-define (game-state players turn movements selected board marble-locations) s)
  (match movements
    [(cons cur rst)
     #:when (or (pair? rst)
                (eqv? 6 cur))
     (game-state players
                 turn
                 (if (eqv? 6 cur)
                     (append rst (list (die-roll)))
                     rst)
                 #f
                 board
                 marble-locations)]
    [_
     (game-state players
                 (inc-turn turn)
                 (list (die-roll))
                 #f
                 board
                 marble-locations)]))
