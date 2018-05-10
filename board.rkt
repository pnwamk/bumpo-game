#lang racket/base

(require define-with-spec
         racket/match
         racket/struct
         racket/fixnum)



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

(define (movement? x)
  (and (fixnum? x)
       (and (fx<= 0 x)
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
(struct/spec state ([players (list boolean? boolean? boolean? boolean?)]
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
  (state (list (and player1 #t)
               (and player2 #t)
               (and player3 #t)
               (and player4 #t))
         0  ;; initial turn is player 0
         (list (random 6))
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
  (-> state? loc? (either marble? #f))
  (hash-ref (state-board s) l #f))

;; where is a particular marble location?
(define/spec (marble-loc s m)
  (-> spec? marble? loc?)
  (hash-ref (state-marble-locations s)
            m
            (λ () (error 'marble-loc "impossible!"))))

;; whose turn is it?
(define/spec (current-turn s)
  (-> state? quadrant?)
  (state-selected s))

;; which marble is currently selected?
(define/spec (selected-marble s)
  (-> state? (either marble? #f))
  (state-selected s))

(define/spec (move-marble s m dest)
  (-> state? marble? dest? state?)
  (match-define (state players turn movements selected board marble-locations) s)
  (define maybe-dest-marble (hash-ref board m #f))
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
      (set! marble-locations (hash-set marble-locations maybe-dest-marble h))))
  ;; consume the movement, unselect the marble, if there are no movements left
  ;; for this player, roll the dice again and increment the turn
  (set! movements (cdr movements))
  (set! selected #f)
  (when (null? movements)
    (set! turn (modulo (add1 turn) 4))
    (set! movements (list (random 6))))
  (state players turn movements selected board marble-locations))