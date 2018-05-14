#lang racket/base

(require define-with-spec
         racket/match
         racket/struct
         racket/fixnum
         "loc.rkt")

(provide initial-game-state
         game-state?

         current-tick
         current-turn
         current-die
         
         human-player?

         marble?
         marble-player

         selected-marble
         selected-movement-loc
         selected-movement-dest
         selected-marble-moving?
         valid-selected-marble-move?
         initiate-selected-marble-move
         move-selected-marble-one-step
         set-selected-marble

         marble-loc
         loc-ref

         possible-moves
         next-loc

         skip-turn
         increment-tick

         in-player-marbles)


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

(define (die-val? x)
  (and (fixnum? x)
       (and (fx<= 1 x)
            (or (fx<= x 6)
                (fx= x 10)))))

(struct/spec game-info (;; Which player is a human? #t is human, #f is AI
                        [players (list boolean? boolean? boolean? boolean?)]))


(struct/spec selection (;; which marble is selected
                        [marble marble?]
                        ;; if this marble is moving, at what
                        ;; location is it currently
                        [movement-location (either loc? #f)]
                        ;; if this marble is moving, what is
                        ;; its final destination
                        [movement-destination (either dest? #f)]))

(struct/spec game-state
             (;; how many ticks have passed?
              [tick exact-nonnegative-integer?]
              [info game-info?]
              ;; Whose turn is it?
              [turn quadrant?]
              ;; List of die values to be used by the current player
              ;; (there can be more than 1 value after landing on another player)
              [dice (listof die-val?)]
              ;; Is there a marble currently selected?
              [selected (either selection? #f)]
              ;; Mapping of locations to marbles & marbles to locations:
              [board hash?]))

(define (initial-game-state player1 player2 player3 player4)
  ;; build initial marble/location mappings
  (define board
    (for/fold ([board (hasheq)])
              ([m (in-marbles)]
               [i (in-range 16)])
      (define loc (home (quotient i 4) (remainder i 4)))
      (hash-set* board loc m m loc)))
  (game-state 0
              (game-info (list (and player1 #t)
                               (and player2 #t)
                               (and player3 #t)
                               (and player4 #t)))
              (random 4) ;; a random player starts
              (list (die-roll))
              #f ;; start with no marble selected
              board))

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
  (hash-ref (game-state-board s)
            m
            (λ () (error 'marble-loc "impossible!"))))

(define/spec (human-player? s p)
  (-> game-state? quadrant? boolean?)
  (and (list-ref (game-info-players (game-state-info s)) p) #t))

(define/spec (current-tick s)
  (-> game-state? exact-nonnegative-integer?)
  (game-state-tick s))

;; whose turn is it?
(define/spec (current-turn s)
  (-> game-state? quadrant?)
  (game-state-turn s))

(define/spec (current-die s)
  (-> game-state? die-val?)
  (car (game-state-dice s)))

;; which marble is currently selected?
(define/spec (current-selection s)
  (-> game-state? (either selection? #f))
  (game-state-selected s))

(define/spec (selected-marble s)
  (-> game-state? (either marble? #f))
  (cond
    [(game-state-selected s)
     => selection-marble]
    [else #f]))

(define/spec (selected-movement-loc s)
  (-> game-state? (either loc? #f))
  (cond
    [(game-state-selected s)
     => selection-movement-location]
    [else #f]))

(define/spec (selected-movement-dest s)
  (-> game-state? (either dest? #f))
  (cond
    [(game-state-selected s)
     => selection-movement-destination]
    [else #f]))


(define/spec (set-selected-marble s maybe-m)
  (-> game-state? (either marble? #f) game-state?)
  (match-define (game-state tick info turn dice _ board) s)
  (game-state (add1 tick) info turn dice
              (and maybe-m (selection maybe-m #f #f))
              board))

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
  (-> game-state? marble? die-val?
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
(define/spec (valid-selected-marble-move? s clicked-location)
  (-> game-state? loc? boolean?)
  (define sel-marble (selection-marble (game-state-selected s)))
  (define dist (current-die s))
  (define moves (possible-moves s sel-marble dist))
  (or (equal? clicked-location moves)
      (and (pair? moves)
           (or (equal? (car moves) clicked-location)
               (equal? (cdr moves) clicked-location)))))

(define (inc-turn n)
  (remainder (add1 n) 4))


;; NOTE: `initiate-move` assumes the move is valid!
(define/spec (initiate-selected-marble-move s dest)
  (-> game-state? dest? game-state?)
  (match-define (game-state tick info turn dice (selection m _ _) board) s)
  (game-state (add1 tick) info turn dice
              (selection m (marble-loc s m) dest)
              board))

(define/spec (selected-marble-moving? s)
  (-> game-state? boolean?)
  (cond
    [(game-state-selected s)
     => (λ (sel) (and (selection-movement-location sel) #t))]
    [else #f]))


(define/spec (move-selected-marble-one-step s)
  (-> game-state? game-state?)
  (match-define (game-state tick
                            info
                            turn
                            dice
                            (selection sel-marble sel-loc sel-dest)
                            board)
    s)
  (cond
    [(next-loc turn sel-loc sel-dest)
     => (λ (new-loc) (game-state (add1 tick)
                                 info
                                 turn
                                 dice
                                 (selection sel-marble new-loc sel-dest)
                                 board))]
    [else (move-marble/inc-turn s sel-marble sel-dest)]))

;; given state `s`, move marble `m` to `dest`
;; (adjusting any marble already at `dest` if
;;  necessary)
;; NOTE: `move-marble` assumes the move is valid!
(define/spec (move-marble/inc-turn s m dest)
  (-> game-state? marble? dest? game-state?)
  (match-define (game-state tick info turn dice _ board) s)
  ;; consume the movement, unselect the marble, if there are no movements left
  ;; for this player, roll the dice again and increment the turn
  (define start-loc (hash-ref board m))
  (define maybe-dest-marble (hash-ref board dest #f))
  (set! board (hash-set* (hash-remove board start-loc)
                         dest m
                         m dest))
  (define last-roll (car dice))
  (set! dice (cdr dice))
  (when (eqv? 6 last-roll)
    (set! dice (append dice (list (die-roll)))))
  
  (when maybe-dest-marble
    ;; if there was a marble at the destination,
    ;; move it to an available home location
    (define color (marble-player maybe-dest-marble))
    (for*/first ([idx (in-range 4)]
                 [h (in-value (home color idx))]
                 #:unless (hash-ref board h #f))
      (set! board (hash-set* board
                             h maybe-dest-marble
                             maybe-dest-marble h)))
    ;; award the current player a 10
    (set! dice (cons 10 dice)))
  (when (null? dice)
    (set! turn (inc-turn turn))
    (set! dice (list (die-roll))))
  (game-state (add1 tick) info turn dice #f board))

(define/spec (skip-turn s)
  (-> game-state? game-state?)
  (match-define (game-state tick info turn dice _ board) s)
  (match dice
    [(cons 6 rst)
     (game-state (add1 tick) info turn (append rst (list (die-roll))) #f board)]
    [(cons _ (? pair? rst))
     (game-state (add1 tick) info turn rst #f board)]
    [(cons _ (? null?))
     (game-state (add1 tick) info (inc-turn turn) (list (die-roll)) #f board)]))

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


(define/spec (increment-tick s)
  (-> game-state? game-state?)
  (match-define (game-state tick info turn dice sel board) s)
  (game-state (add1 tick) info turn dice sel board))
