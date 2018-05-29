#lang typed/racket/base #:with-refinements

(require racket/match
         "typed-loc.rkt")

#;
(provide initial-game-state
         game-state?

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
         
         in-player-marbles)

(: die-roll (-> (Refine [n : Integer] (<= 1 n 6))))
(define (die-roll)
  (add1 (random 6)))

(struct Marble ([player : Z4] [idx : Z4])
  #:transparent)


(struct GameInfo ([players : (List (U String #f)
                                   (U String #f)
                                   (U String #f)
                                   (U String #f))])
  #:transparent)


(struct Selection (;; which marble is selected
                   [marble : Marble]
                   ;; if this marble is moving, at what
                   ;; location is it currently
                   [move-loc : (U Loc #f)]
                   ;; if this marble is moving, what is
                   ;; its final destination
                   [move-dest : (U Dest #f)])
  #:transparent)


(struct GameState
  ([info : GameInfo]
   ;; Whose turn is it?
   [turn : Z4]
   ;; List of die values to be used by the current player
   ;; (there can be more than 1 value after landing on another player)
   [moves : (Listof Movement)]
   ;; Is there a marble currently selected?
   [selected : (U Selection #f)]
   ;; Mapping of locations to marbles & marbles to locations:
   [board : (Immutable-HashTable (U Loc Marble) (U Loc Marble))])
  #:transparent)


(: new-GameState (-> (U String #f) (U String #f) (U String #f) (U String #f)
                     GameState))
(define (new-GameState player1 player2 player3 player4)
  (GameState (GameInfo (list player1 player2 player3 player4))
             (random 4) ;; a random player starts
             (list (die-roll))
             #f ;; start with no marble selected
             (hash (Home 0 0) (Marble 0 0)
                   (Home 0 1) (Marble 0 1)
                   (Home 0 2) (Marble 0 2)
                   (Home 0 3) (Marble 0 3)
                   (Home 1 0) (Marble 1 0)
                   (Home 1 1) (Marble 1 1)
                   (Home 1 2) (Marble 1 2)
                   (Home 1 3) (Marble 1 3)
                   (Home 2 0) (Marble 2 0)
                   (Home 2 1) (Marble 2 1)
                   (Home 2 2) (Marble 2 2)
                   (Home 2 3) (Marble 2 3)
                   (Home 3 0) (Marble 3 0)
                   (Home 3 1) (Marble 3 1)
                   (Home 3 2) (Marble 3 2)
                   (Home 3 3) (Marble 3 3))))

;; return the marble at the location, or #f if
;; nothing is there
(: board-ref (-> GameState Loc (U Marble #f)))
(define (board-ref s l)
  (define val (hash-ref (GameState-board s) l #f))
  (and (Marble? val) val))

(: marble-ref (-> GameState Marble (U Loc #f)))
(define (marble-ref s m)
  (define val (hash-ref (GameState-board s) m #f))
  (if (Marble? val)
      #f
      val))

(: human? (-> GameState Z4 Boolean))
(define (human? s p)
  (and (list-ref (GameInfo-players (GameState-info s)) p) #t))

;; whose turn is it?
(: turn (-> GameState Z4))
(define (turn s)
  (GameState-turn s))

(: die (-> GameState Movement))
(define (die s)
  (car (GameState-moves s)))

;; which marble is currently selected?
(: selected (-> GameState (U Selection #f)))
(define (selected s)
  (GameState-selected s))

(: selected-marble (-> GameState (U Marble #f)))
(define (selected-marble s)
  (cond [(GameState-selected s) => Selection-marble]
        [else #f]))

(: selected-move-loc (-> GameState (U Loc #f)))
(define (selected-move-loc s)
  (cond [(GameState-selected s) => Selection-move-loc]
        [else #f]))

(: selected-move-dest (-> GameState (U Dest #f)))
(define (selected-move-dest s)
  (cond [(GameState-selected s) => Selection-move-dest]
        [else #f]))


(: set-selected-marble (-> GameState (U Marble #f) GameState))
(define (set-selected-marble s maybe-m)
  (struct-copy GameState s [selected (and maybe-m (Selection maybe-m #f #f))]))


(: passes-self? (-> GameState Loc Dest Boolean))
;; if the current player tries to move a marble
;; from `start` to `dest`, will they pass and/or
;; land on one of their own marbles?
(define (passes-self? s start dest)
  (define player (turn s))
  (let loop ([cur start])
    (define next (next-loc player cur dest))
    (cond
      [(not next) #f]
      [else
       (define maybe-marble-at-loc (board-ref s next))
       (cond
         [(and maybe-marble-at-loc
               (eqv? player (Marble-player maybe-marble-at-loc)))
          #t]
         [else (loop next)])])))

(: valid-dest? (-> GameState Marble Loc
                   (-> Dest Boolean)))
(define ((valid-dest? s m start) dest)
  (cond
    ;; you can't pass your own marbles
    [(passes-self? s start dest) #f]
    [(board-ref s dest)
     => (match-lambda
          [(Marble other-player _)
           (cond
             ;; you can't land on your own marble
             [(eqv? (turn s) other-player) #f]
             ;; or on an enemy marble in its safe spot
             [(and (Coord? dest)
                   (eqv? other-player (Coord-quad dest))
                   (zero? (Coord-idx dest)))
              #f]
             [else #t])])]
    [else #t]))

(: possible-destinations (-> GameState Marble Movement (Listof Dest)))
;; given state `s`, marble `m`, and
;; movement distance `dist`, calculate
;; possible moves for `m`
(define (possible-destinations s m dist)
  (define player (turn s))
  (cond
    ;; if it's not your marble, you can't move it
    [(not (= player (Marble-player m))) (list)]
    [else
     (define start (marble-ref s m))
     (define possibilities : (Listof Dest)
       (cond
         [(Home? start)
          (cond
            [(or (= dist 1) (= dist 6)) (list (Coord player (sub1 dist)))]
            [else (list)])]
         [(Center? start)
          (cond
            [(= dist 1) (list (Coord (Z4-sub1 player) 5))]
            [else (list)])]
         [(Goal? start)
          (define goal-pos (Goal-idx start))
          (define n* (+ goal-pos dist))
          (cond
            [(<= n* 3) (Goal player n*)]
            [else (list)])]
         [else
          (define q (Coord-quad start))
          (define pos (Coord-idx start))
          ;; each quadrant has 12 non-goal locations, so mod by 12
          (define-values (overflow pos*)
            (let ([n (+ dist pos)])
              (if (>= n 12)
                  (values #t (modulo n 12))
                  (values #f n))))
          (cond
            ;; when movement did not result in changing quadrants
            [(not overflow)
             (cond
               ;; the special case when a marble may enter the center location
               [(and (= q player) (= pos* 6)) (list (Coord player 6))]
               [else (list (Coord q pos*))])]
            ;; when movement did result in overflow but
            ;; we weren't in the predecessor quadrant yet
            ;; (and so we don't have to worry about rounding
            ;; home plate, so to speak.
            [(not (= q (Z4-sub1 player))) (list (Coord (Z4-add1 q) pos*))]
            ;; when movement did result in overlow, we
            ;; were in the predecessor quadrant, but
            ;; the movement was too far.
            [(< 3 pos*) (list)]
            ;; when movement did result in overlow, we
            ;; were in the predecessor quadrant, and
            ;; the movement wasn't too far.
            [else (list (Goal player pos*))])]))
     (filter (valid-dest? s m start) possibilities)]))


;(: valid-selected-marble-move? (-> GameState Loc Boolean))
;;; the user clicked on a location -- can they
;;; move the selected marble there?
;(define (valid-selected-marble-move? s clicked-location)
;  (define sel-marble (Selection-marble (GameState-selected s)))
;  (define dist (die s))
;  (define moves (possible-moves s sel-marble dist))
;  (or (equal? clicked-location moves)
;      (and (pair? moves)
;           (or (equal? (car moves) clicked-location)
;               (equal? (cdr moves) clicked-location)))))



(: initiate-selected-marble-move (-> GameState Dest GameState))
;; NOTE: `initiate-move` assumes the move is valid!
;; if no marble is selected, is a no-op
(define (initiate-selected-marble-move s dest)
  (define m (selected-marble s))
  (if m
      (struct-copy GameState s
                   [selected (Selection m (marble-ref s m) dest)])
      s))

(: selected-marble-moving? (-> GameState Boolean))
(define (selected-marble-moving? s)
  (cond
    [(GameState-selected s)
     => (λ (sel) (and (Selection-move-loc sel) #t))]
    [else #f]))


;(define/spec (move-selected-marble-one-step s)
;  (-> game-state? game-state?)
;  (match-define (game-state info
;                            turn
;                            dice
;                            (selection sel-marble sel-loc sel-dest)
;                            board)
;    s)
;  (cond
;    [(next-loc turn sel-loc sel-dest)
;     => (λ (new-loc) (game-state info
;                                 turn
;                                 dice
;                                 (selection sel-marble new-loc sel-dest)
;                                 board))]
;    [else (move-marble/inc-turn s sel-marble sel-dest)]))
;
;;; given state `s`, move marble `m` to `dest`
;;; (adjusting any marble already at `dest` if
;;;  necessary)
;;; NOTE: `move-marble` assumes the move is valid!
;(define/spec (move-marble/inc-turn s m dest)
;  (-> game-state? marble? dest? game-state?)
;  (match-define (game-state info turn dice _ board) s)
;  ;; consume the movement, unselect the marble, if there are no movements left
;  ;; for this player, roll the dice again and increment the turn
;  (define start-loc (hash-ref board m))
;  (define maybe-dest-marble (hash-ref board dest #f))
;  (set! board (hash-set* (hash-remove board start-loc)
;                         dest m
;                         m dest))
;  (define last-roll (car dice))
;  (set! dice (cdr dice))
;  (when (eqv? 6 last-roll)
;    (set! dice (append dice (list (die-roll)))))
;  
;  (when maybe-dest-marble
;    ;; if there was a marble at the destination,
;    ;; move it to an available home location
;    (define color (marble-player maybe-dest-marble))
;    (for*/first ([idx (in-range 4)]
;                 [h (in-value (home color idx))]
;                 #:unless (hash-ref board h #f))
;      (set! board (hash-set* board
;                             h maybe-dest-marble
;                             maybe-dest-marble h)))
;    ;; award the current player a 10
;    (set! dice (cons 10 dice)))
;  (when (null? dice)
;    (set! turn (inc-turn turn))
;    (set! dice (list (die-roll))))
;  (game-state info turn dice #f board))

(: skip-turn (-> GameState GameState))
(define (skip-turn s)
  (match-define (cons last-move remaining-moves) (GameState-moves s))
  (struct-copy GameState s
               [selected #f]
               [turn (if (null? remaining-moves) (Z4-add1 (turn s)) (turn s))]
               [moves (cond
                        [(= 6 last-move) (append remaining-moves (list (ann (die-roll) Movement)))]
                        [(null? remaining-moves) (list (die-roll))]
                        [else remaining-moves])]))


;; if a marble is going to move from `start` to `end`,
;; what is the single next step towards that movement,
;; or #f if start == end
(: next-loc (-> Z4 Loc Dest (U Dest #f)))
(define (next-loc player start end)
  (cond
    [(equal? start end) #f]
    [(and (Coord? start)
          (Center? end)
          (= player (Coord-quad start))
          (= 5 (Coord-idx start)))
     center]
    [else (Loc-inc player start)]))
