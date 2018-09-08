#lang typed/racket/base #:with-refinements

(require racket/match
         "loc.rkt")

(provide Marble
         Marble?
         Marble-player
         Marble-idx

         setup-up-state
         initial-state
         PlayerKind
         PlayerKind?
         GameInfo
         GameInfo?
         Selection
         GameState
         GameState?
         State
         
         die
         turn
         tick
         board-ref
         human?
         player-info
         
         selected-marble
         selected-move-loc
         set-selected-marble
         set-selected-marble/tick
         selected-marble-moving?
         valid-selected-marble-move?
         initiate-selected-marble-move
         move-selected-marble-one-step

         skip-turn
         possible-destinations
         next-loc
         inc-tick)

(define-type State (U GameInfo GameState String False))


(: die-roll (-> (Refine [n : Integer] (<= 1 n 6))))
(define (die-roll)
  (add1 (random 6)))

(struct Marble ([player : Z4] [idx : Z4])
  #:transparent)

(define-type PlayerKind (U 'HUM 'COM))

(: PlayerKind? (-> Any Boolean : PlayerKind))
(define (PlayerKind? p)
  (or (eq? p 'HUM) (eq? p 'COM)))

(struct GameInfo ([players : (List PlayerKind
                                   PlayerKind
                                   PlayerKind
                                   PlayerKind)])
  #:transparent)

(struct Selection (;; which marble is selected
                   [marble : Marble]
                   ;; if this marble is moving, at what
                   ;; location is it currently and
                   ;; where is it going
                   [move : (U #f (Pairof Loc Dest))])
  #:transparent)


(struct GameState
  ([tick : Natural]
   [info : GameInfo]
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

(define setup-up-state (GameInfo (list 'COM 'COM 'COM 'COM)))

(: initial-state (-> (List PlayerKind PlayerKind PlayerKind PlayerKind)
                     GameState))
(define (initial-state players)
  (GameState 0
             (GameInfo players)
             (random 4) ;; a random player starts
             (list (die-roll))
             #f ;; start with no marble selected
             (foldl (λ ([player-idx : Z4]
                        [h : (Immutable-HashTable (U Loc Marble) (U Loc Marble))])
                      (foldl (λ ([marble-idx : Z4]
                                 [h : (Immutable-HashTable (U Loc Marble) (U Loc Marble))])
                               (define home (Home player-idx marble-idx))
                               (define marble (Marble player-idx marble-idx))
                               (hash-set (hash-set h home marble)
                                         marble home))
                             h
                             z4s))
                    ((inst hash (U Loc Marble) (U Loc Marble)))
                    z4s)))

;; return the marble at the location, or #f if
;; nothing is there
(: board-ref (case->
              (-> GameState Loc (U Marble #f))
              (-> GameState Marble Loc)))
(define (board-ref s x)
  (cond
    [(Marble? x)
     (define l (hash-ref (GameState-board s) x #f))
     (cond
       [(or (not l) (Marble? l))
        (error 'board-ref "failed to locate marble! ~a ~a" s x)]
       [else l])]
    [else
     (define val (hash-ref (GameState-board s) x #f))
     (and (Marble? val) val)]))

(: human? (-> GameState Z4 Boolean))
(define (human? s p)
  (eq? 'HUM (list-ref (GameInfo-players (GameState-info s)) p)))

(: player-info (-> (U GameInfo GameState)
                   (List PlayerKind
                         PlayerKind
                         PlayerKind
                         PlayerKind)))
(define (player-info s)
  (cond
    [(GameInfo? s)
     (GameInfo-players s)]
    [else
     (GameInfo-players (GameState-info s))]))

;; whose turn is it?
(: turn (-> GameState Z4))
(define (turn s)
  (GameState-turn s))

(: die (-> GameState Movement))
(define (die s)
  (car (GameState-moves s)))

(: tick (-> GameState Natural))
(define (tick s)
  (GameState-tick s))

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
  (define sel (GameState-selected s))
  (cond [sel
         (define move (Selection-move sel))
         (and move (car move))]
        [else #f]))

(: selected-move-dest (-> GameState (U Dest #f)))
(define (selected-move-dest s)
  (define sel (GameState-selected s))
  (cond [sel
         (define move (Selection-move sel))
         (and move (cdr move))]
        [else #f]))


(: set-selected-marble (-> GameState (U Marble #f) GameState))
(define (set-selected-marble s maybe-m)
  (struct-copy GameState s [selected (and maybe-m (Selection maybe-m #f))]))

(: set-selected-marble/tick (-> GameState (U Marble #f) GameState))
(define (set-selected-marble/tick s maybe-m)
  (struct-copy GameState s
               [tick (add1 (GameState-tick s))]
               [selected (and maybe-m (Selection maybe-m #f))]))


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
               (= player (Marble-player maybe-marble-at-loc)))
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
             [(= (turn s) other-player) #f]
             ;; or on an enemy marble in its safe spot
             [(and (Coord? dest)
                   (= other-player (Coord-quad dest))
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
     (define start (board-ref s m))
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
            [(<= n* 3) (list (Goal player n*))]
            [else (list)])]
         [else
          (define q (Coord-quad start))
          (define idx (Coord-idx start))
          (define-values (idx* overflow?) (Z12+/overflow idx dist))
          (cond
            ;; when movement did not result in changing quadrants
            [(not overflow?)
             (cond
               ;; the special case when a marble may enter the center location
               [(and (= q player) (= idx* 6)) (list center (Coord player 6))]
               [else (list (Coord q idx*))])]
            ;; when movement did result in overflow but
            ;; we weren't in the predecessor quadrant yet
            ;; (and so we don't have to worry about rounding
            ;; home plate, so to speak.
            [(not (= q (Z4-sub1 player))) (list (Coord (Z4-add1 q) idx*))]
            ;; when movement did result in overlow, we
            ;; were in the predecessor quadrant, but
            ;; the movement was too far.
            [(< 3 idx*) (list)]
            ;; when movement did result in overlow, we
            ;; were in the predecessor quadrant, and
            ;; the movement wasn't too far.
            [else (list (Goal player idx*))])]))
     ;; now remove possibilities that are invalid
     (filter (valid-dest? s m start) possibilities)]))


(: valid-selected-marble-move? (-> GameState Loc Boolean))
;; the user clicked on a location -- can they
;; move the selected marble there?
(define (valid-selected-marble-move? s clicked-location)
  (define sel (GameState-selected s))
  (cond
    [(not sel) #f]
    [else
     (define sel-marble (Selection-marble sel))
     (define dist (die s))
     (define ds (possible-destinations s sel-marble dist))
     (and (member clicked-location ds) #t)]))



(: initiate-selected-marble-move (-> GameState Dest GameState))
;; NOTE: `initiate-move` assumes the move is valid!
;; if no marble is selected, is a no-op
(define (initiate-selected-marble-move s dest)
  (define m (selected-marble s))
  (if m
      (struct-copy GameState s
                   [tick (add1 (GameState-tick s))]
                   [selected (Selection m (cons (board-ref s m) dest))])
      s))

(: selected-marble-moving? (-> GameState Boolean))
(define (selected-marble-moving? s)
  (cond
    [(GameState-selected s)
     => (λ (sel) (and (Selection-move sel) #t))]
    [else #f]))


(: move-selected-marble-one-step (-> GameState GameState))
(define (move-selected-marble-one-step s)
  (define sel (GameState-selected s))
  (cond
    [sel
     (define sel-marble (Selection-marble sel))
     (define sel-move (Selection-move sel))
     (cond
       [(not sel-move) s]
       [else
        (define new-loc (next-loc (turn s) (car sel-move) (cdr sel-move)))
        (cond
          [new-loc
           (struct-copy GameState s
                        [tick (add1 (GameState-tick s))]
                        [selected (Selection sel-marble (cons new-loc (cdr sel-move)))])]
          [else (move-marble/inc-turn s sel-marble (cdr sel-move))])])]
    [else s]))

(: move-marble/inc-turn (-> GameState Marble Dest GameState))
;; given state `s`, move marble `m` to `dest`
;; (adjusting any marble already at `dest` if necessary)
;; NOTE: `move-marble` assumes the move is valid!
(define (move-marble/inc-turn s m dest)
  (define maybe-dest-marble (board-ref s dest))
  (define-values (next-moves next-turn?)
    (let* ([roll-again (if (= 6 (die s)) (list (die-roll)) (list))]
           [bonus-10 (if maybe-dest-marble (list 10) (list))]
           [ms ((inst append Movement) ;; TODO ugh why is this necessary
                bonus-10
                (cdr (GameState-moves s))
                roll-again)])
      (values ms (null? ms))))
  (struct-copy
   GameState s
   [board (let ([board* (hash-set*
                         (hash-remove
                          (hash-remove (GameState-board s) m)
                          (board-ref s m))
                         dest m
                         m dest)])
            (cond
              [maybe-dest-marble
               (define home (Home (Marble-player maybe-dest-marble)
                                  (Marble-idx maybe-dest-marble)))
               (hash-set* (hash-remove board* maybe-dest-marble)
                          home maybe-dest-marble
                          maybe-dest-marble home)]
              [else board*]))]
   [selected #f]
   [moves (if next-turn? (list (die-roll)) next-moves)]
   [turn (if next-turn?
             (Z4-add1 (turn s))
             (turn s))]))

(: skip-turn (-> GameState GameState))
(define (skip-turn s)
  (match-define (cons last-move remaining-moves) (GameState-moves s))
  (struct-copy GameState s
               [tick (add1 (GameState-tick s))]
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
    [else (Loc-add1 player start)]))

(: inc-tick (-> GameState GameState))
(define (inc-tick s)
  (struct-copy GameState s
               [tick (add1 (GameState-tick s))]))
