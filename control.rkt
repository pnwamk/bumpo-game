#lang typed/racket/base #:with-refinements

(require racket/match
         racket/list
         "loc.rkt"
         "logic.rkt"
         "render.rkt")

(provide handle-button-down
         handle-key
         handle-tick)


(: handle-key (-> State String
                  State))
(define (handle-key s ke)
  (cond
    [(or (GameInfo? s) (GameState? s))
     (define maybe-num (string->number ke))
     (cond
       [(and (exact-integer? maybe-num)
             (<= 1 maybe-num 4))
        (toggle-player-kind s (sub1 maybe-num))]
       [(and (string=? ke "\r") (GameInfo? s))
        (initial-state (player-info s))]
       [else s])]
    [(string? s) setup-up-state]
    [else s]))

(: handle-button-down (-> State Real Real State))
(define (handle-button-down s x y)
  (cond
    [(string? s) setup-up-state]
    [(not (GameState? s)) s]
    [(or (not (human? s (turn s)))
         (selected-marble-moving? s))
     s]
    [else
     (define sel-marble (selected-marble s))
     (define clicked-location (image-coord->location x y))
     (define marble-at-loc (and clicked-location
                                (board-ref s clicked-location)))
     (cond
       [(not clicked-location) (set-selected-marble s #f)]
       [(and sel-marble
             (not (Home? clicked-location))
             (valid-selected-marble-move? s clicked-location))
        (initiate-selected-marble-move s clicked-location)]
       [(and marble-at-loc
             (eqv? (turn s) (Marble-player marble-at-loc)))
        (set-selected-marble s marble-at-loc)]
       [else
        (set-selected-marble s #f)])]))

(define goal-locations
  (list (list (Goal 0 0) (Goal 0 1) (Goal 0 2) (Goal 0 3))
        (list (Goal 1 0) (Goal 1 1) (Goal 1 2) (Goal 1 3))
        (list (Goal 2 0) (Goal 2 1) (Goal 2 2) (Goal 2 3))
        (list (Goal 3 0) (Goal 3 1) (Goal 3 2) (Goal 3 3))))

;; do 3 players have all their marbles in Goal locations?
(define (game-over? s)
  (cond
    [(not (GameState? s)) #f]
    [else
     (: player-done (-> (List Goal Goal Goal Goal) Boolean))
     (define (player-done p)
       (match-define (list g1 g2 g3 g4) p)
       (and (Marble? (board-ref s g1))
            (Marble? (board-ref s g2))
            (Marble? (board-ref s g3))
            (Marble? (board-ref s g4))))
     (define players-done (foldl (λ ([goals : (List Goal Goal Goal Goal)]
                                     [count : Natural])
                                   (if (player-done goals)
                                       (add1 count)
                                       count))
                                 0
                                 goal-locations))
     (>= players-done 3)]))

(: handle-tick (-> State State))
(define (handle-tick s)
  (cond
    [(not (GameState? s)) s]
    [(game-over? s) "Game over!"]
    ;; marble is moving (should happen every tick)
    [(selected-marble-moving? s) (move-selected-marble-one-step s)]
    ;; all other activity should occur every 5 ticks
    [(not (zero? (modulo (tick s) 5))) (inc-tick s)]
    [else
     (define cur-player (turn s))
     (define dist (die s))
     (define sel-marble (selected-marble s))
     (define cur-possible-moves : (Listof (Pairof Marble (Listof Dest)))
       (foldl (λ ([marble-idx : Z4]
                  [moves : (Listof (Pairof Marble (Listof Dest)))])
                (define m (Marble cur-player marble-idx))
                (define ds (possible-destinations s m dist))
                (if (null? ds)
                    moves
                    (cons (ann (cons m ds) (Pairof Marble (Listof Dest))) moves)))
              '()
              z4s))
     (cond
       ;; no moves
       [(null? cur-possible-moves) (skip-turn s)]
       ;; AI player, choose a random marble to select (if
       ;; one is not selected) or move (if one is selected)
       [(not (human? s cur-player))
        (cond
          [(not sel-marble)
           (set-selected-marble/tick
            s
            (car (list-ref cur-possible-moves
                           (random (length cur-possible-moves)))))]
          [else
           (match (assoc sel-marble cur-possible-moves)
             [#f
              ;; a marble that cannot move is selected
              ;; (this can happen from user intervention)
              ;; unselect this marble and continue!
              (set-selected-marble/tick s #f)]
             [(cons m ds)
              (initiate-selected-marble-move s (list-ref ds (random (length ds))))])])]
       ;; humans must use their own biological tick handlers
       [else s])]))
