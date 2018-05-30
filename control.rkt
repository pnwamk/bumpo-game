#lang typed/racket/base

(require racket/match
         "loc.rkt"
         "logic.rkt"
         "render.rkt")

(provide handle-button-down
         handle-tick)


(: handle-button-down (-> GameState Real Real GameState))
(define (handle-button-down s x y)
  (cond
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

(: handle-tick (-> GameState GameState))
(define (handle-tick s)
  (cond
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
           (match-define (cons m ds) (assoc sel-marble cur-possible-moves))
           (initiate-selected-marble-move s (list-ref ds (random (length ds))))])]
       [else s])]))
