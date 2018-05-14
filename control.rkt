#lang racket/base

(require define-with-spec
         2htdp/universe
         racket/list
         racket/match
         "loc.rkt"
         "logic.rkt"
         "graphics.rkt")

(provide handle-mouse
         handle-tick)

(define/spec (handle-mouse s x y mevent)
  (-> game-state? real? real? mouse-event?
      game-state?)
  (cond
    [(or (not (human-player? s (current-turn s)))
         (selected-marble-moving? s))
     s]
    [(not (mouse=? mevent "button-down")) s]
    [else
     (define sel-marble (selected-marble s))
     (define clicked-location (image-coord->location x y))
     (define marble-at-loc (and clicked-location
                                (loc-ref s clicked-location)))
     (cond
       [(not clicked-location) (set-selected-marble s #f)]
       [(and clicked-location
             sel-marble
             (valid-selected-marble-move? s clicked-location))
        (initiate-selected-marble-move s clicked-location)]
       [(and marble-at-loc
             (eqv? (current-turn s)
                   (marble-player marble-at-loc)))
        (set-selected-marble s marble-at-loc)]
       [else
        (set-selected-marble s #f)])]))

(define/spec (handle-tick s)
  (-> game-state? game-state?)
  (cond
    ;; marble is moving (should happen every tick)
    [(selected-marble-moving? s) (move-selected-marble-one-step s)]
    ;; all other activity should occur every 5 ticks
    [(not (zero? (remainder (current-tick s) 5)))
     (increment-tick s)]
    [else
     (define cur-player (current-turn s))
     (define dist (current-die s))
     (define sel-marble (selected-marble s))
     (define cur-possible-moves
       (for*/list ([m (in-player-marbles cur-player)]
                   [moves (in-value (possible-moves s m dist))]
                   #:when moves)
         (cons m moves)))
     (cond
       ;; no moves
       [(null? cur-possible-moves) (skip-turn s)]
       ;; AI player, choose a random marble to select (if
       ;; one is not selected) or move (if one is selected)
       [(not (human-player? s cur-player))
        (cond
          [(not sel-marble)
           (set-selected-marble s (car (car (shuffle cur-possible-moves))))]
          [else
           (match (assoc sel-marble cur-possible-moves)
             [(cons m (cons dest1 dest2))
              (if (zero? (random 2))
                  (initiate-selected-marble-move s dest1)
                  (initiate-selected-marble-move s dest2))]
             [(cons m dest) (initiate-selected-marble-move s dest)])])]
       [else (increment-tick s)])]))
