#lang racket/base

(require define-with-spec
         2htdp/image
         2htdp/universe
         racket/list
         racket/match
         "logic.rkt"
         "control.rkt"
         "render.rkt")






(big-bang (initial-game-state #f #f #f #t)
  [on-draw draw-world]
  [on-tick handle-tick .2]
  [on-mouse handle-mouse])



