#lang racket/base

(require 2htdp/universe
         "logic.rkt"
         "control.rkt"
         "render.rkt")






(big-bang (initial-state #f #f #f #f)
  [on-draw render-state]
  [on-tick handle-tick .2]
  [on-mouse (λ (s x y me)
              (cond
                [(mouse=? me "button-down")
                 (handle-button-down s x y)]
                [else s]))])


