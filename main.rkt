#lang racket/base

(require 2htdp/universe
         "logic.rkt"
         "control.rkt"
         "render.rkt")






(big-bang "Welcome to Bumpo!"
  [on-draw render-state]
  [on-tick handle-tick .2]
  [stop-when (λ (s) (not s))]
  [on-key handle-key]
  [on-mouse (λ (s x y me)
              (cond
                [(mouse=? me "button-down")
                 (handle-button-down s x y)]
                [else s]))])


