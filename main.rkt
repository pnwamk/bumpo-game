#lang racket/base

(require define-with-spec
         racket/class
         lux
         lux/chaos/gui
         lux/chaos/gui/key
         lux/chaos/gui/mouse
         (only-in 2htdp/image image-width image-height)
         mrlib/image-core
         racket/list
         racket/match
         "logic.rkt"
         "control.rkt"
         "render.rkt")




(struct bumpo (tick state)
  #:transparent
  #:methods gen:word
  [;; fps
   (define (word-fps b) 20.0)
   ;; label
   (define (word-label b frame-time) "Bumpo!")
   ;; event handler
   (define (word-event b e)
     (cond
       [(or (eq? e 'close)
            (and (key-event? e)
                 (eq? (send e get-key-code) 'escape)))
        #f]
       [(and (mouse-event? e)
             (eq? (send e get-event-type) 'left-down))
        (match-define (bumpo tick state) b)
        (bumpo tick
               (handle-button-down state (send e get-x) (send e get-y)))]
       [else b]))
   ;; output
   (define (word-output b)
     (match-define (bumpo tick state) b)
     (lambda (width height dc)
       (define board (render-state state))
       (send dc set-background "white")
       (send dc clear)
       (render-image board dc 0 0)))
   ;; tick
   (define (word-tick b)
     (match-define (bumpo tick state) b)
     (bumpo (add1 tick) (handle-tick state tick)))])


(define (play-bumpo)
  (fiat-lux (bumpo 0 (initial-game-state #t #f #f #f))))

(define-values (width height)
  (let ([img (render-state (initial-game-state #f #f #f #f))])
    (values (image-width img)
            (image-height img))))

(module+ main
  (call-with-chaos
   (make-gui #:width width	 	 	 	 
             #:height height)
   (λ () (play-bumpo))))
