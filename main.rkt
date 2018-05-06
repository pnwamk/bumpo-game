#lang racket/base

(require define-with-spec
         2htdp/image
         2htdp/universe
         racket/match)

(define pi 3.141592653589793)

;; 6 lands you at the tip of your corner
;; 3 accross on the goal road

;; the board has 4 quadrants
;; and a "center location" C
;;       |
;;  4    |   1
;;       |       
;;- - -  C  - - - 
;;       |
;;  3    |   2
;;       |

;; each quadrant (except 0) has 16 locations,
;; here is quadrant 1 for an example:
;;- - - - - - - - - - - |
;;  0  1                |
;; -1  2                |
;; -2  3                |
;; -3  4                |
;; -4  5                |
;;     6 7 8 9 10 11    |
;; #t                   |
;;- - - - - - - - - - - |
;;
;; Note: positions -1 through -4 are the safe
;; goal positions, position 1 is the first position
;; that a marble lands on after leaving home base,
;; and #t is the center position on the board

(define (quadrant-index? x)
  (and (exact-nonnegative-integer? x)
       (<= 1 x 4)))
(define (position-index? x)
  (and (exact-integer? x)
       (<= -4 x 11)))

(struct/spec coord ([quadrant quadrant-index?]
                    [position position-index?]))

(define (location? x)
  (match x
    ['start #t] ;; home base
    ['center #t] ;; center
    [(? coord?) #t] ;; board coordinate
    [_ #f]))

(struct/spec player ([name string?]
                     [human? boolean?]
                     [marbles (list location?
                                    location?
                                    location?
                                    location?)]))


(struct/spec posn ([x real?] [y real?]))

;; dimensions
(define board-color (color 139 69 19))
(define board-side-length 225)
(define board-base (regular-polygon board-side-length 8 'solid board-color))
(define board-width (image-width board-base))
(define board-height (image-height board-base))
(define board-center (posn (/ board-width 2) (/ board-height 2)))
(define cell-width (/ board-width 15))
(define cell-height (/ board-height 15))
(define circle-radius 12)
;; colors
(define green-color (color 0 204 0))
(define yellow-color (color 200 200 0))
(define red-color (color 204 0 0))
(define blue-color (color 0 102 204))
;; board circles
(define white-circle
  (overlay (circle (sub1 circle-radius) "solid" "white")
           (circle circle-radius "solid" "black")))
(define green-circle
  (overlay (circle (- circle-radius 5) "solid" "white")
           (circle (sub1 circle-radius) "solid" green-color)
           (circle circle-radius "solid" "black")))
(define yellow-circle
  (overlay (circle (- circle-radius 5) "solid" "white")
           (circle (sub1 circle-radius) "solid" yellow-color)
           (circle circle-radius "solid" "black")))
(define red-circle
  (overlay (circle (- circle-radius 5) "solid" "white")
           (circle (sub1 circle-radius) "solid" red-color)
           (circle circle-radius "solid" "black")))
(define blue-circle
  (overlay (circle (- circle-radius 5) "solid" "white")
           (circle (sub1 circle-radius) "solid" blue-color)
           (circle circle-radius "solid" "black")))
(define center-circle
  (overlay
   (circle 4 "solid" "white")
   (circle 5 "solid" "violet")
   (circle 6 "solid" "blue")
   (circle 7 "solid" "green")
   (circle 8 "solid" "yellow")
   (circle 9 "solid" "orange")
   (circle 10 "solid" "red")))
;; marbles
(define (marble-image color1 color2 sides)
  (overlay
   (radial-star sides 4 (- circle-radius 2) "solid" color2)
   (circle (- circle-radius 2) 'solid color1)
   (circle circle-radius 'solid "black")))
(define green-marble
  (marble-image (color 0 255 0) (color 0 128 0) 4))
(define blue-marble
  (marble-image (color 0 255 255) "blue" 6))
(define yellow-marble
  (marble-image (color 255 255 0) "goldenrod" 8))
(define red-marble
  (marble-image (color 255 0 0) (color 150 0 0) 10))



(define/spec (place-image/locs base image/locs)
  (-> image? (listof (cons image? posn?)) image?)
  (for/fold ([img base])
            ([i/l (in-list image/locs)])
    (match-define (posn x y) (cdr i/l))
    (place-image (car i/l)
                 x y
                 img)))

(define (image->cartesian-x x)
  (- x (/ board-width 2)))
(define (cartesian->image-x x)
  (+ x (/ board-width 2)))
(define (image->cartesian-y y)
  (- (/ board-height 2) y))
(define (cartesian->image-y y)
  (- (/ board-height 2) y))

(define (cartesian->image-point p)
  (posn (cartesian->image-x (posn-x p))
        (cartesian->image-y (posn-y p))))

(define ((rotate θ) p)
  (match-define (posn x y) p)
  (posn (- (* x (cos θ)) (* y (sin θ)))
        (+ (* y (cos θ)) (* x (sin θ)))))

(define rotate-1/2-pi (rotate (* (/ 1 2) pi)))
(define rotate-3/2-pi (rotate (* (/ 3 2) pi)))
(define rotate-pi (rotate pi))

(define (quadrant-circle+translation i)
  (match i
    [1 (values green-circle
               cartesian->image-point)]
    [2 (values yellow-circle
               (λ (p) (cartesian->image-point
                       (rotate-3/2-pi p))))]
    [3 (values red-circle
               (λ (p) (cartesian->image-point
                       (rotate-pi p))))]
    [4 (values blue-circle
               (λ (p) (cartesian->image-point
                       (rotate-1/2-pi p))))]))

(define (qloc x y)
  (posn (* x cell-width)
        (* y cell-height)))

(define/spec (generate-quadrant i)
  (-> exact-nonnegative-integer?
      (listof (cons image? posn?)))
  (define-values (quadrant-circle translate)
    (quadrant-circle+translation i))
  (append
   (build-list
    5
    (λ (i) (cons (if (= i 4)
                     white-circle
                     quadrant-circle)
                 (translate
                  (qloc 0 (+ 2 i))))))
   (build-list
    6
    (λ (i) (cons (if (zero? i)
                     quadrant-circle
                     white-circle)
                 (translate
                  (qloc 1 (- 6 i))))))
   (build-list
    5
    (λ (i) (cons white-circle
                 (translate
                  (qloc (+ 2 i) 1)))))))

(define/spec (generate-home i)
  (-> exact-nonnegative-integer?
      (listof (cons image? posn?)))
  (define-values (quadrant-circle translate)
    (quadrant-circle+translation i))1
  (for/list ([i (in-range 4)])
    (cons quadrant-circle
          (translate (qloc (+ 3 i)
                           (- 6 i))))))

;; board-quadrants : (listof (listof (cons image? posn?)))
(define board-quadrants
  (list (generate-quadrant 1)
        (generate-quadrant 2)
        (generate-quadrant 3)
        (generate-quadrant 4)))

(define bumpo-board
  (place-image/locs
   board-base
   (cons
    (cons center-circle
          (cartesian->image-point
           (posn 0 0)))
    (append
     (generate-home 1)
     (generate-home 2)
     (generate-home 3)
     (generate-home 4)
     (apply
      append
      board-quadrants)))))

(define distance
  (case-lambda
    [(p1 p2)
     (match* (p1 p2)
       [((posn x1 y1) (posn x2 y2)) (distance x1 y1 x2 y2)])]
    [(arg1 arg2 arg3)
     (match* (arg1 arg2 arg3)
       [((posn x1 y1) x2 y2) (distance x1 y1 x2 y2)]
       [(x1 y1 (posn x2 y2)) (distance x1 y1 x2 y2)])]
    [(x1 y1 x2 y2) (sqrt (+ (expt (- x2 x1) 2)
                            (expt (- y2 y1) 2)))]))

;; checks if an x,y is a board location
;; (sans the start locations), return #f
;; if not
(define/spec (image-coord->location x y)
  (-> real? real? (either #f 'center coord?))
  (cond
    [(<= (distance x y
                   (/ board-width 2)
                   (/ board-height 2))
         circle-radius)
     'center]
    [else
     (for/or ([q (in-list board-quadrants)]
              [q-idx (in-range 1 5)])
       (for/or ([img/posn (in-list q)]
                [idx (in-range -4 12)]
                #:when (<= (distance x y (cdr img/posn))
                           circle-radius))
         (coord q-idx idx)))]))

(define (handle-mouse world x y mevent)
  (when (mouse=? mevent "button-down")
    (eprintf "handle-mouse x: ~a y: ~a --> ~a\n"
             x y (image-coord->location x y)))
  world)


(big-bang #t
  [to-draw (λ (world) bumpo-board)]
  [on-mouse handle-mouse])


