#lang racket/base

(require define-with-spec
         2htdp/image
         2htdp/universe
         racket/list
         racket/match
         "loc.rkt"
         "logic.rkt")


(provide image-coord->location
         render-state)

(define pi 3.141592653589793)

(struct/spec posn ([x real?] [y real?]))

(define board-color (color 139 69 19))
(define board-side-length 225)
(define board-base-image
  (regular-polygon board-side-length 8 'solid board-color))
(define board-width (image-width board-base-image))
(define board-height (image-height board-base-image))
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
  (for/fold ([img (circle circle-radius "solid" "black")])
            ([c (in-list '("red" "orange" "yellow" "green" "blue" "white"))]
             [n (in-naturals)])
    (overlay
     (circle (- circle-radius n) "solid" c)
     img)))

;; marbles
(define green-marble
  (scale 1/4 (bitmap "img/green.png")))
(define blue-marble
  (scale 1/4 (bitmap "img/blue.png")))
(define yellow-marble
  (scale 1/4 (bitmap "img/yellow.png")))
(define red-marble
  (scale 1/4 (bitmap "img/red.png")))
(define (highlight-marble color1)
  (radial-star 24 (+ 3 circle-radius) (+ 5 circle-radius) "outline" color1))
(define highlight-green-marble (highlight-marble "green"))
(define highlight-yellow-marble (highlight-marble "yellow"))
(define highlight-red-marble (highlight-marble "red"))
(define highlight-blue-marble (highlight-marble "cornflowerblue"))
(define highlight-white (highlight-marble "white"))

;; dice
(define dice-size 40)
(define base-dice-image (overlay (square (sub1 dice-size) "outline" "black")
                                 (square dice-size "solid" "white")))
(define base-dice-dot-image (circle 3 "solid" "black"))
(define base-dice-spacer-image (circle 3 "solid" "white"))
(define (dice-image n)
  (match n
    [1 (overlay base-dice-dot-image
                base-dice-image)]
    [2 (place-image
        base-dice-dot-image
        10 10
        (place-image
         base-dice-dot-image
         (- dice-size 10)
         (- dice-size 10)
         base-dice-image))]
    [3 (place-image
        base-dice-dot-image
        (/ dice-size 2)
        (/ dice-size 2)
        (dice-image 2))]
    [4 (place-image
        base-dice-dot-image
        (- dice-size 10) 10
        (place-image
         base-dice-dot-image
         10 (- dice-size 10)
         (dice-image 2)))]
    [5 (place-image
        base-dice-dot-image
        (/ dice-size 2) (/ dice-size 2)
        (dice-image 4))]
    [6 (place-image
        base-dice-dot-image
        10 10
        (place-image
         base-dice-dot-image
         10 (/ dice-size 2)
         (place-image
          base-dice-dot-image
          10 (- dice-size 10)
          (place-image
           base-dice-dot-image
           (- dice-size 10) 10
           (place-image
            base-dice-dot-image
            (- dice-size 10) (/ dice-size 2)
            (place-image
             base-dice-dot-image
             (- dice-size 10) (- dice-size 10)
             base-dice-image))))))]
    [10 (overlay (text "10" 30 "indigo")
                 base-dice-image)]))



(define/spec (place-image/posns base image/locs)
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

(define center-posn
  (posn (cartesian->image-x 0)
        (cartesian->image-y 0)))

(define ((rotate θ) p)
  (match-define (posn x y) p)
  (posn (- (* x (cos θ)) (* y (sin θ)))
        (+ (* y (cos θ)) (* x (sin θ)))))

(define rotate-1/2-pi (rotate (* (/ 1 2) pi)))
(define rotate-3/2-pi (rotate (* (/ 3 2) pi)))
(define rotate-pi (rotate pi))

(define (quadrant-circle+translation i)
  (match i
    [0 (values green-circle
               cartesian->image-point)]
    [1 (values yellow-circle
               (λ (p) (cartesian->image-point
                       (rotate-3/2-pi p))))]
    [2 (values red-circle
               (λ (p) (cartesian->image-point
                       (rotate-pi p))))]
    [3 (values blue-circle
               (λ (p) (cartesian->image-point
                       (rotate-1/2-pi p))))]))

(define (qloc x y)
  (posn (* x cell-width)
        (* y cell-height)))

(define/spec (generate-goal i)
  (-> exact-nonnegative-integer?
      (listof (cons image? posn?)))
  (define-values (quadrant-circle translate)
    (quadrant-circle+translation i))
  (reverse
   (build-list
    4
    (λ (i) (cons quadrant-circle
                 (translate
                  (qloc 0 (+ 2 i))))))))

(define/spec (generate-quadrant i)
  (-> exact-nonnegative-integer?
      (listof (cons image? posn?)))
  (define-values (quadrant-circle translate)
    (quadrant-circle+translation i))
  (append
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
                  (qloc (+ 2 i) 1)))))
   (list (cons white-circle
               (translate (qloc 6 0))))))

(define/spec (generate-home i)
  (-> exact-nonnegative-integer?
      (listof (cons image? posn?)))
  (define-values (quadrant-circle translate)
    (quadrant-circle+translation i))1
  (for/list ([i (in-range 4)])
    (cons quadrant-circle
          (translate (qloc (+ 3 i)
                           (- 6 i))))))

(struct/spec image-quadrant
             ([home (listof (cons image? posn?))]
              [goal (listof (cons image? posn?))]
              [locations (listof (cons image? posn?))]))

;; (listof quad?)
(define board-quadrants
  (for/list ([i (in-range 4)])
    (image-quadrant (generate-home i)
                    (generate-goal i)
                    (generate-quadrant i))))

(define/spec (draw-quadrant q base)
  (-> image-quadrant? image? image?)
  (match-define (image-quadrant h g locs) q)
  (place-image/posns
   (place-image/posns
    (place-image/posns
     base
     locs)
    g)
   h))

;; bumpo-board ; image
(define bumpo-board-base-image
  (place-image
   center-circle
   (cartesian->image-x 0)
   (cartesian->image-y 0)
   (foldl draw-quadrant
          board-base-image
          board-quadrants)))

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
  (-> real? real? (either #f loc?))
  (cond
    [(<= (distance x y
                   (/ board-width 2)
                   (/ board-height 2))
         circle-radius)
     center]
    [else
     (for/or ([q (in-list board-quadrants)]
              [q-idx (in-range 4)])
       (or (for/or ([img/posn (in-list (image-quadrant-home q))]
                    [idx (in-range 4)]
                    #:when (<= (distance x y (cdr img/posn))
                               circle-radius))
             (home q-idx idx))
           (for/or ([img/posn (in-list (image-quadrant-goal q))]
                    [idx (in-range 4)]
                    #:when (<= (distance x y (cdr img/posn))
                               circle-radius))
             (goal q-idx idx))
           (for/or ([img/posn (in-list (image-quadrant-locations q))]
                    [idx (in-range 12)]
                    #:when (<= (distance x y (cdr img/posn))
                               circle-radius))
             (coord q-idx idx))))]))


(define (player-num->marble-image i)
  (match i
    [0 green-marble]
    [1 yellow-marble]
    [2 red-marble]
    [3 blue-marble]))

(define (player-num->highlight-marble-image i)
  (match i
    [0 highlight-green-marble]
    [1 highlight-yellow-marble]
    [2 highlight-red-marble]
    [3 highlight-blue-marble]))

(define/spec (draw-dice-on-board state board)
  (-> game-state? image? image?)
  (define val (current-die state))
  (define turn (current-turn state))
  (define die (dice-image val))
  (place-image
   die
   (match turn
     [(or 0 1) (- board-width (+ 5 (image-width die)))]
     [(or 2 3) (+ 5 (image-width die))])
   (match turn
     [(or 0 3) (+ 5 (image-width die))]
     [(or 1 2) (- board-height (+ 5 (image-width die)))])
   board))


(define (loc->image-posn l)
  (match l
    [(? center?) center-posn]
    [(? home?)
     (define-values (home-q home-idx) (home->quadrant/index l))
     (cdr (list-ref (image-quadrant-home
                     (list-ref board-quadrants home-q))
                    home-idx))]
    [(? goal?)
     (define-values (q-idx goal-idx) (goal->quadrant/index l))
     (cdr (list-ref (image-quadrant-goal
                     (list-ref board-quadrants q-idx))
                    goal-idx))]
    [(? coord?)
     (define-values (q-idx loc-idx) (coord->quadrant/index l))
     (cdr (list-ref (image-quadrant-locations
                     (list-ref board-quadrants q-idx))
                    loc-idx))]))

(define/spec (render-state s)
  (-> game-state? image?)
  (define sel-marble (selected-marble s)) 
  (define current-player (current-turn s))
  (define highlight-image
    (player-num->highlight-marble-image current-player))
  (define rendered-board-image
    (for/fold ([img (draw-dice-on-board s bumpo-board-base-image)])
              ([player-idx (in-range 4)])
      (define marble-image
        (player-num->marble-image player-idx))
      (for*/fold ([img img])
                 ([m (in-player-marbles player-idx)]
                  [m-loc (in-value (marble-loc s m))])
        (match-define (posn x y) (loc->image-posn m-loc))
        (cond
          [(and (eqv? player-idx (current-turn s))
                (eq? m sel-marble))
           (cond
             ;; if its moving, we draw it later after all
             ;; the other marbles are drawn
             [(and sel-marble (selected-movement-loc s)) img]
             [else
              (let ([marble-image (overlay marble-image highlight-image)])
                (place-image marble-image x y img))])]
          [else
           (place-image marble-image x y img)]))))
  (cond
    ;; no selected marble -- we're done
    [(not sel-marble) rendered-board-image]
    ;; the selected marble is moving, render it over
    ;; the correct location
    [(selected-movement-loc s)
     => (λ (sel-marble-movement-loc)
          (match-define (posn moving-x moving-y)
            (loc->image-posn sel-marble-movement-loc))
          (define marble-at-loc (loc-ref s sel-marble-movement-loc))
          ;; if the location is occupied, raise the marble slightly
          (place-image (player-num->marble-image (marble-player sel-marble))
                       moving-x
                       (if (and marble-at-loc (not (eq? sel-marble marble-at-loc)))
                           (- moving-y 5)
                           moving-y)
                       rendered-board-image))]
    ;; a marble is selected but not moving, let's render
    ;; the paths it can take
    [else
     (define dist (current-die s))
     (define moves (possible-moves s sel-marble dist))
     (match moves
       [#f rendered-board-image]
       [(cons dest1 dest2)
        (render-path (render-path rendered-board-image
                                  current-player
                                  (marble-loc s sel-marble)
                                  dest1)
                     current-player
                     (marble-loc s sel-marble)
                     dest2)]
       [(? dest? dest)
        (render-path rendered-board-image
                     current-player
                     (marble-loc s sel-marble)
                     dest)])]))


(define green-pen (make-pen green-color 5 "solid" "round" "round"))
(define yellow-pen (make-pen yellow-color 5 "solid" "round" "round"))
(define red-pen (make-pen red-color 5 "solid" "round" "round"))
(define blue-pen (make-pen blue-color 5 "solid" "round" "round"))

(define/spec (player->path-pen player)
  (-> quadrant? pen?)
  (match player
    [0 green-pen]
    [1 yellow-pen]
    [2 red-pen]
    [3 blue-pen]))

(define green-end-circle (circle 6 'solid green-color))
(define yellow-end-circle (circle 6 'solid yellow-color))
(define red-end-circle (circle 6 'solid red-color))
(define blue-end-circle (circle 6 'solid blue-color))

(define/spec (player->path-end-circle player)
  (-> quadrant? image?)
  (match player
    [0 green-end-circle]
    [1 yellow-end-circle]
    [2 red-end-circle]
    [3 blue-end-circle]))

(define/spec (render-path img player start dest)
  (-> image? quadrant? loc? dest? image?)
  (let loop ([img img]
             [cur start])
    (define next (next-loc player cur dest))
    (cond
      [(not next)
       (match-define (posn end-x end-y) (loc->image-posn cur))
       (place-image (player->path-end-circle player) end-x end-y img)]
      [else
       (match-define (posn cur-x cur-y) (loc->image-posn cur))
       (match-define (posn next-x next-y) (loc->image-posn next))
       (loop (add-line img cur-x cur-y next-x next-y (player->path-pen player))
             next)])))
