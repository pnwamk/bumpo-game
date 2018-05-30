#lang typed/racket/base

(require typed/2htdp/image
         racket/match
         "loc.rkt"
         "logic.rkt")


(provide image-coord->location
         render-state)

(define pi 3.141592653)
(: sqr (-> Real Nonnegative-Real))
(define (sqr x)
  (if (negative? x)
      (let ([x (* -1 x)]) (* x x))
      (* x x)))

(struct Posn ([x : Real] [y : Real]) #:transparent)

(define board-color (color 139 69 19))
(define board-side-length 225)
(define board-base-image
  (regular-polygon board-side-length 8 'solid board-color))
(define board-width (image-width board-base-image))
(define board-height (image-height board-base-image))
(define board-center (Posn (/ board-width 2) (/ board-height 2)))
(define cell-width (/ board-width 15))
(define cell-height (/ board-height 15))
(define circle-radius 12)
(define inner-circle-radius 7)
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
  (overlay (circle inner-circle-radius "solid" "white")
           (circle (sub1 circle-radius) "solid" green-color)
           (circle circle-radius "solid" "black")))
(define yellow-circle
  (overlay (circle inner-circle-radius "solid" "white")
           (circle (sub1 circle-radius) "solid" yellow-color)
           (circle circle-radius "solid" "black")))
(define red-circle
  (overlay (circle inner-circle-radius "solid" "white")
           (circle (sub1 circle-radius) "solid" red-color)
           (circle circle-radius "solid" "black")))
(define blue-circle
  (overlay (circle inner-circle-radius "solid" "white")
           (circle (sub1 circle-radius) "solid" blue-color)
           (circle circle-radius "solid" "black")))
(define center-circle
  (overlay (circle (+ inner-circle-radius 0) "solid" "white")
           (circle (+ inner-circle-radius 1) "solid" "blue")
           (circle (+ inner-circle-radius 2) "solid" "green")
           (circle (+ inner-circle-radius 3) "solid" "orange")
           (circle (+ inner-circle-radius 4) "solid" "red")
           (circle (+ inner-circle-radius 5) "solid" "black")))

;; marbles
(define green-marble
  (scale 1/4 (bitmap/file "img/green.png")))
(define blue-marble
  (scale 1/4 (bitmap/file "img/blue.png")))
(define yellow-marble
  (scale 1/4 (bitmap/file "img/yellow.png")))
(define red-marble
  (scale 1/4 (bitmap/file "img/red.png")))
(: highlight-marble (-> (U Image-Color Pen) Image))
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

(: dice-image (-> Natural Image))
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


(: place-image/posns (-> Image (Listof (Pairof Image Posn)) Image))
(define (place-image/posns base image/locs)
  (foldr (λ ([img/posn : (Pairof Image Posn)]
             [image : Image])
           (place-image (car img/posn)
                        (Posn-x (cdr img/posn))
                        (Posn-y (cdr img/posn))
                        image))
         base
         image/locs))

(: image->cartesian-x (-> Real Real))
(define (image->cartesian-x x) (- x (/ board-width 2)))
(: cartesian->image-x (-> Real Real))
(define (cartesian->image-x x) (+ x (/ board-width 2)))
(: image->cartesian-y (-> Real Real))
(define (image->cartesian-y y) (- (/ board-height 2) y))
(: cartesian->image-y (-> Real Real))
(define (cartesian->image-y y) (- (/ board-height 2) y))

(: cartesian->image-point (-> Posn Posn))
(define (cartesian->image-point p)
  (Posn (cartesian->image-x (Posn-x p))
        (cartesian->image-y (Posn-y p))))


(define center-posn
  (Posn (cartesian->image-x 0)
        (cartesian->image-y 0)))

(: rotate (-> Real (-> Posn Posn)))
(define ((rotate θ) p)
  (match-define (Posn x y) p)
  (Posn (- (* x (cos θ)) (* y (sin θ)))
        (+ (* y (cos θ)) (* x (sin θ)))))

(define rotate-1/2-pi (rotate (* (/ 1 2) pi)))
(define rotate-3/2-pi (rotate (* (/ 3 2) pi)))
(define rotate-pi (rotate pi))

(: quadrant-circle+translation (-> Natural (Values Image (-> Posn Posn))))
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

(: qloc (-> Real Real Posn))
(define (qloc x y)
  (Posn (* x cell-width)
        (* y cell-height)))

(: generate-goal (-> Z4 (Listof (Pairof Image Posn))))
(define (generate-goal i)
  (define-values (quadrant-circle translate)
    (quadrant-circle+translation i))
  (reverse
   (build-list
    4
    (λ ([i : Natural])
      (cons quadrant-circle
            (translate
             (qloc 0 (+ 2 i))))))))

(: generate-coords (-> Z4 (Listof (Pairof Image Posn))))
(define (generate-coords i)
  (define-values (quadrant-circle translate)
    (quadrant-circle+translation i))
  (append
   (build-list
    6
    (λ ([i : Natural])
      (cons (if (zero? i)
                quadrant-circle
                white-circle)
            (translate
             (qloc 1 (- 6 i))))))
   (build-list
    5
    (λ ([i : Natural])
      (cons white-circle
            (translate
             (qloc (+ 2 i) 1)))))
   (list (cons white-circle
               (translate (qloc 6 0))))))

(: generate-home (-> Z4 (Listof (Pairof Image Posn))))
(define (generate-home i)
  (define-values (quadrant-circle translate)
    (quadrant-circle+translation i))
  (build-list
   4
   (λ ([i : Natural])
     (cons quadrant-circle
           (translate (qloc (+ 3 i)
                            (- 6 i)))))))

(struct ImageQuadrant
  ([idx : Z4]
   [home : (Listof (Pairof Image Posn))]
   [goal : (Listof (Pairof Image Posn))]
   [coords : (Listof (Pairof Image Posn))])
  #:transparent)

(: generate-quadrant (-> Z4 ImageQuadrant))
(define (generate-quadrant i)
  (ImageQuadrant i
                 (generate-home i)
                 (generate-goal i)
                 (generate-coords i)))

;; (listof quad?)
(define board-quadrants : (Listof ImageQuadrant)
  (list (generate-quadrant 0)
        (generate-quadrant 1)
        (generate-quadrant 2)
        (generate-quadrant 3)))

(: draw-quadrant (-> ImageQuadrant Image Image))
(define (draw-quadrant q base)
  (match-define (ImageQuadrant idx h g locs) q)
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

(: distance (case->
             (-> Posn Posn Real)
             (-> Posn Real Real Real)
             (-> Real Real Posn Real)
             (-> Real Real Real Real Real)))
(define distance
  (case-lambda
    [(p1 p2)
     (match* (p1 p2)
       [((Posn x1 y1) (Posn x2 y2)) (distance x1 y1 x2 y2)])]
    [(arg1 arg2 arg3)
     (match* (arg1 arg2 arg3)
       [((Posn x1 y1) x2 y2) (distance x1 y1 x2 y2)]
       [(x1 y1 (Posn x2 y2)) (distance x1 y1 x2 y2)])]
    [(x1 y1 x2 y2) (sqrt (+ (sqr (- x2 x1))
                            (sqr (- y2 y1))))]))


(: image-coord->quadrant-loc (-> Real Real
                                 (-> ImageQuadrant (U Loc #f))))
(define ((image-coord->quadrant-loc x y) q)
  (define q-idx (ImageQuadrant-idx q))
  (or (for/or : (U Home #f)
        ([img/posn : (Pairof Image Posn) (in-list (ImageQuadrant-home q))]
         [idx : Z4 (in-list z4s)]
         #:when (<= (distance x y (cdr img/posn))
                    circle-radius))
        (Home q-idx idx))
      (for/or : (U Goal #f)
        ([img/posn : (Pairof Image Posn) (in-list (ImageQuadrant-goal q))]
         [idx : Z4 (in-list z4s)]
         #:when (<= (distance x y (cdr img/posn))
                    circle-radius))
        (Goal q-idx idx))
      (for/or : (U Coord #f)
        ([img/posn : (Pairof Image Posn) (in-list (ImageQuadrant-coords q))]
         [idx : Z12 (in-list z12s)]
         #:when (<= (distance x y (cdr img/posn))
                    circle-radius))
        (Coord q-idx idx))))

;; checks if an x,y is a board location
;; (sans the start locations), return #f if not
(: image-coord->location (-> Real Real (U Loc #f)))
(define (image-coord->location x y)
  (cond
    [(<= (distance x y
                   (/ board-width 2)
                   (/ board-height 2))
         circle-radius)
     center]
    [else
     (ormap (image-coord->quadrant-loc x y) board-quadrants)]))


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

(: draw-dice-on-board (-> GameState Image Image))
(define (draw-dice-on-board s board)
  (define val (die s))
  (define t (turn s))
  (define d-img (dice-image val))
  (place-image
   d-img
   (match t
     [(or 0 1) (- board-width (+ 5 (image-width d-img)))]
     [(or 2 3) (+ 5 (image-width d-img))])
   (match t
     [(or 0 3) (+ 5 (image-width d-img))]
     [(or 1 2) (- board-height (+ 5 (image-width d-img)))])
   board))


(: loc->image-posn (-> Loc Posn))
(define (loc->image-posn l)
  (cond
    [(Center? l) center-posn]
    [(Home? l)
     (cdr (list-ref (ImageQuadrant-home
                     (list-ref board-quadrants (Home-quad l)))
                    (Home-idx l)))]
    [(Goal? l)
     (cdr (list-ref (ImageQuadrant-goal
                     (list-ref board-quadrants (Goal-quad l)))
                    (Goal-idx l)))]
    [else
     (cdr (list-ref (ImageQuadrant-coords
                     (list-ref board-quadrants (Coord-quad l)))
                    (Coord-idx l)))]))


(: draw-marbles-on-board (-> GameState Image Image))
(define (draw-marbles-on-board s base-image)
  (define current-turn (turn s))
  (define sel-marble (selected-marble s))
  (foldl (λ ([player-idx : Z4]
             [img : Image])
           (define marble-image (player-num->marble-image player-idx))
           (define highlight-image (player-num->highlight-marble-image current-turn))
           (foldl (λ ([marble-idx : Z4]
                      [img : Image])
                    (define m (Marble player-idx marble-idx))
                    (match-define (Posn x y) (loc->image-posn (board-ref s m)))
                    (cond
                      [(and (= player-idx current-turn)
                            (equal? m sel-marble))
                       (cond
                         ;; if it's moving, we draw it in a later pass
                         [(selected-move-loc s) img]
                         [else
                          (place-image (overlay marble-image highlight-image)
                                       x y
                                       img)])]
                      [else
                       (place-image marble-image x y img)]))
                  img
                  z4s))
         base-image
         z4s))

(: draw-selected-marble-on-board (-> GameState Image Image))
(define (draw-selected-marble-on-board s base-image)
  (define current-turn (turn s))
  (define sel-marble (selected-marble s))
  (cond
    ;; no selected marble -- we're done
    [(not sel-marble) base-image]
    ;; the selected marble is moving, render it over
    ;; the correct location
    [(selected-move-loc s)
     => (λ ([sel-marble-movement-loc : Loc])
          (match-define (Posn moving-x moving-y)
            (loc->image-posn sel-marble-movement-loc))
          (define marble-at-loc (board-ref s sel-marble-movement-loc))
          ;; if the location is occupied, raise the marble slightly
          (place-image (player-num->marble-image (Marble-player sel-marble))
                       moving-x
                       (if (and marble-at-loc (not (equal? sel-marble marble-at-loc)))
                           (- moving-y 5)
                           moving-y)
                       base-image))]
    [else
     ;; a marble is selected but not moving,
     ;; let's render the paths it can take
     (foldl
      (λ ([dest : Dest]
          [img : Image])
        (render-path img
                     current-turn
                     (board-ref s sel-marble)
                     dest))
      base-image
      (possible-destinations s sel-marble (die s)))]))


(define green-pen (make-pen green-color 5 "solid" "round" "round"))
(define yellow-pen (make-pen yellow-color 5 "solid" "round" "round"))
(define red-pen (make-pen red-color 5 "solid" "round" "round"))
(define blue-pen (make-pen blue-color 5 "solid" "round" "round"))

(: player->path-pen (-> Z4 Pen))
(define (player->path-pen player)
  (cond
    [(= player 0) green-pen]
    [(= player 1) yellow-pen]
    [(= player 2) red-pen]
    [else blue-pen]))

(define green-end-circle (circle 6 'solid green-color))
(define yellow-end-circle (circle 6 'solid yellow-color))
(define red-end-circle (circle 6 'solid red-color))
(define blue-end-circle (circle 6 'solid blue-color))

(: player->path-end-circle (-> Z4 Image))
(define (player->path-end-circle player)
  (cond
    [(= player 0) green-end-circle]
    [(= player 1) yellow-end-circle]
    [(= player 2) red-end-circle]
    [else blue-end-circle]))

(: render-path (-> Image Z4 Loc Dest Image))
(define (render-path img player start dest)
  (let loop ([img : Image img]
             [cur : Loc start])
    (define next (next-loc player cur dest))
    (cond
      [(not next)
       (match-define (Posn end-x end-y) (loc->image-posn cur))
       (place-image (player->path-end-circle player) end-x end-y img)]
      [else
       (match-define (Posn cur-x cur-y) (loc->image-posn cur))
       (match-define (Posn next-x next-y) (loc->image-posn next))
       (loop (add-line img cur-x cur-y next-x next-y (player->path-pen player))
             next)])))


(: render-state (-> GameState Image))
(define (render-state s)
  (let* ([img (draw-dice-on-board s bumpo-board-base-image)]
         [img (draw-marbles-on-board s img)]
         [img (draw-selected-marble-on-board s img)])
    img))