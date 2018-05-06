#lang racket/base

(require define-with-spec
         2htdp/image
         2htdp/universe
         racket/list
         racket/match)

;; TODO
;; - placing undefined identifiers in specs doesn't
;;   cause a compile-time error...
;; - use fewer lists? -- if we use structs
;;   everywhere we can get constant-time predicate
;;   checks

(define pi 3.141592653589793)

;; 6 lands you at the tip of your corner
;; 3 accross on the goal road

;; the board has 4 quadrants
;; and a "center location" C
;;       |
;;  3    |   0
;;       |       
;;- - -  C  - - - 
;;       |
;;  2    |   1
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
       (<= 0 x 3)))
(define player-index? quadrant-index?)
(define marble-index? quadrant-index?)
(define (position-index? x)
  (and (exact-integer? x)
       (<= -4 x 11)))
(define (dice-val? x)
  (and (exact-integer? x)
       (<= 1 x 6)))
(define (move-val? x)
  (and (exact-integer? x)
       (or (<= 1 x 6)
           (eqv? x 10))))

(struct/spec home ([quadrant quadrant-index?]
                   [position marble-index?]))
(struct/spec coord ([quadrant quadrant-index?]
                    [position position-index?]))


(define (location? x)
  (match x
    ['center #t]
    [(? home?) #t]
    [(? coord?) #t]
    [_ #f]))

;; i.e. locations that can actually
;; be moved to
(define (action-location? x)
  (match x
    ['center #t] ;; center
    [(? coord?) #t] ;; board coordinate
    [_ #f]))

;; is this location a goal state for
;; some player?
(define (goal-location? x)
  (match x
    [(coord _ pos) (negative? pos)]
    [_ #f]))

(struct/spec player ([human? boolean?]
                     [marbles (list location?
                                    location?
                                    location?
                                    location?)]))


(struct/spec posn ([x real?] [y real?]))

;; dimensions
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
(define (marble-image color1 color2 sides)
  (overlay
   (radial-star sides 4 (- circle-radius 2) "solid" color2)
   (circle (- circle-radius 2) 'solid color1)
   (circle circle-radius 'solid color2)))
(define green-marble
  (marble-image (color 0 255 0) (color 0 128 0) 4))
(define blue-marble
  (marble-image (color 0 255 255) "blue" 6))
(define yellow-marble
  (marble-image (color 255 255 0) "goldenrod" 8))
(define red-marble
  (marble-image (color 255 0 0) (color 150 0 0) 10))
(define (highlight-marble color1)
  (radial-star 20 (add1 circle-radius) (+ 4 circle-radius) "outline" color1))
(define highlight-green-marble (highlight-marble "green"))
(define highlight-yellow-marble (highlight-marble "yellow"))
(define highlight-red-marble (highlight-marble "red"))
(define highlight-blue-marble (highlight-marble "blue"))




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

(struct/spec quadrant ([home (listof (cons image? posn?))]
                       [locations (listof (cons image? posn?))]))

;; (listof quad?)
(define board-quadrants
  (for/list ([i (in-range 4)])
    (quadrant (generate-home i)
              (generate-quadrant i))))

(define/spec (draw-quadrant q base)
  (-> quadrant? image? image?)
  (match-define (quadrant h locs) q)
  (place-image/posns
   (place-image/posns
    base
    locs)
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
  (-> real? real? (either #f location?))
  (cond
    [(<= (distance x y
                   (/ board-width 2)
                   (/ board-height 2))
         circle-radius)
     'center]
    [else
     (for/or ([q (in-list board-quadrants)]
              [q-idx (in-range 4)])
       (or (for/or ([img/posn (in-list (quadrant-home q))]
                    [idx (in-range 4)]
                    #:when (<= (distance x y (cdr img/posn))
                               circle-radius))
             (home q-idx idx))
           (for/or ([img/posn (in-list (quadrant-locations q))]
                    [idx (in-range -4 12)]
                    #:when (<= (distance x y (cdr img/posn))
                               circle-radius))
             (coord q-idx idx))))]))

(define/spec (player-has-marble-at-location? players turn clicked-location)
  (-> (list player? player? player? player?)
      player-index?
      location?
      (either #f marble-index?))
  (for/or ([marble-loc (in-list (player-marbles (list-ref players turn)))]
           [idx (in-naturals)])
    (cond
      [(equal? marble-loc clicked-location) idx]
      [else #f])))

(define (handle-mouse state x y mevent)
  (cond
    [(not (mouse=? mevent "button-up"))
     state]
    [else
     (define clicked-location (image-coord->location x y))
     (match-define (game-state turn players sel dice pmoves) state)
     (cond
       [(not clicked-location)
        (game-state turn players #f dice pmoves)]
       [(player-has-marble-at-location? players turn clicked-location)
        ;; select that marble
        => (λ (marble-idx) (game-state turn players marble-idx dice pmoves))]
       [else
        ;; TODO
        ;; - if player clicks on possible move,
        ;;   move there and advance the turn.
        (game-state turn players #f dice pmoves)])]))


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



(define/spec (draw-world state)
  (-> game-state? image?)
  (define sel-marble (game-state-selected-marble state))
  (for/fold ([img bumpo-board-base-image])
            ([p (in-list (game-state-players state))]
             [player-idx (in-naturals)])
    (define marble-image
      (player-num->marble-image player-idx))
    (define highlight-image
      (player-num->highlight-marble-image player-idx))
    (for/fold ([img img])
              ([loc (in-list (player-marbles p))]
               [marble-idx (in-naturals)])
      (define marble-posn
        (match loc
          ['center center-posn]
          [(? home?)
           (cdr (list-ref (quadrant-home
                           (list-ref board-quadrants player-idx))
                          marble-idx))]
          [(coord q-idx loc-idx)
           (cdr (list-ref (quadrant-locations
                           (list-ref board-quadrants q-idx))
                          (+ loc-idx 4)))]))
      (match-define (posn x y) marble-posn)
      (cond
        [(and (eqv? player-idx (game-state-turn state))
              (eqv? marble-idx sel-marble))
         (let ([marble-image (overlay marble-image highlight-image)])
           (place-image marble-image x y img))]
        [else
         (place-image marble-image x y img)]))))

(struct/spec game-state
             ([turn player-index?]
              [players (list player? player? player? player?)]
              [selected-marble (either #f marble-index?)]
              [dice (listof move-val?)]
              [possible-moves (listof (cons marble-index?
                                            action-location?))]))

;; does a player have all of their marbles
;; in the safe home zone
(define (player-done? p)
  (andmap goal-location? (player-marbles p)))


(struct/spec move ([marble marble-index?]
                   [destination location?]))

(define/spec (occupied? loc players)
  (-> action-location?
      (list player? player? player? player?)
      (either #f player-index?))
  (for/or ([p (in-list players)]
           [idx (in-range 4)])
    (and (for/or ([mloc (in-list (player-marbles p))])
           (equal? loc mloc))
         idx)))

(define/spec (predecessor-quadrant player-num)
  (-> player-index?
      player-index?)
  (modulo (- player-num 1) 4))

(define/spec (increment-quadrant q)
  (-> player-index?
      player-index?)
  (modulo (+ q 1) 4))

(define/spec (moves-from-location turn n loc)
  (-> player-index?
      move-val?
      location?
      (listof action-location?))
  (match* (loc n)
    [((? home?) 1) (list (coord turn 1))]
    [((? home?) 6) (list (coord turn 6))]
    [((? home?) _) (list)]
    [('center 1) (list (coord (predecessor-quadrant turn) 6))]
    [('center _) (list)]
    [((coord q pos) _)
     (cond
       ;; marble is already in either the goal or the location
       ;; 1 before the goal -- check if we can move the marble
       ;; into a valid goal spot.
       [(and (= q turn) (<= pos 0))
        (define n* (- pos n))
        (cond
          [(<= -4 n*) (list (coord turn n*))]
          [else (list)])]
       [else
        ;; each quadrant has 12 non-goal locations, so mod by 12
        (define-values (overflow pos*) (quotient/remainder (+ n pos) 12))
        (cond
          ;; when movement did not result in changing quadrants
          [(zero? overflow)
           (cond
             ;; the special case when a marble may enter
             ;; the center location
             [(and (eqv? q turn) (eqv? pos* 7))
              (list 'center (coord turn 7))]
             [else (list (coord q pos*))])]
          ;; when movement did result in overflow but
          ;; we weren't in the predecessor quadrant yet
          ;; (and so we don't have to worry about rounding
          ;; home plate, so to speak.
          [(not (eqv? q (predecessor-quadrant turn)))
           (list (coord (increment-quadrant q) pos*))]
          ;; when movement did result in overlow, we
          ;; were in the predecessor quadrant, but
          ;; the movement was too far.
          [(< 4 pos*) (list)]
          ;; when movement did result in overlow, we
          ;; were in the predecessor quadrant, and
          ;; the movement wasn't too far.
          [else (list (coord turn (- pos*)))])])]))

(define/spec (possible-player-moves players turn n)
  (-> (list player? player? player? player?)
      player-index?
      move-val?
      (listof move?))
  (for/fold ([possible-moves '()])
            ([marble-idx (in-range 4)])
    (define loc (list-ref (player-marbles (list-ref players turn)) marble-idx))
    (define locs
      (for/list ([loc* (in-list (moves-from-location turn n loc))]
                 ;; drop movements which land on a players own marble
                 ;; or when they land on a player's marble in their safe spot
                 #:when (match (occupied? loc* players)
                          [(== turn) #f]
                          [other (match loc*
                                   [(coord (== other) 1) #f]
                                   [_ #t])]))
        (move marble-idx loc*)))
    (append locs possible-moves)))

(define/spec (update-players/movements players movements)
  (-> (list player? player? player? player?)
      (listof (cons player-index? move?))
      (list player? player? player? player?))
  (for/fold ([players players])
            ([movement (in-list movements)])
    (match-define (cons player-idx (move marble-idx dest)) movement)
    (match (list-ref players player-idx)
      [(player human? marbles)
       (list-set players player-idx
                 (player human? (list-set marbles marble-idx dest)))])))



(module+ test
  (require rackunit
           racket/set)
  (define-binary-check (check-set=? set=? actual expected))

  ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ;; player-moves tests
  ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  (let ([ps (list (player #t (list (home 0 0)
                                   (home 0 1)
                                   (coord 0 6)
                                   (coord 2 6)))
                  (player #t (list (home 1 0) (home 1 1) (home 1 2) (home 1 3)))
                  (player #t (list (home 2 0) (home 2 1) (home 2 2) (home 2 3)))
                  (player #t (list (home 3 0) (home 3 1) (home 3 2) (home 3 3))))])
    ;; errors for bad movement values (i.e. dice can only be 1-6
    ;; or you can move 10 for stomping someone)
    (check-exn (λ (_) #t) (λ () (possible-player-moves ps 0 0)))
    (check-exn (λ (_) #t) (λ () (possible-player-moves ps 0 7)))
    (check-exn (λ (_) #t) (λ () (possible-player-moves ps 0 11)))
    (check-set=? (possible-player-moves ps 0 1)
                 (list (move 0 (coord 0 1))
                       (move 1 (coord 0 1))
                       (move 2 (coord 0 7))
                       (move 2 'center)
                       (move 3 (coord 2 7))))
    (check-set=? (possible-player-moves ps 0 2)
                 (list (move 2 (coord 0 8))
                       (move 3 (coord 2 8))))
    (check-set=? (possible-player-moves ps 0 10)
                 (list (move 2 (coord 1 4))
                       (move 3 (coord 3 4)))))

  (let ([ps (list (player #t (list (home 0 0)
                                   (coord 0 1)
                                   (coord 0 11)
                                   (coord 3 10)))
                  (player #t (list (coord 1 1) (home 1 1) (home 1 2) (home 1 3)))
                  (player #t (list (home 2 0) (home 2 1) (home 2 2) (home 2 3)))
                  (player #t (list (home 3 0) (home 3 1) (home 3 2) (home 3 3))))])
    (check-set=? (possible-player-moves ps 0 1)
                 (list (move 1 (coord 0 2))
                       (move 2 (coord 1 0))
                       (move 3 (coord 3 11))))
    (check-set=? (possible-player-moves ps 0 2)
                 (list (move 1 (coord 0 3))
                       (move 3 (coord 0 0))))
    (check-set=? (possible-player-moves ps 0 3)
                 (list (move 1 (coord 0 4))
                       (move 2 (coord 1 2))
                       (move 3 (coord 0 -1))))
    (check-set=? (possible-player-moves ps 0 4)
                 (list (move 1 (coord 0 5))
                       (move 2 (coord 1 3))
                       (move 3 (coord 0 -2))))
    (check-set=? (possible-player-moves ps 0 5)
                 (list (move 1 (coord 0 6))
                       (move 2 (coord 1 4))
                       (move 3 (coord 0 -3))))
    (check-set=? (possible-player-moves ps 0 6)
                 (list (move 0 (coord 0 6))
                       (move 1 'center)
                       (move 1 (coord 0 7))
                       (move 2 (coord 1 5))
                       (move 3 (coord 0 -4))))
    (check-set=? (possible-player-moves ps 0 10)
                 (list (move 2 (coord 1 9)))))
  (let ([ps (list (player #t (list (coord 0 1) (home 0 1) (home 0 2) (home 0 3)))
                  (player #t (list (home 1 0)  (home 1 1) (home 1 2) (home 1 3)))
                  (player #t (list (home 2 0)  (home 2 1) (home 2 2) (home 2 3)))
                  (player #t (list (home 3 0)
                                   (coord 3 1)
                                   (coord 3 11)
                                   (coord 2 10))))])
    (check-set=? (possible-player-moves ps 3 1)
                 (list (move 1 (coord 3 2))
                       (move 2 (coord 0 0))
                       (move 3 (coord 2 11))))
    (check-set=? (possible-player-moves ps 3 2)
                 (list (move 1 (coord 3 3))
                       (move 3 (coord 3 0))))
    (check-set=? (possible-player-moves ps 3 3)
                 (list (move 1 (coord 3 4))
                       (move 2 (coord 0 2))
                       (move 3 (coord 3 -1))))
    (check-set=? (possible-player-moves ps 3 4)
                 (list (move 1 (coord 3 5))
                       (move 2 (coord 0 3))
                       (move 3 (coord 3 -2))))
    (check-set=? (possible-player-moves ps 3 5)
                 (list (move 1 (coord 3 6))
                       (move 2 (coord 0 4))
                       (move 3 (coord 3 -3))))
    (check-set=? (possible-player-moves ps 3 6)
                 (list (move 0 (coord 3 6))
                       (move 1 'center)
                       (move 1 (coord 3 7))
                       (move 2 (coord 0 5))
                       (move 3 (coord 3 -4))))
    (check-set=? (possible-player-moves ps 3 10)
                 (list (move 2 (coord 0 9))))))

(define initial-game-state
  (game-state 0 (list (player #t (list (home 0 0)
                                       (coord 1 2)
                                       (coord 2 10)
                                       (coord 3 0)))
                      (player #t (list (home 1 0) (home 1 1) (home 1 2) (home 1 3)))
                      (player #t (list (home 2 0) (home 2 1) (home 2 2) (home 2 3)))
                      (player #t (list (home 3 0) (home 3 1) (home 3 2) (home 3 3))))
              #f
              '()
              '()))

(big-bang initial-game-state
  [to-draw draw-world]
  [on-mouse handle-mouse])


