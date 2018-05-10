#lang racket/base

(require define-with-spec
         racket/match
         racket/fixnum)

(provide (all-defined-out))

;; the board has 4 quadrants
;; and a "center location" C
;;       |
;;  3    |   0
;;       |       
;;- - -  C  - - - 
;;       |
;;  2    |   1
;;       |


;; each quadrant has:
;; 12 standard locations (0-11 + q*12),
;; 4 goal locations (1q0-1q3),
;; and 4 home locatinos ('hq0-'hq3)
;; here is quadrant 0 for an example:
;;- - - - - - - - - - - -|
;;        0   h00        |
;;   100  1     h01      |
;;   101  2       h02    |
;;   102  3          h03 |
;;   103  4              |
;;        5 6 7 8 9 10   |
;; #t               11   |
;;                       |
;;- - - - - - - - - - - -|

(define-syntax-rule (fx-range-pred lower-inclusive upper-exclusive)
  (λ (x) (and (fixnum? x)
              (fx<= lower-inclusive x)
              (fx< x upper-exclusive))))

(define quadrant? (fx-range-pred 0 4))
(define coord-index? (fx-range-pred 0 12))
(define home-index? quadrant?)
(define goal-index? quadrant?)
(define coord? (fx-range-pred 0 48))

(define/spec (predecessor-quadrant player-num)
  (-> quadrant? quadrant?)
  (modulo (- player-num 1) 4))

(define/spec (increment-quadrant q)
  (-> quadrant? quadrant?)
  (modulo (+ q 1) 4))

;; if `c` is a safe loc, return the quadrant,
;; otherwise return #f
(define/spec (safe-coord? c)
  (-> coord? quadrant?)
  (and (fx= 0 (fxremainder c 12))
       (fxquotient c 12)))

(define/spec (coord q i)
  (-> quadrant?
      coord-index?
      coord?)
  (fx+ (fx* q 12) i))

(define/spec (coord->quadrant l)
  (-> coord? quadrant?)
  (fxquotient l 12))

(define/spec (coord->index l)
  (-> coord? coord-index?)
  (fxremainder l 12))

(define (coord->quadrant/index l)
  (if (coord? l)
      (values (fxquotient l 12) (fxremainder l 12))
      (error 'coord->quadrant/index "not a coord ~a" l)))

(define home? symbol?)

(define index->home-table
  (vector-immutable
   'h00 'h01 'h02 'h03
   'h10 'h11 'h12 'h13
   'h20 'h21 'h22 'h23
   'h30 'h31 'h32 'h33))

(define/spec (home q i)
  (-> quadrant? home-index? home?)
  (vector-ref index->home-table (+ (arithmetic-shift q 2) i)))

(define home->quadrant-table
  (hasheq 'h00 0 'h01 0 'h02 0 'h03 0
          'h10 1 'h11 1 'h12 1 'h13 1
          'h20 2 'h21 2 'h22 2 'h23 2
          'h30 3 'h31 3 'h32 3 'h33 3))

(define (home->quadrant h)
  (hash-ref home->quadrant-table h
            (λ () (error 'home->quadrant "given non-home!"))))

(define home->index-table
  (hasheq 'h00 0 'h01 1 'h02 2 'h03 3
          'h10 0 'h11 1 'h12 2 'h13 3
          'h20 0 'h21 1 'h22 2 'h23 3
          'h30 0 'h31 1 'h32 2 'h33 3))

(define (home->index h)
  (hash-ref home->index-table h
            (λ () (error 'home->index "given non-home!"))))

(define (home->quadrant/index h)
  (values (home->quadrant h)
          (home->index h)))

(define (goal? x)
  (and (fixnum? x)
       (fx<= 100 x)
       (or (fx<= x 103)
           (and (fx<= 110 x) (fx<= x 113))
           (and (fx<= 120 x) (fx<= x 123))
           (and (fx<= 130 x) (fx<= x 133)))))

(define/spec (goal q i)
  (-> quadrant? goal-index? goal?)
  (fx+ 100 (fx+ (fx* 10 q) i)))

(define/spec (goal->quadrant g)
  (-> goal? quadrant?)
  (fxquotient (fx- g 100) 10))

(define/spec (goal->index g)
  (-> goal? goal-index?)
  (fxremainder (fx- g 100) 10))

(define (goal->quadrant/index g)
  (if (goal? g)
      (let ([rel (fx- g 100)])
        (values (fxquotient rel 10)
                (fxremainder rel 10)))
      (error 'goal->quadrant/index "not a goal ~a" g)))


(define center #t)

(define (center? x)
  (eq? #t x))

(define (loc? x)
  (or (coord? x)
      (home? x)
      (goal? x)
      (center? x)))

;; a loc? that can be directly moved to
;; by a player movement.
(define (dest? x)
  (or (coord? x)
      (goal? x)
      (center? x)))

(define (loc->real x)
  (cond
    [(fixnum? x) x]
    [(center? x) +nan.0]
    [(home? x) -1]
    [else (error 'loc->real "invalid loc! ~a" x)]))

(define (loc< x y)
  (< (loc->real x) (loc->real y)))
