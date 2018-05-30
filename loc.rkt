#lang typed/racket/base #:with-refinements

(provide (all-defined-out))

;; A Bumpo board has 4 quadrants and a "center location" C:
;;       |
;;  3    |   0
;;       |       
;;- - -  C  - - - 
;;       |
;;  2    |   1
;;       |


;; Each quadrant has:
;; - 12 standard locations (0-11 + q*12),
;; - 4 goal locations (1q0-1q3), and
;; - 4 home locations ('hq0-'hq3).
;;
;; Here is quadrant 0 for an example:
;;- - - - - - - - - - - -|
;;       0   h0          |
;;   g0  1     h1        |
;;   g1  2       h2      |
;;   g2  3         h3    |
;;   g3  4               |
;;       5 6 7 8 9 10    |
;; 'center         11    |
;;                       |
;;- - - - - - - - - - - -|

(define z4s : (Listof Z4) (list 0 1 2 3))
(define z12s : (Listof Z12) (list 0 1 2 3 4 5 6 7 8 9 10 11))

(define-type Z4 (Refine [n : Natural] (<= n 3)))
(define-type Z12 (Refine [n : Natural] (<= n 11)))

(struct Home  ([quad : Z4] [idx : Z4])  #:transparent)
(struct Coord ([quad : Z4] [idx : Z12]) #:transparent)
(struct Goal  ([quad : Z4] [idx : Z4])  #:transparent)


(: Z4-add1 (-> Z4 Z4))
(define (Z4-add1 z4)
  (modulo (add1 z4) 4))

(: Z4-sub1 (-> Z4 Z4))
(define (Z4-sub1 z4)
  (modulo (sub1 z4) 4))

(: Z4= (-> Z4 Z4 Boolean))
(define Z4= eqv?)

(define-type Movement (Refine [n : Integer] (or (<= 1 n 6) (= n 10))))

(: Z12+ (-> Z12 Z12 Z12))
(define (Z12+ x y)
  (modulo (+ x y) 12))

(: Z12+/overflow (-> Z12 Z12 (Values Z12 Boolean)))
(define (Z12+/overflow x y)
  (define sum (+ x y))
  (if (>= sum 12)
      (values (- sum 12) #t)
      (values sum #f)))

(: Z12= (-> Z12 Z12 Boolean))
(define Z12= eqv?)

(define-type Center 'center)
(define center : Center 'center)
(: Center? (-> Any Boolean : Center))
(define (Center? x) (eq? 'center x))

(define-type Loc (U Home Coord Goal Center))
(define-type Dest (U Coord Goal Center))

(: Loc-inc (-> Z4 Loc Dest))
(define (Loc-inc player l)
  (cond
    [(Home? l) (Coord player 0)]
    [(Coord? l)
     (define idx* (Z12+ (Coord-idx l) 1))
     (cond
       [(zero? idx*)
        (define quad* (Z4-add1 (Coord-quad l)))
        (cond
          [(= player quad*) (Goal player 0)]
          [else (Coord quad* idx*)])]
       [else (Coord (Coord-quad l) idx*)])]
    [(Goal? l) (Goal player (Z4-add1 (Goal-idx l)))]
    [(Center? l) (Coord (Z4-sub1 player) 5)]))



