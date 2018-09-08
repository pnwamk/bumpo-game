#lang typed/racket/base #:with-refinements

(provide (all-defined-out))

;; A Bumpo board has 4 quadrants (0,1,2, and 3)
;; and a center location ('center):
;;       |
;;  3    |   0
;;       |       
;;- - -  C  - - - 
;;       |
;;  2    |   1
;;       |
;;
;; For each quadrant Q ∈ [0..3] there are
;; - 12 standard locations [(Home Q 0)..(Home Q 11)],
;; - 4 goal locations [(Goal Q 0)..(Goal Q 3)], and
;; - 4 home locations [(Home Q 0)..(Home Q 3)].
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

;; useful for iterating through Z4 and Z12
(define z4s : (Listof Z4) (list 0 1 2 3))
(define z12s : (Listof Z12) (list 0 1 2 3 4 5 6 7 8 9 10 11))

(define-type Z4 (Refine [n : Natural] (<= n 3)))
(define-type Z12 (Refine [n : Natural] (<= n 11)))

;; locations (see board description above)
(define-type Center 'center)
(define center : Center 'center)
(: Center? (-> Any Boolean : Center))
(define (Center? x) (eq? 'center x))

(struct Home  ([quad : Z4] [idx : Z4])  #:transparent)
(struct Coord ([quad : Z4] [idx : Z12]) #:transparent)
(struct Goal  ([quad : Z4] [idx : Z4])  #:transparent)

;; some basic modular arithmetic for Z4 and Z12
(: Z4-add1 (-> Z4 Z4))
(define (Z4-add1 z4) (modulo (add1 z4) 4))

(: Z4-sub1 (-> Z4 Z4))
(define (Z4-sub1 z4) (modulo (sub1 z4) 4))

(: Z4= (-> Z4 Z4 Boolean))
(define Z4= eqv?)

(: Z12+ (-> Z12 Z12 Z12))
(define (Z12+ x y) (modulo (+ x y) 12))

(: Z12= (-> Z12 Z12 Boolean))
(define Z12= eqv?)

;; addition in Z12 that reports whether or not
;; there was overflow (this is useful for knowing
;; when a marble crosses into the next quadrant)
(: Z12+/overflow (-> Z12 Z12 (Values Z12 Boolean)))
(define (Z12+/overflow x y)
  (define sum (+ x y))
  (if (>= sum 12)
      (values (- sum 12) #t)
      (values sum #f)))

;; how far a marble can move at once,
;; either a D6 roll or 10 if they just
;; landed on an enemy marble not on a safe spot
(define-type Movement (Refine [n : Integer] (or (<= 1 n 6) (= n 10))))


;; Loc are locations that marbles may be on the board
(define-type Loc (U Home Coord Goal Center))
;; Dest are locations a player may move a marble
;; to on their turn (i.e. it does not include home,
;; because a player cannot choose to move their marble
;; home as)
(define-type Dest (U Coord Goal Center))

;; calculate what location is one move forward
;; from location `l` for player `player-num`
;; (NOTE: this does not include the center Loc)
(: Loc-add1 (-> Z4 Loc Dest))
(define (Loc-add1 player-num l)
  (cond
    [(Home? l) (Coord player-num 0)]
    [(Coord? l)
     (define idx* (Z12+ (Coord-idx l) 1))
     (cond
       [(zero? idx*)
        (define quad* (Z4-add1 (Coord-quad l)))
        (cond
          [(= player-num quad*) (Goal player-num 0)]
          [else (Coord quad* idx*)])]
       [else (Coord (Coord-quad l) idx*)])]
    [(Goal? l) (Goal player-num (Z4-add1 (Goal-idx l)))]
    [(Center? l) (Coord (Z4-sub1 player-num) 5)]))



