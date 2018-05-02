#lang racket/base

(require define-with-spec
         2htdp/image
         2htdp/universe
         racket/match)


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

(define (quadrant? x)
  (and (exact-nonnegative-integer? x)
       (<= 1 x 4)))
(define (position? x)
  (and (exact-integer? x)
       (<= -4 x 11)))

(struct/spec coord ([quadrant quadrant?]
                    [position position?]))

(define (location? x)
  (match x
    [#f #t] ;; home base
    [#t #t] ;; center
    [(? coord?) #t] ;; board coordinate
    [_ #f]))

(struct/spec player ([name string?]
                     [human? boolean?]
                     [marbles (list location?
                                    location?
                                    location?
                                    location?)]))
