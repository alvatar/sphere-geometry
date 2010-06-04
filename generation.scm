;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Geometry generation procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1))
(import ../core/syntax)
(import ../core/functional)
(import ../math/exact-algebra)
(import ../math/inexact-algebra)
(import kernel)

;;; Return a random point that is inside a given pseq

(define (generate.random-point-inside pseq)
  (define (gen a b)
    (aif p (curry pseq:point-inside? pseq)  
         (make-point (random-real/range (point-x a) (point-x b))
                     (random-real/range (point-y a) (point-y b)))
         p (gen origin delta)))
  (let ((bounding-box (pseq->bounding-box pseq)))
    (gen
     (bounding-box-lefttop bounding-box)
     (bounding-box-rightbottom bounding-box))))

;;; Return a random point that is inside a given pseq

(define (generate.random-points/separation&boundaries N pseq p-dist b-dist)
                                        ; TODO: OPTIMIZE, A way would be dividing the space
  (define (respects-distances? p plis)
    (every (lambda (p-in-plis) (< p-dist (~distance.point-point p p-in-plis))) plis))
  (define (respects-boundaries? p)
    (< b-dist (~distance.point-pseq p pseq)))
  (define (gen n plis)
    (aif p (lambda (p) (and (respects-distances? p plis)
                       (respects-boundaries? p)))
         (generate.random-point-inside pseq)
         (if (>= n N)
             (cons p plis)
             (gen (add1 n) (cons p plis)))
         (gen n plis)))
  (gen 0 '()))
