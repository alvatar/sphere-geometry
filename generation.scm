;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Geometry generation procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1))
(import ../core/syntax)
(import ../core/functional)
(import ../dev/debugging)
(import ../math/exact-algebra)
(import ../math/inexact-algebra)
(import ../visualization)
(import kernel)

;-------------------------------------------------------------------------------
; Point generation
;-------------------------------------------------------------------------------

;;; Return a random point that is inside a given pseq

(define (generate.random-point-inside pseq)
  (define (gen a b)
    (aif p (curry pseq:point-inside? pseq)  
         (make-point (random-real/range (point-x a) (point-x b))
                     (random-real/range (point-y a) (point-y b)))
         p (gen origin delta)))
  (let ((bounding-box (pseq->bbox pseq)))
    (gen
     (bbox-lefttop bounding-box)
     (bbox-rightbottom bounding-box))))

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

;;; Generate regular point mesh

(define (generate.point-mesh-centered bb wall-offset offset-x offset-y)
  (let* ((obox (vect2+ (make-point wall-offset wall-offset) (bbox-lefttop bb)))
         (o-x (vect2-x obox))
         (o-y (vect2-y obox))
         (ebox (vect2- (bbox-rightbottom bb) (make-point wall-offset wall-offset)))
         (e-x (vect2-x ebox))
         (e-y (vect2-y ebox))
         (size (vect2- (bbox:size-segment bb) (vect2:*scalar (make-vect2 wall-offset wall-offset) 2)))
         (size-x (vect2-x size))
         (size-y (vect2-y size)))
    (let ((start (make-point            ; center the mesh
                  (+ o-x
                     (/ (* (~decimal-part (/ size-x offset-x))
                           offset-x)
                        2))
                  (+ o-y
                     (/ (* (~decimal-part (/ size-y offset-y))
                           offset-y)
                        2)))))
      (unfold (lambda (p) (> (point-y p) e-y))
              values
              (lambda (p) (if (> (+ offset-x (point-x p)) e-x)
                         (make-point (point-x start)
                                     (+ offset-y (point-y p)))
                         (make-point (+ offset-x (point-x p))
                                     (point-y p))))
              start))))

;-------------------------------------------------------------------------------
; Line generation
;-------------------------------------------------------------------------------

;;; Generates 2 values: the two parallels to the given one at the given distance

(define (generate.parallels-at-distance line distance)
  (error "unimplemented generate.parallels"))