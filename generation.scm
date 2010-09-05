;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Geometry generation procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (declare (standard-bindings)
;;          (extended-bindings)
;;          (block))
;; (compile-options force-compile: #t)

(import (std srfi/1)
        ../core/syntax
        ../core/functional
        ../core/debugging
        ../core/list
        ../math/exact-algebra
        ../math/inexact-algebra
        ../visualization
        kernel)

;-------------------------------------------------------------------------------
; Direction generation
;-------------------------------------------------------------------------------

;;; Generate inexact random direction

(define (~generate.random-direction)
  (make-direction (random-real)
                  (random-real)))

;;; Generate exact random direction

(define (generate.random-direction)
  (make-direction (inexact->exact (random-real))
                  (inexact->exact (random-real))))

;;; Generate inexact random point

(define (~generate.random-point)
  (make-point (random-real)
              (random-real)))

;;; Generate exact random point

(define (generate.random-point)
  (make-point (inexact->exact (random-real))
              (inexact->exact (random-real))))

;-------------------------------------------------------------------------------
; Point generation
;-------------------------------------------------------------------------------

;;; Point between two points

(define (generate.point/two-points pa pb alpha)
  (vect2+
   pa
   (vect2:*scalar (point&point->direction pa pb)
                  alpha)))

;;; Random point between two points

(define (generate.random-point/two-points pa pb)
  (generate.point/two-points pa pb (random-exact)))

;-------------------------------------------------------------------------------
; Point mesh generation
;-------------------------------------------------------------------------------

;;; Return a random point that is inside a given pseq

(define (~generate.random-point-inside pseq)
  (define (gen a b)
    (aif p
         (curry pseq:point-inside? pseq)  
         (make-point (random-real/range (point-x a) (point-x b))
                     (random-real/range (point-y a) (point-y b)))
         p
         (gen a b)))
  (let ((bounding-box (pseq:bbox pseq)))
    (gen
     (bbox-lefttop bounding-box)
     (bbox-rightbottom bounding-box))))

;;; Return a random point that is inside a given pseq

(define (~generate.random-points/separation&boundaries N pseq p-dist b-dist)
                                        ; TODO: OPTIMIZE, A way would be dividing the space
  (define (respects-distances? p plis)
    (every (lambda (p-in-plis) (< p-dist (~distance.point-point p p-in-plis))) plis))
  (define (respects-boundaries? p)
    (< b-dist (~distance.point-pseq p pseq)))
  (define (gen n plis)
    (aif p
         (lambda (p) (and (respects-distances? p plis)
                     (respects-boundaries? p)))
         (~generate.random-point-inside pseq)
         (if (>= n N)
             (cons p plis)
             (gen (add1 n) (cons p plis)))
         (gen n plis)))
  (gen 0 '()))

;;; Generate regular point mesh

(define (generate.point-mesh-centered bb limits-offset offset-x offset-y #!optional point-modifier)
  (let* ((obox (vect2+ (make-point limits-offset limits-offset) (bbox-lefttop bb)))
         (o-x (vect2-x obox))
         (o-y (vect2-y obox))
         (ebox (vect2- (bbox-rightbottom bb) (make-point limits-offset limits-offset)))
         (e-x (vect2-x ebox))
         (e-y (vect2-y ebox))
         (size (vect2- (bbox:size-segment bb) (vect2:*scalar (make-vect2 limits-offset limits-offset) 2)))
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
                        2))))
          (modifier (if point-modifier point-modifier (lambda (p) p))))
      (unfold (lambda (p) (> (point-y p) e-y))
              values
              (lambda (p) (if (> (+ offset-x (point-x p)) e-x)
                         (modifier (make-point (point-x start)
                                               (+ offset-y (point-y p))))
                         (modifier (make-point (+ offset-x (point-x p))
                                               (point-y p)))))
              start))))

;-------------------------------------------------------------------------------
; Line generation
;-------------------------------------------------------------------------------

;;; Generates 2 values: the two parallels to the given one at the given distance

(define (generate.parallels-at-distance line distance)
  (let ((perp (vect2:*scalar
               (vect2:inexact->exact (vect2:~normalize
                                      (direction:perpendicular (line->direction line))))
               (inexact->exact distance))))
    (values (translate.line line perp)
            (translate.line line (vect2:symmetric perp)))))