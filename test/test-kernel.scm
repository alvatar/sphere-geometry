;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tests for geometry package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/64))
(import math/algebra)
(import kernel)

(define-syntax test-equal-vect2
  (syntax-rules ()
   ((_ name expr result)
    (test-assert name (vect2:=? expr result)))))

(define-syntax test-equal-point2
  (syntax-rules ()
   ((_ name expr result)
    (test-assert name (point:=? expr result)))))

;-------------------------------------------------------------------------------
(test-begin "geometry")
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
; Construction
;-------------------------------------------------------------------------------

(test-equal "segment->line horizontal line"
  (segment->line (make-segment (make-point 2.2 -2.0)
                               (make-point 2.2 3.0)))
  (make-line -1.0 0.0 2.2))

(test-equal "segment->line vertical line"
  (segment->line (make-segment (make-point 1.0 1.0)
                               (make-point 11.0 1.0)))
  (make-line 0.0 1.0 -1.0))

;-------------------------------------------------------------------------------
; Segment
;-------------------------------------------------------------------------------

(test-equal
 "segment:point-relative-position"
 (segment:point->relative-position (make-segment (make-point 1.0 2.0) (make-point 3.0 2.0)) (make-point 2.0 2.0))
 0.5)

;-------------------------------------------------------------------------------
; pseq
;-------------------------------------------------------------------------------

(test-equal-vect2
 "centroid"
 (pseq:centroid
  (list (make-point 0.0 0.0)
        (make-point 2.0 0.0)
        (make-point 2.0 2.0)
        (make-point 0.0 2.0)))
 (make-point 1.0 1.0))

(test-equal-vect2
 "extreme-right"
 (pseq:extreme-right
  (list (make-point 0.0 1.0)
        (make-point 1.0 1.0)
        (make-point 2.0 2.0)
        (make-point 2.0 3.0)))
 (make-point 2.0 3.0))

(test-equal-vect2
 "extreme-bottom"
 (pseq:extreme-bottom
  (list (make-point 0.0 1.0)
        (make-point 1.0 1.0)
        (make-point 2.0 3.0)
        (make-point 3.0 3.0)))
 (make-point 0.0 1.0))

;-------------------------------------------------------------------------------
; Intersections
;-------------------------------------------------------------------------------

(test-equal-vect2 "intersection:line-line 2"
  (intersection:line-line
    (segment->line (make-segment (make-point 0.0 0.0)
                                 (make-point 2.0 2.0)))
    (segment->line (make-segment (make-point 0.0 2.0)
                                 (make-point 2.0 0.0))))
  (make-point 1.0 1.0))

(test-equal-vect2 "intersection:line-line 2"
  (intersection:line-line
    (segment->line (make-segment (make-point 0.0 0.0)
                                 (make-point 2.0 2.0)))
    (segment->line (make-segment (make-point -2.0 0.0)
                                 (make-point 0.0 -2.0))))
  (make-point -1.0 -1.0))

;-------------------------------------------------------------------------------
(test-end "geometry")
;-------------------------------------------------------------------------------
