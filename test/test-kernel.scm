;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tests for geometry package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/64))
(import ../../math/exact-algebra)
(import ../kernel)

(define-syntax test-equal/=
  (syntax-rules ()
   ((_ name =f expr result)
    (test-assert name (=f expr result)))))

;-------------------------------------------------------------------------------
(test-begin "geometry")
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
; Construction
;-------------------------------------------------------------------------------

(test-equal/= "segment->line horizontal line"
              line:=
              (segment->line (make-segment (make-point 2.2 -2.0)
                                           (make-point 2.2 3.0)))
              (make-line -1.0 0.0 2.2))

(test-equal/= "segment->line vertical line"
              line:=
              (segment->line (make-segment (make-point 1.0 1.0)
                                           (make-point 11.0 1.0)))
              (make-line 0.0 1.0 -1.0))

(test-equal/= "line:point"
              vect2:=
              (line:point (segment->line
                           (make-segment (make-point 4.0 1.0)
                                         (make-point 3.0 1.0)))
                          0)
              (make-point 1.0 1.0))

;-------------------------------------------------------------------------------
; Segment
;-------------------------------------------------------------------------------

(test-equal "segment:point-relative-position"
            (segment:point->relative-position
             (make-segment (make-point 1.0 2.0) (make-point 3.0 2.0))
             (make-point 2.0 2.0))
            0.5)

;-------------------------------------------------------------------------------
; pseq
;-------------------------------------------------------------------------------

(test-equal/= "centroid"
              vect2:=
              (pseq:centroid
               (list (make-point 0.0 0.0)
                     (make-point 2.0 0.0)
                     (make-point 2.0 2.0)
                     (make-point 0.0 2.0)))
              (make-point 1.0 1.0))

(test-equal/= "extreme-right"
              vect2:=
              (pseq:extreme-right
               (list (make-point 0.0 1.0)
                     (make-point 1.0 1.0)
                     (make-point 2.0 2.0)
                     (make-point 2.0 3.0)))
              (make-point 2.0 3.0))

(test-equal/= "extreme-bottom"
              vect2:=
              (pseq:extreme-bottom
               (list (make-point 0.0 1.0)
                     (make-point 1.0 1.0)
                     (make-point 2.0 3.0)
                     (make-point 3.0 3.0)))
              (make-point 0.0 1.0))

;-------------------------------------------------------------------------------
; Translations
;-------------------------------------------------------------------------------

(test-equal/= "translate.line"
              line:=
              (translate.line (segment->line
                               (make-segment (make-point 0.0 0.0)
                                             (make-point 1.0 1.0)))
                              (make-vect2 0.0 1.0))
              (segment->line
               (make-segment (make-point 0.0 1.0)
                             (make-point 1.0 2.0))))

(test-equal/= "translate.line"
              line:=
              (translate.line (segment->line
                               (make-segment (make-point 0.0 0.0)
                                             (make-point 1.0 1.0)))
                              (make-vect2 1.0 1.0))
              (segment->line
               (make-segment (make-point 1.0 1.0)
                             (make-point 2.0 2.0))))

;-------------------------------------------------------------------------------
; Intersections
;-------------------------------------------------------------------------------

(test-equal/= "intersection:line-line 2"
              vect2:=
              (intersection.line-line
               (segment->line (make-segment (make-point 0.0 0.0)
                                            (make-point 2.0 2.0)))
               (segment->line (make-segment (make-point 0.0 2.0)
                                            (make-point 2.0 0.0))))
              (make-point 1.0 1.0))

(test-equal/= "intersection:line-line 2"
              vect2:=
              (intersection.line-line
               (segment->line (make-segment (make-point 0.0 0.0)
                                            (make-point 2.0 2.0)))
               (segment->line (make-segment (make-point -2.0 0.0)
                                            (make-point 0.0 -2.0))))
              (make-point -1.0 -1.0))

;-------------------------------------------------------------------------------
(test-end "geometry")
;-------------------------------------------------------------------------------