;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Fixnum geometrical operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import algebra)

;-------------------------------------------------------------------------------
; Distance
;-------------------------------------------------------------------------------

;;; Calculate the distance between two points

(define (fl-distance:point-point a b)
  (flsqrt (fl+ (flsquare (fl- (point-x a) (point-x b)))
               (flsquare (fl- (point-y a) (point-y b))))))

;;; Calculate the distance between point and segment

(define (fl-distance:point-segment p sg)
  (let* ((p1x (point-x (segment-a sg)))
         (p1y (point-y (segment-a sg)))
         (p2x (point-x (segment-b sg)))
         (p2y (point-y (segment-b sg)))
         (px (point-x p))
         (py (point-y p))
         (su (fl- p2x p1x))
         (sv (fl- p2y p1y))
         (div (fl+ (fl* su su) (fl* sv sv)))
         (u (fl/ (fl+ (fl* (fl- px p1x) su)
                      (fl* (fl- py p1y) sv))
                 div)))
    (cond
     ((fl> u 1.0)
      (set! u 1.0))
     ((fl< u 0.0)
      (set! u 0.0)))
    (let* ((x (fl+ p1x (fl* u su)))
           (y (fl+ p1y (fl* u sv)))
           (dx (fl- x px))
           (dy (fl- y py)))
      (flsqrt (fl+ (fl* dx dx) (fl* dy dy))))))

;;; Calculate the distance between a point and a point list

(define (fl-distance:point-pseq p plis)
  (cond
   ((or (null? plis) (null? (cdr plis)))
    +inf.0)
   (else
    (min (fl-distance:point-segment
           p
           (make-segment (car plis) (cadr plis)))
         (fl-distance:point-pseq
           p
           (cdr plis))))))

