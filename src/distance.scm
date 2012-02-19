;;; Copyright (c) 2012 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Distances and squared distances
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Calculate the distance between two points

(define (squareddistance.point-point a b)
  (+ (square (- (point-x a) (point-x b)))
     (square (- (point-y a) (point-y b)))))

;;; Calculate the distance between point and segment

(define (squareddistance.point-segment p sg)
  (let* ((p1x (point-x (segment-a sg)))
         (p1y (point-y (segment-a sg)))
         (p2x (point-x (segment-b sg)))
         (p2y (point-y (segment-b sg)))
         (px (point-x p))
         (py (point-y p))
         (su (- p2x p1x))
         (sv (- p2y p1y))
         (div (+ (* su su) (* sv sv)))
         (u (/ (+ (* (- px p1x) su)
                  (* (- py p1y) sv))
               div)))
    (cond
     ((> u 1)
      (set! u 1))
     ((< u 0)
      (set! u 0)))
    (let* ((x (+ p1x (* u su)))
           (y (+ p1y (* u sv)))
           (dx (- x px))
           (dy (- y py)))
      (+ (* dx dx) (* dy dy)))))

;;; Calculate the distance between a point and a point list

(define (squareddistance.point-pseq p plis)
  (cond
   ((or (null? plis) (null? (cdr plis)))
    +inf.0)
   (else
    (min (squareddistance.point-segment
           p
           (make-segment (car plis) (cadr plis)))
         (squareddistance.point-pseq
           p
           (cdr plis))))))

;;; Calculate the minimum squared distance between the endpoints of two segments

(define (squareddistance.segment-segment/endpoints s1 s2)
  (let ((s1a (segment-a s1))
        (s1b (segment-b s1))
        (s2a (segment-a s2))
        (s2b (segment-b s2)))
    (min (squareddistance.point-point s1a s2a)
         (squareddistance.point-point s1a s2b)
         (squareddistance.point-point s1b s2a)
         (squareddistance.point-point s1b s2b))))

;;; Calculate the minimum squared distance between the endpoints of two pseqs

(define (squareddistance.pseq-pseq/endpoints ps1 ps2)
  (let ((ps1-f (first ps1))
        (ps1-l (last ps1))
        (ps2-f (first ps2))
        (ps2-l (last ps2)))
    (min (squareddistance.point-point ps1-f ps2-f)
         (squareddistance.point-point ps1-f ps2-l)
         (squareddistance.point-point ps1-l ps2-f)
         (squareddistance.point-point ps1-l ps2-l))))

;;; Calculate the distance between two points

(define (~distance.point-point a b)
  (sqrt (squareddistance.point-point a b)))

;;; Calculate the distance between point and segment

(define (~distance.point-segment p sg)
  (sqrt (squareddistance.point-segment p sg)))

;;; Calculate the distance between a point and a point list

(define (~distance.point-pseq p plis)
  (sqrt (squareddistance.point-pseq p plis)))

;;; Calculate the minimum distance between the end points of two segments

(define (~distance.segment-segment/endpoints s1 s2)
  (sqrt (squareddistance.segment-segment/endpoints s1 s2)))

;;; Calculate the minimum distance between the end points of two pseqs

(define (~distance.pseq-pseq/endpoints ps1 ps2)
  (sqrt (squareddistance.pseq-pseq/endpoints ps1 ps2)))

