;;; Copyright (c) 2012 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Conversions and construction from different elements
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Midpoint between 2 points

(define (point&point->midpoint p1 p2)
  (make-point (/ (+ (point-x p1) (point-x p2)) 2)
              (/ (+ (point-y p1) (point-y p2)) 2)))

;;; Direction of the line passing through two points

(define (point&point->direction p1 p2)
  (vect2:-vect2 p2 p1))

;;; Build a line from two points

(define (point&point->line p q)
  (let ((px (point-x p))
        (py (point-y p))
        (qx (point-x q))
        (qy (point-y q)))
    (cond
     ((= qy py)
      (cond
       ((> qx px)
        (make-line #e0 #e1 (- py)))
       ((= qx px)
        'point)
        ;(make-line #e0 #e0 #e0))
       (else
        (make-line #e0 #e-1 py))))
     ((= qx px)
      (cond
       ((> qy py)
        (make-line #e-1 #e0 px))
       ((= qy py)
        'point)
        ;(make-line #e0 #e0 #e0))
       (else
        (make-line #e1 #e0 (- px)))))
     (else
      (let ((a (- py qy))
            (b (- qx px)))
        (make-line a
                   b
                   (- (+ (* px a)
                         (* py b)))))))))

;;; Build a line from a point and a direction vector

(define (point&direction->line p dir)
  (let ((dx (direction-x dir))
        (dy (direction-y dir)))
    (make-line (- dy)
               dx
               (- (* (point-x p)
                     dy)
                  (* (point-y p)
                     dx)))))

;;; Segment's direction vector

(define (segment->direction seg)
  (%accept (segment? seg))
  (vect2:-vect2
    (segment-b seg)
    (segment-a seg)))

;;; Build a line from a segment

(define (segment->line seg)
  (point&point->line (segment-a seg) (segment-b seg)))

;;; Get the list of points that make the segment

(define (segment->pseq seg)
  (list (segment-a seg)
        (segment-b seg)))

;;; Build a line from a ray

(define (ray->line r)
  (error "Not implemented"))

;;; Convert line into direction

(define (line->direction line)
  (make-direction (line-b line) (- (line-a line))))

;;; Convert line into segment

(define (line->segment line from to)
  (make-segment (line:point line from)
                (line:point line to)))

;;; Picks the first and the last points of the pseq to build the segment

(define (pseq->segment-from-extremes plis)
  (make-segment (first plis)
                (last plis)))

;;; Convert a pseq to a list of independent segments

(define (pseq->list-segments pseq)
  (map (lambda (a b) (make-segment b a))
       (cdr pseq)
       pseq))

