;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Geometrical operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(declare (standard-bindings)(extended-bindings)(block)(not safe))
;(compile-options force-compile: #t)

(import (std srfi/1))

(import math/algebra)
(import utils/misc)

;;; run-time checks

(define-syntax check-arg
  (syntax-rules ()
    ((_ predicate arg proc)
     (check-arg-per-module "do" predicate arg proc))))

;-------------------------------------------------------------------------------
; Point 2d
;-------------------------------------------------------------------------------

(define make-point make-vect2)
(define point? vect2?)
(define point-x vect2-x)
(define point-y vect2-y)

;-------------------------------------------------------------------------------
; Direction 2d
;-------------------------------------------------------------------------------

(define make-direction make-vect2)
(define direction? vect2?)
(define direction-x vect2-x)
(define direction-y vect2-y)

;-------------------------------------------------------------------------------
; Infinite lines 2d
;-------------------------------------------------------------------------------

(define-structure line a b c)

;-------------------------------------------------------------------------------
; Ray (semi-infinite line) 2d
;-------------------------------------------------------------------------------

(define-structure ray o direction)

;-------------------------------------------------------------------------------
; Point operations
;-------------------------------------------------------------------------------

;;; Point rotation

(define (rotation:point vec r-angle)
  (make-point (- (* (point-x vec) (cos r-angle))
                 (* (point-y vec) (sin r-angle)))
              (+ (* (point-y vec) (cos r-angle))
                 (* (point-x vec) (sin r-angle)))))

;;; Point rotation

(define (rotation:point-w/reference ref p r-angle)
  (vect2:+vect2 ref
    (rotation:point
      (vect2:-vect2 p ref)
      r-angle)))

;;; Point translation

(define (translation:point p vec)
  (vect2:+vect2 p vec))

;-------------------------------------------------------------------------------
; Segments
;-------------------------------------------------------------------------------

(define-structure segment a b)

;;; Segment's direction vector

(define (segment->direction seg)
  (check-arg segment? seg segment->direction)
  (vect2:-vect2
    (segment-b seg)
    (segment-a seg)))

;;; Segment length

(define (segment:length seg)
  (check-arg segment? seg segment:length)
  (vect2:magnitude (segment->direction seg)))

;;; Reverse segment

(define (segment:reverse seg)
  (check-arg segment? seg segment:reverse)
  (make-segment (segment-b seg)
                (segment-a seg)))

;;; Tell whether the point is an end point of the segment

(define (segment:is-end-point? segment point)
  (let* ((px (point-x point))
         (py (point-y point))
         (a (segment-a segment))
         (ax (point-x a))
         (ay (point-y a))
         (b (segment-b segment))
         (bx (point-x b))
         (by (point-y b)))
    (or (and
          (=~ ax px)
          (=~ ay py))
        (and
          (=~ bx px)
          (=~ by py)))))

;;; Tell whether the two segments are connected

(define (segment:connected-segment? seg1 seg2)
  (or (segment:is-end-point? seg2 (segment-a seg1))
      (segment:is-end-point? seg2 (segment-b seg1))))

;;; Tell whether the segments are parallel

(define (segment:parallel-segment? seg1 seg2)
  (vect2:=?e
    (vect2:normalize (segment->direction seg1))
    (vect2:normalize (segment->direction seg2))
    0.01))

;;; Calculate absolute point given segment and percent

(define (segment:relative-position->point seg rel)
  (let ((vec (segment->direction seg))
        (O (segment-a seg)))
    (make-point (+ (point-x O) (* (point-x vec) rel))
                (+ (point-y O) (* (point-y vec) rel)))))

;;; Calculate the segment's mid point

(define (segment:mid-point seg)
  (let ((a (segment-a seg))
        (b (segment-b seg)))
    (make-point (average (point-x a) (point-x b))
                  (average (point-y a) (point-y b)))))

;-------------------------------------------------------------------------------
; Point sequences
;-------------------------------------------------------------------------------

;;; Is pseq?

(define (pseq? plis)
  (and (notnull? plis)
       (every (lambda (p) (point? p))
              plis)))

;;; Is pseq? (shallow version)

(define (pseq?-shallow plis)
  (and (notnull? plis)
       (point? (car plis))))

;;; Picks the first and the last points of the pseq to build the segment

(define (pseq->segment plis)
  (make-segment (first plis)
                (last plis)))

;; Is end point?

(define (pseq:is-end-point? segment point)
  (let* ((px (point-x point))
         (py (point-y point))
         (a (first segment))
         (ax (point-x a))
         (ay (point-y a))
         (b (last segment))
         (bx (point-x b))
         (by (point-y b)))
    (or (and
          (=~ ax px)
          (=~ ay py))
        (and
          (=~ bx px)
          (=~ by py)))))

;;; Are these pseq connected?

(define (pseq:connected-pseq? p1 p2)
  (or (pseq:is-end-point? p2 (first p1))
      (pseq:is-end-point? p2 (last p1))))

;;; Close a point-list (repeats first point in the last position)

(define (pseq:close plis)
  (snoc plis (car plis)))

;;; Append two point lists, appends always after (optimized for second one being shorter)

(define (pseq:append a b)
  (let ((fa (first a))
        (la (last a))
        (fb (first b))
        (lb (last b)))
    (cond ; target situation: ----->x---->>
     ((vect2:=? fa fb) ; case: <-----x---->>
      (append-reverse a (cdr b)))
     ((vect2:=? fa lb) ; case: <-----x<<----
      (append-reverse a (cdr (reverse b))))
     ((vect2:=? la fb) ; case: ----->x---->>
      (append a (cdr b)))
     ((vect2:=? la lb) ; case: ----->x<<----
      (append a (cdr (reverse b))))
     (else
      (pp a)
      (pp b)
      (error "Point sequences cannot be connected")))))

;;; Join two point lists, appends to either first or last (optimized for second one being shorter)

(define (pseq:join a b)
  (let ((fa (first a))
        (la (last a))
        (fb (first b))
        (lb (last b)))
    (cond ; target situation: ----->x---->>
     ((vect2:=? fa fb) ; case: <-----x---->>
      (append a (reverse (cdr b))))
     ((vect2:=? fa lb) ; case: <-----x<<----
      (append a (reverse (cdr (reverse b)))))
     ((vect2:=? la fb) ; case: ----->x---->>
      (append a (cdr b)))
     ((vect2:=? la lb) ; case: ----->x<<----
      (append a (reverse (cdr b))))
     (else
      (pp a)
      (pp b)
      (error "Point sequences cannot be connected")))))

;;; pseq centroid

(define (pseq:centroid plis)
  (define (iter n sum plis-tail)
    (cond
     ((null? plis-tail)
      (vect2:/scalar sum (exact->inexact n)))
     (else
      (iter
        (+ 1 n)
        (vect2:+vect2 sum (car plis-tail))
        (cdr plis-tail)))))
  (cond
   ((null? plis)
    (error "Argument #1 should be a point list"))
   (else
    (iter 0 (make-point 0.0 0.0) plis))))

;;; pseq extreme point

(define (pseq:extreme plis f)
  (define (iter current plis-tail)
    (cond
     ((null? plis-tail)
      current)
     (else
       (iter (f current (car plis-tail)) (cdr plis-tail)))))
  (if (null? plis)
      (error "Argument #1 must be a point list")
    (iter (car plis) (cdr plis))))

;;; pseq right-most point

(define (pseq:extreme-right plis)
  (pseq:extreme
    plis
    (lambda (current next)
      (if (< (point-x current) (point-x next))
          next
        current))))

;;; pseq left-most point

(define (pseq:extreme-left plis)
  (pseq:extreme
    plis
    (lambda (current next)
      (if (> (point-x current) (point-x next))
          next
        current))))

;;; pseq top-most point

(define (pseq:extreme-top plis)
  (pseq:extreme
    plis
    (lambda (current next)
      (if (< (point-y current) (point-y next))
          next
        current))))

;;; pseq bottom-most point

(define (pseq:extreme-bottom plis)
  (pseq:extreme
    plis
    (lambda (current next)
      (if (> (point-x current) (point-x next))
          next
        current))))

;;; Find a common point of two given point lists

(define (pseq:common-point? plis1 plis2)
  (find
    (lambda (e)
      (any (lambda (it) (vect2:=? it e)) plis1))
    plis2))

;;; Calculate the tangent vector in a point-list given the relative position

(define (pseq:tangent-in-relative plis rel)
  (let ((approx (pseq->segment plis))) ; TODO: handle pseqs properly
    (vect2:normalize
      (segment->direction
        (make-segment
          (segment:relative-position->point approx rel)
          (segment-b approx))))))

;;; Is point inside the polygon pseq?

;; http://www.ecse.rpi.edu/Homepages/wrf/Research/Short_Notes/pnpoly.html
(define (pseq:point-inside? point-list p)
  (define (iter c a-points b-points)
    (cond
     ((null? a-points)
      c)
     ((and (not (eq? (> (point-y (car a-points)) (point-y p))
                     (> (point-y (car b-points)) (point-y p))))
           (< (point-x p)
              (+ (/ (* (- (point-x (car b-points)) (point-x (car a-points)))
                       (- (point-y p) (point-y (car a-points))))
                    (- (point-y (car b-points)) (point-y (car a-points))))
                 (point-x (car a-points)))))
      (iter (not c) (cdr a-points) (cdr b-points)))
     (else
      (iter c (cdr a-points) (cdr b-points)))))
  (iter #f point-list (cons (last point-list) point-list)))

;;; Return a random point that is inside a given pseq

(define (pseq:make-random-point-inside point-list)
  (define (try origin delta)
    (let ((p (make-point (+ (point-x origin)
                              (* (point-x delta) (random-real)))
                           (+ (point-y origin)
                              (* (point-y delta) (random-real))))))
      (if (pseq:point-inside? point-list p)
          p
        (try origin delta))))
  (let* ((bounding-box (pseq:bounding-box point-list))
         (bb-left-corner (bounding-box-lefttop bounding-box))
         (bb-right-corner (bounding-box-rightbottom bounding-box)))
    (try
      bb-left-corner
      (vect2:-vect2 bb-right-corner
                    bb-left-corner))))

;-------------------------------------------------------------------------------
; Distance
;-------------------------------------------------------------------------------

;;; Calculate the distance between two points

(define (distance:point-point a b)
  (sqrt (+ (square (- (point-x a) (point-x b)))
           (square (- (point-y a) (point-y b))))))

;;; Calculate the distance between point and segment

(define (distance:point-segment p sg)
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
      (sqrt (+ (* dx dx) (* dy dy))))))

;;; Calculate the distance between a point and a point list

(define (distance:point-pseq p plis)
  (cond
   ((or (null? plis) (null? (cdr plis)))
    +inf.0)
   (else
    (min (distance:point-segment
           p
           (make-segment (car plis) (cadr plis)))
         (distance:point-pseq
           p
           (cdr plis))))))

;-------------------------------------------------------------------------------
; Intersections
;-------------------------------------------------------------------------------

;;; Segment-segment intersection

(define (intersection:segment-segment sg1 sg2)
  (let* ((a1 (car sg1))
         (a2 (cadr sg1))
         (b1 (car sg2))
         (b2 (cadr sg2))
         (ua-t (- (* (- (point-x b2) (point-x b1))
                     (- (point-y a1) (point-y b1)))
                  (* (- (point-y b2) (point-y b1))
                     (- (point-x a1) (point-x b1)))))
         (ub-t (- (* (- (point-x a2) (point-x a1))
                     (- (point-y a1) (point-y b1)))
                  (* (- (point-y a2) (point-y a1))
                     (- (point-x a1) (point-x b1)))))
         (u-b (- (* (- (point-y b2) (point-y b1))
                    (- (point-x a2) (point-x a1)))
                 (* (- (point-x b2) (point-x b1))
                    (- (point-y a2) (point-y a1))))))
    (if (=~ u-b 0.0)
        (if (or (=~ ua-t 0.0) (=~ ub-t 0.0))
            'coincident
          'parallel)
      (let ((ua (/ ua-t u-b))
            (ub (/ ub-t u-b)))
        (if (and (<= 0 ua)
                 (<= ua 1)
                 (<= 0 ub)
                 (<= ub 1))
            (make-point (* (+ (point-x a1) ua)
                             (- (point-x a2) (point-x a1)))
                          (* (+ (point-y a1) ua)
                             (- (point-y a2) (point-y a1))))
          'no-intersection)))))

;;; Segment-pseq intersection

(define (intersection:segment-pseq seg pol)
  (define (append-next intersections pol-rest)
    (let ((inters (intersection:segment-segment seg (make-segment (car pol-rest) (cadr pol-rest)))))
      (if (or (null? pol-rest) (< (length pol-rest) 3))
          (append intersections (list inters))
        (if (point? inters)
            (append-next (append intersections (list inters)) (cdr pol-rest))
          (append-next intersections (cdr pol-rest))))))
  (append-next '() pol))

;;; Segment-pseq (closed pseq) intersection

(define (intersection:segment-pseq seg plis)
  (intersection:segment-pseq seg (pseq:close plis)))

;-------------------------------------------------------------------------------
; Bounding boxes
;-------------------------------------------------------------------------------

(define-structure bounding-box lefttop rightbottom)

;;; Calculate the bounding point of a pseq

(define (pseq:bounding-box point-list)
  (let ((first (car point-list))
        (rest (cdr point-list)))
    (make-bounding-box
      (make-point (fold (lambda (point x) (min x (point-x point)))
                        (point-x first)
                        rest)
                  (fold (lambda (point y) (min y (point-y point)))
                        (point-y first)
                        rest))
      (make-point (fold (lambda (point x) (max x (point-x point)))
                        (point-x first)
                        rest)
                  (fold (lambda (point y) (max y (point-y point)))
                        (point-y first)
                        rest)))))

;;; Calculate the diagonal segment connecting the two extremes of the bb

(define (bounding-box:diagonal-segment bb)
  (make-segment (bounding-box-lefttop bb)
                (bounding-box-rightbottom bb)))

;;; Bounding box size segment

(define (bounding-box:size-segment bb)
  (segment->direction (bounding-box:diagonal-segment bb)))
