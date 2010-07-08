;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Geometrical operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare (standard-bindings)
         (extended-bindings)
         (block))
(compile-options force-compile: #t)

(import (std srfi/1))

(import ../core/list)
(import ../core/syntax)
(import ../dev/debugging)
(import ../math/exact-algebra)
(import ../math/inexact-algebra)

;;; run-time checks

(define-syntax assert
  (syntax-rules ()
    ((_ test proc)
     (assert-aux "do" test proc))))

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

;;; Perpendicular direction

(define (direction:perpendicular dir)
  (make-direction (direction-y dir)
                  (- (direction-x dir))))

;;; Direction to angle

(define (direction->angle-rad dir)
  (angle (make-rectangular (direction-x dir)
                           (direction-y dir))))

;-------------------------------------------------------------------------------
; Infinite lines 2d
;-------------------------------------------------------------------------------

(define-structure line a b c)

;;; Equality

(define (line:= la lb)
  (and (= (line-a la) (line-a lb))
       (= (line-b la) (line-b lb))
       (= (line-c la) (line-c lb))))

;;; Build a line from a point and a direction vector

(define (point+direction->line p dir)
  (let ((dx (direction-x dir))
        (dy (direction-y dir)))
    (make-line (- dy)
               dx
               (- (* (point-x p)
                     dy)
                  (* (point-y p)
                     dx)))))

;;; Build a line from two points

(define (point+point->line p q)
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

;;; Get line direction

(define (line->direction line)
  (make-direction (line-b line) (- (line-a line))))

;;; Get point in line

(define (line:point line i)
  (let ((a (line-a line))
        (b (line-b line))
        (c (line-c line)))
    (if (zero? b)
        (make-point (+ (/ (- (- b) c) a)
                       (* i b))
                    (- 1 (* i a)))
        (make-point (+ 1 (* i b))
                    (- (/ (- (- a) c) b)
                       (* i a))))))

;-------------------------------------------------------------------------------
; Ray (semi-infinite line) 2d
;-------------------------------------------------------------------------------

(define-structure ray o direction)

;;; Build a line from a ray

(define (ray->line r) ; TODO
  (make-line #e0 #e0 #e0))

;-------------------------------------------------------------------------------
; Segments
;-------------------------------------------------------------------------------

(define-structure segment a b)

;;; Segment's direction vector

(define (segment->direction seg)
  (assert (segment? seg) segment->direction)
  (vect2:-vect2
    (segment-b seg)
    (segment-a seg)))

;;; Build a line from a segment

(define (segment->line seg)
  (point+point->line (segment-a seg) (segment-b seg)))

;;; Get the list of points that make the segment

(define (segment->pseq seg)
  (list (segment-a seg)
        (segment-b seg)))

;;; Segment length

(define (segment:~length seg)
  (assert (segment? seg) segment:~length)
  (vect2:~magnitude (segment->direction seg)))

;;; Reverse segment

(define (segment:reverse seg)
  (assert (segment? seg) segment:reverse)
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
          (= ax px)
          (= ay py))
        (and
          (= bx px)
          (= by py)))))

;;; Point collinear to segment?

(define (segment:collinear-point? seg p)
  (error "unimplemented")) ; TODO

;;; Point collinear and on the segment?

(define (segment:collinear-point-on? seg p)
  (collinear-ordered-points? (segment-a seg) p (segment-b seg)))

;;; Tell whether the two segments are connected

(define (segment:connected-segment? seg1 seg2)
  (or (segment:is-end-point? seg2 (segment-a seg1))
      (segment:is-end-point? seg2 (segment-b seg1))))

;;; Tell whether the segments are parallel

(define (segment:~parallel-segment? seg1 seg2)
  (vect2:~=e
    (vect2:~normalize (segment->direction seg1))
    (vect2:~normalize (segment->direction seg2))
    0.01))

;;; Calculate absolute point given segment and percent

(define (segment:relative-position->point seg rel)
  (let ((vec (segment->direction seg))
        (o (segment-a seg)))
    (make-point (+ (point-x o) (* (point-x vec) rel))
                (+ (point-y o) (* (point-y vec) rel)))))

;;; Calculate relative position given a point collinear and on the segment

(define (segment:point->relative-position seg p)
  (if (segment:collinear-point-on? seg p)
      (let ((s1 (segment-a seg))
            (s2 (segment-b seg)))
        (cond
         ((= (point-x s1) (point-x s2))
          (/ (- (point-y p) (point-y s1)) (- (point-y s2) (point-y s1))))
         ((= (point-y s1) (point-y s2))
          (/ (- (point-x p) (point-x s1)) (- (point-x s2) (point-x s1))))
         (else
          (assert (= (/ (- (point-y p) (point-y s1)) (- (point-y s2) (point-y s1)))
                     (/ (- (point-x p) (point-x s1)) (- (point-x s2) (point-x s1))))
                  segment:point->relative-position)
          (/ (- (point-y p) (point-y s1)) (- (point-y s2) (point-y s1))))))
      'not-collinear))

;;; Calculate the segment's mid point

(define (segment:mid-point seg)
  (let ((a (segment-a seg))
        (b (segment-b seg)))
    (make-point (/ (+ (point-x a) (point-x b)) 2)
                (/ (+ (point-y a) (point-y b)) 2))))

;-------------------------------------------------------------------------------
; Point sequences
;-------------------------------------------------------------------------------

;;; Is pseq?

(define (pseq? plis)
  (and (list? plis)
       (not-null? plis)
       (every (lambda (p) (point? p)) plis)))

;;; Is pseq? (shallow version)

(define (pseq?-shallow plis)
  (and (not-null? plis)
       (point? (car plis))))

;;; Picks the first and the last points of the pseq to build the segment

(define (pseq->segment plis)
  (make-segment (first plis)
                (last plis)))

;;; Convert a pseq to a list of independent segments

(define (pseq->lsegments pseq)
  (map (lambda (a b) (make-segment b a))
       (cdr pseq)
       pseq))

;;; Calculate the bounding point of a pseq

(define (pseq->bbox point-list)
  (let ((first (car point-list))
        (rest (cdr point-list)))
    (make-bbox
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

;;; Length of a pseq

(define (pseq:~length pseq)
  (pair-fold
   (lambda (pair accum)
     (+
      (if (null? (cdr pair))
          0
	  (segment:~length (make-segment (car pair) (cadr pair))))
      accum))
   0
   pseq))

;;; Is end point?

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
          (= ax px)
          (= ay py))
        (and
          (= bx px)
          (= by py)))))

;;; Are these pseq connected?

(define (pseq:connected-pseq? p1 p2)
  (or (pseq:is-end-point? p2 (first p1))
      (pseq:is-end-point? p2 (last p1))))

;;; Is the pseq closed (as a polygon)

(define (pseq:closed? pseq)
  (vect2:= (first pseq) (last pseq)))

;;; Close a point-list (repeats first point in the last position)

(define (pseq:close pseq)
  (if (pseq:closed? pseq)
      pseq
      (snoc pseq (car pseq))))

;;; Append two point lists, appends always after (optimized for second one being shorter)

(define (pseq:append a b)
  (let ((fa (first a))
        (la (last a))
        (fb (first b))
        (lb (last b)))
    (cond ; target situation: ----->x---->>
     ((vect2:= fa fb) ; case: <-----x---->>
      (append-reverse a (cdr b)))
     ((vect2:= fa lb) ; case: <-----x<<----
      (append-reverse a (cdr (reverse b))))
     ((vect2:= la fb) ; case: ----->x---->>
      (append a (cdr b)))
     ((vect2:= la lb) ; case: ----->x<<----
      (append a (cdr (reverse b))))
     (else
      (display "******* pseq:append *******\n")
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
     ((vect2:= fa fb) ; case: <-----x---->>
      (append a (reverse (cdr b))))
     ((vect2:= fa lb) ; case: <-----x<<----
      (append a (reverse (cdr (reverse b)))))
     ((vect2:= la fb) ; case: ----->x---->>
      (append a (cdr b)))
     ((vect2:= la lb) ; case: ----->x<<----
      (append a (reverse (cdr b))))
     (else
      (display "******* pseq:join *******\n")
      (pp a)
      (pp b)
      (error "Point sequences cannot be connected")))))

;;; pseq centroid

(define (pseq:centroid plis)
  (define (iter n sum plis-tail)
    (cond
     ((null? plis-tail)
      (vect2:/scalar sum n))
     (else
      (iter
       (add1 n)
       (vect2:+vect2 sum (car plis-tail))
       (cdr plis-tail)))))
  (cond
   ((null? plis)
    (error "Argument #1 should be a point list"))
   ((pseq:closed? plis)
    (iter #e0 (make-point #e0 #e0) (cdr plis)))
   (else
    (iter #e0 (make-point #e0 #e0) plis))))

;;; pseq right-most point

(define (pseq:extreme-right pseq)
  (reduce
   (lambda (current next)
     (cond
      ((< (point-x current) (point-x next))
       next)
      ((= (point-x current) (point-x next))
       (if (< (point-y current) (point-y next)) next current)) ; then top
      (else
       current)))
   (car pseq)
   pseq))

;;; pseq left-most point

(define (pseq:extreme-left pseq)
  (reduce
   (lambda (current next)
     (cond
      ((> (point-x current) (point-x next))
       next)
      ((= (point-x current) (point-x next))
       (if (> (point-y current) (point-y next)) next current)) ; then bottom
      (else
       current)))
   (car pseq)
   pseq))

;;; pseq top-most point

(define (pseq:extreme-top pseq)
  (reduce
   (lambda (current next)
     (cond
      ((< (point-y current) (point-y next))
       next)
      ((= (point-y current) (point-y next))
       (if (< (point-x current) (point-x next)) next current)) ; then right
      (else
       current)))
   (car pseq)
   pseq))

;;; pseq bottom-most point

(define (pseq:extreme-bottom pseq)
  (reduce
   (lambda (current next)
     (cond
      ((> (point-y current) (point-y next))
       next)
      ((= (point-y current) (point-y next))
       (if (> (point-x current) (point-x next)) next current)) ; then left
      (else
       current)))
   (car pseq)
   pseq))

;;; Find a common point of two given point lists

(define (pseq:common-point? plis1 plis2)
  (find
    (lambda (e)
      (any (lambda (it) (vect2:= it e)) plis1))
    plis2))

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

;;; Calculate the tangent vector in a point-list given the relative position

(define (pseq:tangent-in-relative plis rel)
  (let ((approx (pseq->segment plis))) ; TODO: handle pseqs properly
                                        ;    (vect2:normalize
      (segment->direction
        (make-segment
          (segment:relative-position->point approx rel)
          (segment-b approx)))))

;;; Calculate the normalized tangent vector in a point-list given the relative position

(define (pseq:~normalized-tangent-in-relative plis rel)
  (let ((approx (pseq->segment plis))) ; TODO: handle pseqs properly
    (vect2:~normalize
      (segment->direction
        (make-segment
          (segment:relative-position->point approx rel)
          (segment-b approx))))))

;;; TODO

(define (pseq:relative-position->point pseq r)
  (segment:relative-position->point (pseq->segment pseq) r))

;;; Clip a pseq between the intersections of two lines

(define (pseq:clip/lines pseq la lb)
  (error "unimplemented"))

;;; Clip a pseq between the intersections of two lines ensuring is done clockwise

(define (pseq:clip/lines-clockwise pseq la lb)
  (error "unimplemented"))

;;; Clip a pseq between the intersections of two lines ensuring is done clockwise

(define (pseq:clip/lines-counterclockwise pseq la lb)
  (error "unimplemented"))

;-------------------------------------------------------------------------------
; Bounding boxes
;-------------------------------------------------------------------------------

(define-structure bbox lefttop rightbottom)

;;; Calculate the diagonal segment connecting the two extremes of the bb

(define (bbox:diagonal-segment bb)
  (make-segment (bbox-lefttop bb)
                (bbox-rightbottom bb)))

;;; Bounding box size segment

(define (bbox:size-segment bb)
  (segment->direction (bbox:diagonal-segment bb)))

;;; Calculate left-bottom

(define (bbox-leftbottom bb)
  (make-point (point-x (bbox-lefttop bb))
              (point-y (bbox-rightbottom bb))))

;;; Calculate right-top

(define (bbox-righttop bb)
  (make-point (point-x (bbox-rightbottom bb))
              (point-y (bbox-lefttop bb))))

;;; Bounding box centroid

(define (bbox:centroid bb)
  (pseq:centroid (list
                  (bbox-lefttop bb)
                  (bbox-righttop bb)
                  (bbox-rightbottom bb)
                  (bbox-leftbottom bb))))

;-------------------------------------------------------------------------------
; Rotation
;-------------------------------------------------------------------------------

;;; Point rotation

(define (rotate.point vec r-angle)
  (make-point (- (* (point-x vec) (cos r-angle))
                 (* (point-y vec) (sin r-angle)))
              (+ (* (point-y vec) (cos r-angle))
                 (* (point-x vec) (sin r-angle)))))

;;; Point rotation with a reference

(define (rotate.point-w/reference ref p r-angle)
  (vect2:+vect2 ref
    (rotate.point
      (vect2:-vect2 p ref)
      r-angle)))

;;; Direction rotation

(define (rotate.direction d)
  (error "unimplemented"))

;-------------------------------------------------------------------------------
; Translation
;-------------------------------------------------------------------------------

;;; Point translation

(define (translate.point p vec)
  (vect2:+vect2 p vec))

;;; Direction translation

(define (translate.direction dir vec)
  (error "unimplemented"))

;;; Line translation

(define (translate.line line vec)
  (point+direction->line (translate.point (line:point line 0)
                                          vec)
                         (line->direction line)))

;-------------------------------------------------------------------------------
; Squared distances
;-------------------------------------------------------------------------------

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
;-------------------------------------------------------------------------------
; Distances
;-------------------------------------------------------------------------------

;;; Calculate the distance between two points

(define (~distance.point-point a b)
  (sqrt (squareddistance.point-point a b)))

;;; Calculate the distance between point and segment

(define (~distance.point-segment p sg)
  (sqrt (squareddistance.point-segment p sg)))

;;; Calculate the distance between a point and a point list

(define (~distance.point-pseq p plis)
  (cond
   ((or (null? plis) (null? (cdr plis)))
    +inf.0)
   (else
    (min (~distance.point-segment
           p
           (make-segment (car plis) (cadr plis)))
         (~distance.point-pseq
           p
           (cdr plis))))))

;-------------------------------------------------------------------------------
; Intersections
;-------------------------------------------------------------------------------

;;; Segment-segment intersection

(define (intersection.segment-segment sg1 sg2)
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
    (if (= u-b #e0)
        (if (or (= ua-t #e0) (= ub-t #e0))
            'coincident
          'parallel)
      (let ((ua (/ ua-t u-b))
            (ub (/ ub-t u-b)))
        (if (and (<= #e0 ua)
                 (<= ua #e1)
                 (<= #e0 ub)
                 (<= ub #e1))
            (make-point (* (+ (point-x a1) ua)
                             (- (point-x a2) (point-x a1)))
                          (* (+ (point-y a1) ua)
                             (- (point-y a2) (point-y a1))))
          'no-intersection)))))

;;; Segment-pseq intersection

(define (intersection.segment-pseq seg pol)
  (define (append-next intersections pol-rest)
    (let ((inters (intersection.segment-segment seg (make-segment (car pol-rest) (cadr pol-rest)))))
      (if (or (null? pol-rest) (< (length pol-rest) 3))
          (append intersections (list inters))
        (if (point? inters)
            (append-next (append intersections (list inters)) (cdr pol-rest))
          (append-next intersections (cdr pol-rest))))))
  (append-next '() pol))

;;; Segment-pseq (closed pseq) intersection

;(define (intersection.segment-pseq seg plis)
 ; (intersection.segment-pseq seg (pseq:close plis)))

;;; Infinite line - segment intersection

(define (intersection.line-segment line seg)
  (aif i point? (intersection.line-line line (segment->line seg))
       (if (segment:collinear-point-on? seg i)
           i
           'projection-intersection)
       i))

;;; Infinite line - infinite line interesection

(define (intersection.line-line l1 l2)
  (let ((l1a (line-a l1))
        (l1b (line-b l1))
        (l1c (line-c l1))
        (l2a (line-a l2))
        (l2b (line-b l2))
        (l2c (line-c l2)))
    (aif den zero? (- (* l1a l2b)
                      (* l2a l1b))
          (if (and (zero? (- (* l1a l2c)
                             (* l2a l1c)))
                   (zero? (- (* l1b l2c)
                             (* l2b l1c))))
              'line
            'no-intersection)
      (aif nom1 finite? (- (* l1b l2c)
                           (* l2b l1c))
        (aif nom2 finite? (- (* l2a l1c)
                             (* l1a l2c))
          (make-point (/ nom1 den)
                      (/ nom2 den))
          'no-intersection)
        'no-intersection))))

;-------------------------------------------------------------------------------
; Predicates
;-------------------------------------------------------------------------------

;;; Are these points collinear and ordered (left-to-right or right-to-left)?
;;; TODO: Convert to pseqs!!

(define (collinear-ordered-points? p q r)
  (cond
   ((< (point-x p) (point-x q))
    (> (point-x r) (point-x q)))
   ((< (point-x q) (point-x p))
    (> (point-x q) (point-x r)))
   ((< (point-y p) (point-y q))
    (> (point-y r) (point-y q)))
   ((< (point-y q) (point-y p))
    (> (point-y q) (point-y r)))
   (else #t)))

;; (define (<e a b)
;;   (< (+ a 0.01) b))

;; (define (>e a b)
;;   (> a (+ b 0.01)))

;; (define (collinear-ordered-points? p q r)
;;   (cond
;;    ((<e (point-x p) (point-x q))
;;     (>e (point-x r) (point-x q)))
;;    ((<e (point-x q) (point-x p))
;;     (>e (point-x q) (point-x r)))
;;    ((<e (point-y p) (point-y q))
;;     (>e (point-y r) (point-y q)))
;;    ((<e (point-y q) (point-y p))
;;     (>e (point-y q) (point-y r)))
;;    (else #t)))

;;; Are these points collinear and strictly ordered?

(define (collinear-strictly-ordered-points? p q r)
  (cond
   ((< (point-x p) (point-x q))
    (< (point-x q) (point-x r)))
   ((< (point-x q) (point-x p))
    (< (point-x r) (point-x q)))
   ((< (point-y p) (point-y q))
    (< (point-y q) (point-y r)))
   ((< (point-y q) (point-y p))
    (< (point-y r) (point-y q)))
   (else
    #f)))


;;; Are these points collinear and ordered (left-to-right or right-to-left)?
;;; TODO

(define (<e a b)
  (< (+ a 0.01) b))

(define (>e a b)
  (> a (+ b 0.01)))

(define (~collinear-ordered-points? p q r)
  (cond
   ((<e (point-x p) (point-x q))
    (>e (point-x r) (point-x q)))
   ((<e (point-x q) (point-x p))
    (>e (point-x q) (point-x r)))
   ((<e (point-y p) (point-y q))
    (>e (point-y r) (point-y q)))
   ((<e (point-y q) (point-y p))
    (>e (point-y q) (point-y r)))
   (else #t)))

;;; Are these points collinear and strictly ordered?

(define (~collinear-strictly-ordered-points? p q r)
  (cond
   ((<e (point-x p) (point-x q))
    (<e (point-x q) (point-x r)))
   ((<e (point-x q) (point-x p))
    (<e (point-x r) (point-x q)))
   ((<e (point-y p) (point-y q))
    (<e (point-y q) (point-y r)))
   ((<e (point-y q) (point-y p))
    (<e (point-y r) (point-y q)))
   (else
    #f)))
