;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Geometric kernel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (declare (standard-bindings)
;;          (extended-bindings)
;;          (block))
;; (compile-options force-compile: #t)

(import (std srfi/1)
        ../core/list
        ../core/syntax
        ../core/debugging
        ../math/exact-algebra
        ../math/inexact-algebra)

(%activate-checks)

;-------------------------------------------------------------------------------
; Point 2d
;-------------------------------------------------------------------------------

(define make-point make-vect2)
(define point? vect2?)
(define point-x vect2-x)
(define point-y vect2-y)
(define point:= vect2:=)

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

;;; Reverse direction

(define (direction:reverse dir)
  (make-direction (- (direction-x dir))
                  (- (direction-y dir))))

;;; Direction to angle

(define (direction:angle-rad dir)
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

;;; Are points in same half-plane?
;;;
;;;    \       p1
;;;     \     x
;;;      \
;;;       \  x
;;;        \  p2

(define (line:are-points-in-same-halfplane? line p1 p2)
  (not (point? (intersect.line-segment line (make-segment p1 p2)))))

;-------------------------------------------------------------------------------
; Ray (semi-infinite line) 2d
;-------------------------------------------------------------------------------

(define-structure ray o direction)

;-------------------------------------------------------------------------------
; Segments
;-------------------------------------------------------------------------------

(define-structure segment a b)

;;; Segment length

(define (segment:~length seg)
  (vect2:~magnitude (segment->direction seg)))

;;; Segment squared length

(define (segment:squaredlength seg)
  (vect2:squaredmagnitude (segment->direction seg)))

;;; Reverse segment

(define (segment:reverse seg)
  (make-segment (segment-b seg)
                (segment-a seg)))

;;; Tell whether the point is an end point of the segment

(define (segment:end-point? seg p)
  (or (point:= p (segment-a seg))
      (point:= p (segment-b seg))))

;;; Is point collinear to segment?

(define (segment:collinear-point? seg p)
  (collinear-points? (segment-a seg) p (segment-b seg)))

;;; Is point collinear and on the segment?

(define (segment:collinear-point-on? seg p)
  (collinear-ordered-points? (segment-a seg) p (segment-b seg)))

;;; Are the 3 segments in the same half-plane, using the middle one for
;;; supporting the line
;;;          x
;;;      x    \ s3 /
;;;       \    -\ /
;;;        \     X
;;;      s1 \   /
;;;          \ / s2
;;;           X
;;;          /      Here, s1 and s3 lie in the same half-plane
;;;         /
              
(define (segment:3-in-same-halfplane/middle s1 s2 s3) ; TODO: should be halfplane: in query module?
  (let ((merged-pseq (pseq:append
                      (pseq:append
                       (segment->pseq s1)
                       (segment->pseq s2))
                      (segment->pseq s3))))
    (%accept (length= merged-pseq 4) "merged segments in pseq doesn't have 4 points as expected")
    (line:are-points-in-same-halfplane? (point&point->line
                                         (second merged-pseq)
                                         (third merged-pseq))
                                        (first merged-pseq)
                                        (fourth merged-pseq))))

;;; Tell whether the two segments are connected

(define (segment:connected-segment? s1 s2)
  (or (segment:end-point? s2 (segment-a s1))
      (segment:end-point? s2 (segment-b s1))))

;;; Tell whether the segments are parallel

(define (segment:parallel-segment? s1 s2)
  (vect2:proportional?
   (segment->direction s1)
   (segment->direction s2)))

;;; Calculate absolute point given segment and percent

(define (segment:1d-coord->point seg rel)
  (let ((vec (segment->direction seg))
        (o (segment-a seg)))
    (make-point (+ (point-x o) (* (point-x vec) rel))
                (+ (point-y o) (* (point-y vec) rel)))))

;;; Calculate relative position given a collinear point on the segment

(define (segment:point->1d-coord seg p)
  (if (segment:collinear-point-on? seg p)
      (let ((s1 (segment-a seg))
            (s2 (segment-b seg)))
        (cond
         ((= (point-x s1) (point-x s2))
          (/ (- (point-y p) (point-y s1)) (- (point-y s2) (point-y s1))))
         ((= (point-y s1) (point-y s2))
          (/ (- (point-x p) (point-x s1)) (- (point-x s2) (point-x s1))))
         (else
          (%accept (= (/ (- (point-y p) (point-y s1)) (- (point-y s2) (point-y s1)))
                      (/ (- (point-x p) (point-x s1)) (- (point-x s2) (point-x s1))))
                   "Point does not lie on the segment. Check exactness of components")
          (/ (- (point-y p) (point-y s1)) (- (point-y s2) (point-y s1))))))
      'not-collinear))

;;; Calculate relative position given a collinear point on the segment's line
;;; This version calculates positions out of the segment but still collinear

(define (segment:point->1d-coord* seg p)
  (if (segment:collinear-point? seg p)
      (let ((s1 (segment-a seg))
            (s2 (segment-b seg)))
        (cond
         ((= (point-x s1) (point-x s2))
          (/ (- (point-y p) (point-y s1)) (- (point-y s2) (point-y s1))))
         ((= (point-y s1) (point-y s2))
          (/ (- (point-x p) (point-x s1)) (- (point-x s2) (point-x s1))))
         (else
          (%accept (= (/ (- (point-y p) (point-y s1)) (- (point-y s2) (point-y s1)))
                      (/ (- (point-x p) (point-x s1)) (- (point-x s2) (point-x s1))))
                   "Point does not lie on the segment. Check exactness of components")
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
       (every point? plis)))

;;; Is pseq? only first is checked

(define (pseq?-shallow plis)
  (and (list? plis)
       (not-null? plis)
       (point? (car plis))))

;;; Calculate the bounding point of a pseq

(define (pseq:bbox point-list)
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

(define (pseq:end-point? pseq p)
  (%accept (and (pseq? pseq) (point? p)))
  (or (point:= (first pseq) p)
      (point:= (last pseq) p)))

;;; Are these pseq connected?

(define (pseq:connected-pseq? p1 p2)
  (or (pseq:end-point? p2 (first p1))
      (pseq:end-point? p2 (last p1))))

;;; Are these pseq completely parallel?

(define (pseq:parallel-pseq? p1 p2)
  (segment:parallel-segment? (pseq->segment p1) ; TODO: obiviosuly needs generalization
                             (pseq->segment p2)))

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

;;; Make a list of the 1d coordinates of the pseq points, relative to its subspace

(define (pseq:2d-coords->1d-coords pseq)
  (error "unimplemented"))

;;; Get a sub-pseq from two relative points

(define (pseq:slice pseq a b)
;  (pseq:2d-coords->1d-coords pseq)

  (if (= (length pseq) 2)
      (let* ((Ax (point-x (first pseq)))
             (Ay (point-y (first pseq)))
             (ABx (- (point-x (last pseq)) Ax))
             (ABy (- (point-y (last pseq)) Ay)))
        (list (make-vect2 (+ Ax (* ABx a)) (+ Ay (* ABy a)))
              (make-vect2 (+ Ax (* ABx b)) (+ Ay (* ABy b)))))
      (error "Error - wall element has more than 2 relative points\n"))
      ; Else:
        ; 1. Precalcular lista de puntos relativos
        ; 2. Hacer lista de puntos relativos menores que puerta
        ; 3. Dibujar trayectoria de puerta completa de los segmentos menores
        ; 4. Dibujar el porcentaje restante sobre el siguiente segmento
  )

;;; Get the two sub-pseqs in which a pseq is divided with a point

(define (pseq:partition pseq x)
  (error "unimplemented"))

;;; Get a point from a relative position in a pseq

(define (pseq:1d-coord->point pseq x)
  (segment:1d-coord->point
   (pseq->segment pseq)
   x)) ; TODO: consider general case!!!!!!!!!!!

;;; Clip a pseq between the intersections of two lines

(define (pseq:clip/lines pseq la lb)
  (error "unimplemented"))

;;; Clip a pseq between the intersections of two lines ensuring is done clockwise

(define (pseq:clip/lines-clockwise pseq la lb)
  (error "unimplemented"))

;;; Clip a pseq between the intersections of two lines ensuring is done clockwise

(define (pseq:clip/lines-counterclockwise pseq la lb)
  (error "unimplemented"))

;;; Pseq centroid

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

;;; Pseq area
;;; http://www.mathsisfun.com/geometry/area-irregular-polygons.html
;;; http://mathworld.wolfram.com/PolygonArea.html (this one is implemented)
;;; 1/2 * / | x1 x2 | + | x2 x3 | + ... + | xn x1 | \
;;;       \ | y1 y2 |   | y2 y3 |         | yn y1 | /

(define (pseq:area pseq)
  (%accept (pseq:closed? pseq) "pseq must be closed in order to calculate area")
  (abs
   (* 1/2
      (pair-fold (lambda (pair accum)
                   (if (null? (cdr pair))
                       accum
                       (+ accum
                          (let ((a (car pair))
                                (b (cadr pair)))
                            (- (* (point-x a) (point-y b))
                               (* (point-x b) (point-y a)))))))
                 0
                 pseq))))

;;; Find a common point of two given point lists

(define (pseq:common-point? plis1 plis2)
  (find
    (lambda (e)
      (any (lambda (it) (vect2:= it e)) plis1))
    plis2))

;;; Is point inside the polygon pseq?
;;; http://www.ecse.rpi.edu/Homepages/wrf/Research/Short_Notes/pnpoly.html

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
          (segment:1d-coord->point approx rel)
          (segment-b approx)))))

;;; Calculate the normalized tangent vector in a point-list given the relative position

(define (pseq:~normalized-tangent-in-relative plis rel)
  (let ((approx (pseq->segment plis))) ; TODO: handle pseqs properly
    (vect2:~normalize
      (segment->direction
        (make-segment
          (segment:1d-coord->point approx rel)
          (segment-b approx))))))

;-------------------------------------------------------------------------------
; Basic conversions and locus
;-------------------------------------------------------------------------------

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

(define (ray->line r) ; TODO
  (make-line #e0 #e0 #e0))

;;; Convert line into direction

(define (line->direction line)
  (make-direction (line-b line) (- (line-a line))))

;;; Convert line into segment

(define (line->segment line from to)
  (make-segment (line:point line from)
                (line:point line to)))

;;; Picks the first and the last points of the pseq to build the segment

(define (pseq->segment plis)
  (make-segment (first plis)
                (last plis)))

;;; Convert a pseq to a list of independent segments

(define (pseq->lsegments pseq)
  (map (lambda (a b) (make-segment b a))
       (cdr pseq)
       pseq))

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
; Distances/squared distances
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

;;; Calculate the minimum squared distance between two pseqs, considering only end points

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

;;; Calculate the minimum distance between two pseqs, considering only end points

(define (~distance.pseq-pseq/endpoints ps1 ps2)
  (sqrt (squareddistance.pseq-pseq/endpoints ps1 ps2)))

;-------------------------------------------------------------------------------
; Translation
;-------------------------------------------------------------------------------

;;; Point translation

(define (translate.point p vec)
  (vect2:+vect2 p vec))

;;; Line translation

(define (translate.line line vec)
  (point&direction->line (translate.point (line:point line 0)
                                          vec)
                         (line->direction line)))

;;; Ray translation

(define (translate.ray pseq vec)
  (error "unimplemented"))

;;; Segment translation

(define (translate.segment pseq vec)
  (error "unimplemented"))

;;; Pseq translation

(define (translate.pseq pseq vec)
  (error "unimplemented"))

;-------------------------------------------------------------------------------
; Rotation
;-------------------------------------------------------------------------------

;;; Point rotation with the origin

(define (rotate.point/O vec alpha)
  (make-point (- (* (point-x vec) (cos alpha))
                 (* (point-y vec) (sin alpha)))
              (+ (* (point-y vec) (cos alpha))
                 (* (point-x vec) (sin alpha)))))

;;; Point rotation with a reference

(define (rotate.point/ref ref p alpha)
  (vect2:+vect2 ref
                (rotate.point/O (vect2:-vect2 p ref)
                                alpha)))

;;; Direction rotation

(define (rotate.direction d alpha)
  (error "unimplemented"))

;;; Line rotation

(define (rotate.line l alpha)
  (error "unimplemented"))

;;; Ray rotation

(define (rotate.ray r alpha)
  (error "unimplemented"))

;;; Segment rotation with the origin

(define (rotate.segment/O s alpha)
  (error "unimplemented"))

;;; Segment rotation with a reference point

(define (rotate.segment/ref ref s alpha)
  (error "unimplemented"))

;;; Pseq rotation with the origin

(define (rotate.pseq/O pseq alpha)
  (error "unimplemented"))

;;; Pseq rotation with a reference point

(define (rotate.pseq/ref ref pseq alpha)
  (error "unimplemented"))

;-------------------------------------------------------------------------------
; Scaling
;-------------------------------------------------------------------------------

;;; Point scaling with the origin

(define (scale.point/O p scale)
  (error "unimplemented"))

;;; Point scaling with a reference point

(define (scale.point/ref p scale)
  (error "unimplemented"))

;;; Line scaling with the origin

(define (scale.line/O l scale)
  (error "unimplemented"))

;;; Line scaling with a reference point

(define (scale.line/ref l scale)
  (error "unimplemented"))

;;; Ray scaling with the origin

(define (scale.ray/O r scale)
  (error "unimplemented"))

;;; Ray scaling with a reference point

(define (scale.ray/ref ref r scale)
  (error "unimplemented"))

;;; Segment scaling with the origin

(define (scale.segment/O r scale)
  (error "unimplemented"))

;;; Segment scaling with a refernce point

(define (scale.segment/ref r scale)
  (error "unimplemented"))

;;; Segment scaling with the origin

(define (scale.pseq/O r scale)
  (error "unimplemented"))

;;; Segment scaling with a reference point

(define (scale.pseq/ref r scale)
  (error "unimplemented"))

;-------------------------------------------------------------------------------
; Translation along pseq
;-------------------------------------------------------------------------------

;;; Point translation along pseq

(define (translate-along-pseq.point guide p x)
  (error "unimplemented"))

;;; Line translation along pseq

(define (translate-along-pseq.line guide l x)
  (error "unimplemented"))

;;; Ray translation along pseq

(define (translate-along-pseq.ray guide r x)
  (error "unimplemented"))

;;; Segment translation along pseq

(define (translate-along-pseq.segment guide s x)
  (error "unimplemented"))

;;; Pseq translation along pseq

(define (translate-along-pseq.pseq guide pseq x)
  (error "unimplemented"))

;-------------------------------------------------------------------------------
; Scaling along pseq
;-------------------------------------------------------------------------------

;;; Point scaling along pseq with the guide origin

(define (scale-along-pseq.point/O guide p scale)
  (error "unimplemented"))

;;; Point scaling along pseq with a reference point

(define (scale-along-pseq.point/ref guide ref p scale)
  (error "unimplemented"))

;;; Segment scaling along pseq with the guide origin

(define (scale-along-pseq.segment/O guide s scale)
  (error "unimplemented"))

;;; Segment scaling along pseq with a reference point

(define (scale-along-pseq.segment/ref guide ref s scale)
  (error "unimplemented"))

;;; Pseq scaling along pseq with the guide origin

(define (scale-along-pseq.pseq/O guide pseq scale)
  (error "unimplemented"))

;;; Pseq scaling along pseq with a reference point

(define (scale-along-pseq.pseq/ref guide ref pseq scale)
  pseq)

;-------------------------------------------------------------------------------
; Intersections
;-------------------------------------------------------------------------------

;;; Segment-segment intersection

(define (intersect.segment-segment sg1 sg2)
  (%accept (and (segment? sg1) (segment? sg2)))
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

;;; Segment - Pseq intersection

(define (intersect.segment-pseq seg pol)
  (define (append-next intersections pol-rest)
    (let ((inters (intersect.segment-segment seg
                                             (make-segment (car pol-rest)
                                                           (cadr pol-rest)))))
      (if (or (null? pol-rest) (< (length pol-rest) 3))
          (append intersections (list inters))
          (if (point? inters)
              (append-next (append intersections (list inters)) (cdr pol-rest))
              (append-next intersections (cdr pol-rest))))))
  (%accept (and (segment? seg) (pseq? pol)))
  (append-next '() pol))
(define intersect.pseq-segment intersect.segment-pseq)

;;; Ray - Segment intersection

(define (intersect.ray-segment r s)
  (error "unimplemented"))
(define intersect.segment-ray intersect.ray-segment)

;;; Ray - Infinite line intersection

(define (intersect.ray-line r l)
  (error "unimplemented"))
(define intersect.line-ray intersect.ray-line)

;;; Infinite line - segment intersection

(define (intersect.line-segment line seg)
  (%accept (and (line? line) (segment? seg)))
  (aif int point? (intersect.line-line line (segment->line seg))
       (if (segment:collinear-point-on? seg int)
           int
           'projection-intersection)
       int))
(define intersect.segment-line intersect.line-segment)

;;; Infinite line - pseq intersections

(define (intersect.line-pseq line pseq)
  (%accept (and (line? line) (pseq? pseq)))
  (pair-fold-2 (lambda (tail acc)
                 (aif int point?
                      (intersect.line-segment line (make-segment (car tail)
                                                                 (cadr tail)))
                      (cons int acc)
                      acc))
               '()
               pseq))
(define intersect.pseq-line intersect.line-pseq)

;;; Infinite line - infinite line intersection

(define (intersect.line-line l1 l2)
  (%accept (and (line? l1) (line? l2)))
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
; Projections
;-------------------------------------------------------------------------------

;;; Point projection on a line

(define (project.line<-point l p)
  (%accept point? "a point projected to a line should be a point!"
           (intersect.line-line
            l
            (point&direction->line
             p
             (direction:perpendicular (line->direction l))))))

;;; Line projection on a line

(define (project.line<-line l1 l2)
  (error "unimplemented"))

;;; Segment projection on a line

(define (project.line<-segment l s)
  (make-segment
   (project.line<-point l (segment-a s))
   (project.line<-point l (segment-b s))))

;;; Pseq projection on a line

(define (project.line<-pseq l ps)
  (error "unimplemented"))

;-------------------------------------------------------------------------------
; Predicates
;-------------------------------------------------------------------------------

;;; Are these points collinear?

(define (collinear-points? p q r)
  (let ((px (point-x p)) (py (point-y p))
        (qx (point-x q)) (qy (point-y q))
        (rx (point-x r)) (ry (point-y r)))
    (zero?
     (determinant:2x2 (- qx px) (- qy py)
                      (- rx px) (- ry py)))))

;;; Are these points collinear and ordered (left-to-right or right-to-left)?

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
