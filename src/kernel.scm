;;; Copyright (c) 2012 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Geometric kernel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare (standard-bindings)
         (extended-bindings)
         (block))

;;; TODO: 2d structures should be renamed with 2d suffix

;-------------------------------------------------------------------------------
; Point 2d
;-------------------------------------------------------------------------------

(define make-point make-vect2)
(define point? vect2?)
(define point-x vect2-x)
(define point-y vect2-y)
(define point:= vect2:=)

;;; Test whether the points are collinear

(define (point:3-collinear? p q r)
  (let ((px (point-x p)) (py (point-y p))
        (qx (point-x q)) (qy (point-y q))
        (rx (point-x r)) (ry (point-y r)))
    (zero?
     (determinant-2x2 (- qx px) (- qy py)
                      (- rx px) (- ry py)))))

;;; Are these points collinear and ordered (left-to-right or right-to-left)?

(define (point:3-collinear-ordered? p q r)
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

(define (point:3-collinear-strictly-ordered? p q r)
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
;;;
;;; TODO: set tolerance

; (define (<e a b)
;   (< (+ a 0.01) b))
; 
; (define (>e a b)
;   (> a (+ b 0.01)))
; 
; (define (~point:3-collinear-ordered? p q r)
;   (cond
;    ((<e (point-x p) (point-x q))
;     (>e (point-x r) (point-x q)))
;    ((<e (point-x q) (point-x p))
;     (>e (point-x q) (point-x r)))
;    ((<e (point-y p) (point-y q))
;     (>e (point-y r) (point-y q)))
;    ((<e (point-y q) (point-y p))
;     (>e (point-y q) (point-y r)))
;    (else #t)))
; 
; ;;; Are these points collinear and strictly ordered?
; 
; (define (~point:3-collinear-strictly-ordered? p q r)
;   (cond
;    ((<e (point-x p) (point-x q))
;     (<e (point-x q) (point-x r)))
;    ((<e (point-x q) (point-x p))
;     (<e (point-x r) (point-x q)))
;    ((<e (point-y p) (point-y q))
;     (<e (point-y q) (point-y r)))
;    ((<e (point-y q) (point-y p))
;     (<e (point-y r) (point-y q)))
;    (else
;     #f)))

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

(define (line:2-points-in-same-halfplane? line p1 p2)
  (not (point? (intersect.line-segment line (make-segment p1 p2)))))

;-------------------------------------------------------------------------------
; Ray (semi-infinite line) 2d
;-------------------------------------------------------------------------------

(define-structure ray o direction)

;-------------------------------------------------------------------------------
; Segments
;-------------------------------------------------------------------------------

(define-structure segment a b)

;;; Loose equality (they can be reversed)

(define (segment:= s1 s2)
  (let ((a1 (segment-a s1))
        (a2 (segment-a s2))
        (b1 (segment-b s1))
        (b2 (segment-b s2)))
   (or (and (point:= a1 a2)
            (point:= b1 b2))
       (and (point:= a1 b2)
            (point:= b1 a2)))))

;;; Strict equality (they can't be reversed)

(define (segment:=* s1 s2)
  (and (point:= (segment-a s1) (segment-a s2))
       (point:= (segment-b s1) (segment-b s2))))

;;; Convert to inexact segment

(define (~segment:exact->inexact s)
  (make-segment
   (vect2:exact->inexact (segment-a s))
   (vect2:exact->inexact (segment-b s))))

;;; Convert to exact segment

(define (segment:inexact->exact s)
  (make-segment
   (vect2:inexact->exact (segment-a s))
   (vect2:inexact->exact (segment-b s))))

;;; Segment length

(define (~segment:length seg)
  (~vect2:magnitude (segment->direction seg)))

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

;;; Tell whether the two segments are connected

(define (segment:1-connected? s1 s2)
  (or (segment:end-point? s2 (segment-a s1))
      (segment:end-point? s2 (segment-b s1))))

;;; Is point collinear to segment?

(define (segment:point-collinear? seg p)
  (collinear-points? (segment-a seg) p (segment-b seg)))

;;; Is point collinear and on the segment?

(define (segment:point-collinear&on? seg p)
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
    (line:2-points-in-same-halfplane?
      (point&point->line
        (second merged-pseq)
        (third merged-pseq))
      (first merged-pseq)
      (fourth merged-pseq))))

;;; Tell whether the segments are parallel

(define (segment:2-parallel? s1 s2)
  (vect2:proportional?
   (segment->direction s1)
   (segment->direction s2)))

;;; Calculate absolute point given segment and percent

(define (segment:normalized-1d->point2d seg rel)
  (let ((vec (segment->direction seg))
        (o (segment-a seg)))
    (make-point (+ (point-x o) (* (point-x vec) rel))
                (+ (point-y o) (* (point-y vec) rel)))))

;;; Calculate relative position given a collinear point on the segment

(define (segment:point2d->normalized-1d seg p)
  (if (segment:point-collinear&on? seg p)
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

(define (segment:point2d->normalized-1d* seg p)
  (if (segment:point-collinear? seg p)
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

;;; Calculate a point given a 1d coordinate in a segment

;; TODO: ¡se puede hacer sin normalizar!
(define (~segment:1d-coord->point2d seg coord)
  (let ((vec (~vect2:normalize (segment->direction seg)))
        (o (segment-a seg)))
    (make-point (+ (point-x o) (* (point-x vec) coord))
                (+ (point-y o) (* (point-y vec) coord)))))

;;; Calculate the segment's mid point

(define (segment:mid-point seg)
  (let ((a (segment-a seg))
        (b (segment-b seg)))
    (make-point (/ (+ (point-x a) (point-x b)) 2)
                (/ (+ (point-y a) (point-y b)) 2))))

;;; Calculate the 1d coordinate in a segment given a collinear point

(define (segment:point2d->1d-coord seg p)
  (error "Not implemented"))

;;; Calculate the 1d coordinate in a segment gicen a collinear point
;;; This version calculates positions out of the segment but still collinear

(define (segment:2dpoint->1d-coord* seg p)
  (error "Not implemented"))

;-------------------------------------------------------------------------------
; Point sequences
;-------------------------------------------------------------------------------

(define-type pseq
  closed?
  points)

;;; Is pseq well-formed?

(define (pseq:well-formed? pseq)
  (let ((points (pseq-points pseq)))
   (and (list? points)
        (not-null? plis)
        (every point? plis))))

;;; Is pseq? only first is checked

(define (pseq?-shallow plis)
  (and (list? plis)
       (not-null? plis)
       (point? (car plis))))

;;; Length of a pseq

(define (~pseq:length pseq)
  (pair-fold
   (lambda (pair accum)
     (+
      (if (null? (cdr pair))
          0
	  (~segment:length (make-segment (car pair) (cadr pair))))
      accum))
   0
   pseq))

;;; Is end point?

(define (pseq:end-point? pseq p)
  (or (point:= (first pseq) p)
      (point:= (last pseq) p)))

;;; Are these pseq connected?

(define (pseq:1-connected? p1 p2)
  (or (pseq:end-point? p2 (first p1))
      (pseq:end-point? p2 (last p1))))

;;; Are these pseq completely parallel?

(define (pseq:1-parallel? p1 p2)
  (segment:2-parallel? (pseq->segment p1) ; TODO: obiviosuly needs generalization
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

(define (pseq:2d-points->1d-coords pseq)
  (error "Not implemented"))

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
  (error "Not implemented"))

;;; Get a point from a relative position in a pseq

(define (pseq:normalized-1d->point2d pseq x)
  (segment:normalized-1d->point
   (pseq->segment pseq)
   x)) ; TODO: consider general case!!!!!!!!!!!

;;; Clip a pseq between the intersections of two lines

(define (pseq:clip/lines pseq la lb)
  (error "Not implemented"))

;;; Clip a pseq between the intersections of two lines ensuring is done clockwise

(define (pseq:clip/lines-clockwise pseq la lb)
  (error "Not implemented"))

;;; Clip a pseq between the intersections of two lines ensuring is done clockwise

(define (pseq:clip/lines-counterclockwise pseq la lb)
  (error "Not implemented"))

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

(define (pseq:point2d-inside? point-list p)
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
          (segment:normalized-1d->point approx rel)
          (segment-b approx)))))

;;; Calculate the normalized tangent vector in a point-list given the relative position

(define (~pseq:normalized-tangent-in-relative plis rel)
  (let ((approx (pseq->segment plis))) ; TODO: handle pseqs properly
    (~vect2:normalize
      (segment->direction
        (make-segment
          (segment:normalized-1d->point approx rel)
          (segment-b approx))))))

;;; A list of all midsegments in a pseq

(define (pseq:midsegments pseq)
  ;; TODO: this might be the least efficient option of all!
  (delete-duplicates
   (pair-fold-2
    (lambda (refps mids)
      (append (pair-fold-2
               (lambda (refps2 mids2)
                 (cons (make-segment
                        (point&point->midpoint (car refps)
                                               (cadr refps))
                        (point&point->midpoint (car refps2)
                                               (cadr refps2)))
                       mids2))
               '()
               pseq)
              mids))
    '()
    pseq)
   segment:=))

;;; A list of all diagonals of a pseq

(define (pseq:diagonals pseq)
  ;; TODO: slow and maybe buggy (not tested)
  (delete-duplicates
   (pair-fold-x
    3
    (lambda (refps mids)
      (append (pair-fold-2
               (lambda (refps2 mids2)
                 (cons (make-segment (car refps)
                                     (caddr refps))
                       mids2))
               '()
               pseq)
              mids))
    '()
    pseq)
   segment:=))

