;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Geometric queries: search and sort
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare (standard-bindings)
         (extended-bindings)
         (block))

(import (srfi 1-lists)
        (base functional lists)
        kernel)

;-------------------------------------------------------------------------------
; Extremes
;-------------------------------------------------------------------------------

;;; General extreme query

(define (extreme comparator selector
                 tie-comparator tie-selector
                 l)
  (reduce
   (lambda (p extreme)
     (let ((a (selector p))
           (b (selector extreme)))
      (cond
       ((comparator a b) extreme)
       ((= a b) (cond
                 ((tie-comparator (tie-selector p)
                                  (tie-selector extreme)) extreme)
                 (else p)))
       (else p))))
   (car l)
   l))

;;; right-most point

(define extreme.right (curry extreme < point-x < point-y))

;;; left-most point

(define extreme.left (curry extreme > point-x > point-y))

;;; top-most point

(define extreme.top (curry extreme < point-y < point-x))

;;; bottom-most point

(define extreme.bottom (curry extreme > point-y > point-x))

;-------------------------------------------------------------------------------
; Nearest
;-------------------------------------------------------------------------------

;;; Find in a pseq the segment that is nearest to a point

(define (nearest.segment-to-point/pseq p pseq) ; TODO: should return several in case they are at the same dist?
  (%accept (and (pseq? pseq) (> (length pseq) 1)) "not a proper pseq")
  (let ((first-segment (make-segment (car pseq) (cadr pseq))))
    (if (> (length pseq) 2)
        (pair-fold-2 (lambda (tail acc)
                       (let ((current-segment (make-segment (car tail)
                                                            (cadr tail))))
                         (if (< (squareddistance.point-segment p current-segment)
                                (squareddistance.point-segment p acc))
                             current-segment
                             acc)))
                     (make-segment (car pseq) (cadr pseq))
                     (cddr pseq))
        first-segment)))

;;; Find in a pseq the segment that is nearest to a point

(define (nearest.segment-to-point/segment-list p sl) ; TODO: should return several in case they are at the same dist?
  ;; TODO: not tested!
  (%accept (and (list? p) (segment? (car sl))) "not a proper segment list")
  (if (> (length sl) 1)
      (pair-fold-2 (lambda (tail acc)
                     (let ((current-segment (car tail)))
                       (if (< (squareddistance.point-segment p current-segment)
                              (squareddistance.point-segment p acc))
                           current-segment
                           acc)))
                   (car sl)
                   (cdr sl))
      (car sl)))

;-------------------------------------------------------------------------------
; Directions
;-------------------------------------------------------------------------------

;;;
;;; TODO: these "longest" ng procedures should give information on unique results

;;; Find the direction that describes the longest possible diagonal

(define (direction.longest-diagonal pseq)
  (max/generator segment:squaredlength
                 (pseq:diagonals pseq)))

;;; Find the direction that describes the longest possible segment connectig
;;; side mid-points

(define (direction.longest-midsegment pseq)
  (max/generator segment:squaredlength
                 (pseq:midsegments pseq)))

;;; Find the direction that describes the longest possible segment produced by
;;; the intersection of a line passing through the center of the polygon

(define (direction.longest-intersections pseq)
  (error "unimplemented"))

;;; Direction of the line passing through a point and perpendicular to a pseq

(define (direction.point&pseq-perpendicular p pseq)
  (direction:perpendicular (segment->direction
                            (nearest.segment-to-point/pseq p pseq))))

;-------------------------------------------------------------------------------
; Distances
;-------------------------------------------------------------------------------

(define (haversine ? ?)
  (error "Not implemented"))

;-------------------------------------------------------------------------------
; Statistical
;-------------------------------------------------------------------------------

(define (geometric-median pseq)
  (error "Not implemented"))
