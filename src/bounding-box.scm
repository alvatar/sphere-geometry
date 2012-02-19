;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bounding boxes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare (standard-bindings)
         (extended-bindings)
         (block))

(import (srfi 1-lists)
        kernel)

(define-structure bbox lefttop rightbottom)

;;; left-bottom

(define (bbox-leftbottom bb)
  (make-point (point-x (bbox-lefttop bb))
              (point-y (bbox-rightbottom bb))))

;;; right-top

(define (bbox-righttop bb)
  (make-point (point-x (bbox-rightbottom bb))
              (point-y (bbox-lefttop bb))))

;-------------------------------------------------------------------------------
; Properties
;-------------------------------------------------------------------------------

;;; Calculate the diagonal segment connecting the two extremes of the bb

(define (bbox:diagonal-segment bb)
  (make-segment (bbox-lefttop bb)
                (bbox-rightbottom bb)))

;;; Bounding box size segment

(define (bbox:size-segment bb)
  (segment->direction (bbox:diagonal-segment bb)))

;;; Bounding box centroid

(define (bbox:centroid bb)
  (pseq:centroid (list
                  (bbox-lefttop bb)
                  (bbox-righttop bb)
                  (bbox-rightbottom bb)
                  (bbox-leftbottom bb))))

;-------------------------------------------------------------------------------
; Bounding boxes of elements
;-------------------------------------------------------------------------------

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
