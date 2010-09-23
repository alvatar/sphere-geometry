;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bounding boxes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (declare (standard-bindings)
;;          (extended-bindings)
;;          (block))
;; (compile-options force-compile: #t)

(import (std srfi/1)
        kernel)

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
; Element bounding boxes
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
