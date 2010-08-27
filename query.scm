;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Geometric queries: search and sort
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1)
        ../core/functional
        kernel)

;-------------------------------------------------------------------------------
; Extremes
;-------------------------------------------------------------------------------

;;; General extreme query

(define (find.extreme pseq selector tie-selector)
  (reduce
   (lambda (p extreme)
     (let ((a (selector p))
           (b (selector extreme)))
      (cond
       ((< a b) extreme)
       ((= a b) (cond
                 ((< (tie-selector p)
                     (tie-selector extreme)) extreme)
                 (else p)))
       (else p))))
   (car pseq)
   pseq))

;;; pseq right-most point

(define pseq:extreme-right ;(curry find.extreme point-x point-x point-y point-y)
  (lambda (pseq)
    (find.extreme pseq point-x point-y)))

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

;-------------------------------------------------------------------------------
; Nearest queries
;-------------------------------------------------------------------------------

;;; Find in a pseq the segment that is nearest to a point

(define (find.nearest-segment-to-point/pseq p pseq) ; TODO: should return several in case they are at the same dist?
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

(define (find.nearest-segment-to-point/segment-list p sl) ; TODO: should return several in case they are at the same dist?
  ;; TODO: not tested!
  (%accept (and (list? pseq) (segment? (car sl))) "not a proper segment list")
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