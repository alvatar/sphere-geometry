;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Geometric queries: search and sort
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import kernel)

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