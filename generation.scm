;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Geometry generation procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import math/exact-algebra)

(import kernel)

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
