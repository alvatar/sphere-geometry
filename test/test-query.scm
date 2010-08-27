;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tests for geometry queries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/64)
        ../../math/exact-algebra
        ../kernel
        ../query)

(define-syntax test-equal/=
  (syntax-rules ()
   ((_ name =f expr result)
    (test-assert name (=f expr result)))))

;-------------------------------------------------------------------------------
(test-begin "query")
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
; Extremes
;-------------------------------------------------------------------------------

(test-equal/= "extreme-right"
              vect2:=
              (pseq:extreme-right
               (list (make-point 0.0 1.0)
                     (make-point 1.0 1.0)
                     (make-point 2.0 2.0)
                     (make-point 2.0 3.0)))
              (make-point 2.0 3.0))

(test-equal/= "extreme-bottom"
              vect2:=
              (pseq:extreme-bottom
               (list (make-point 0.0 1.0)
                     (make-point 1.0 1.0)
                     (make-point 2.0 3.0)
                     (make-point 3.0 3.0)))
              (make-point 0.0 1.0))

;-------------------------------------------------------------------------------
(test-end "query")
;-------------------------------------------------------------------------------