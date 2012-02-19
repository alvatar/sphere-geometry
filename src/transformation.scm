;;; Copyright (c) 2012 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Transformations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (error "Not implemented"))

;;; Segment translation

(define (translate.segment pseq vec)
  (error "Not implemented"))

;;; Pseq translation

(define (translate.pseq pseq vec)
  (error "Not implemented"))

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
  (error "Not implemented"))

;;; Line rotation

(define (rotate.line l alpha)
  (error "Not implemented"))

;;; Ray rotation

(define (rotate.ray r alpha)
  (error "Not implemented"))

;;; Segment rotation with the origin

(define (rotate.segment/O s alpha)
  (error "Not implemented"))

;;; Segment rotation with a reference point

(define (rotate.segment/ref ref s alpha)
  (error "Not implemented"))

;;; Pseq rotation with the origin

(define (rotate.pseq/O pseq alpha)
  (error "Not implemented"))

;;; Pseq rotation with a reference point

(define (rotate.pseq/ref ref pseq alpha)
  (error "Not implemented"))

;-------------------------------------------------------------------------------
; Scaling
;-------------------------------------------------------------------------------

;;; TODO: Merge /O and /ref, using an optional argument for reference

;;; Point scaling with the origin

(define (scale.point/O p scale)
  (error "Not implemented"))

;;; Point scaling with a reference point

(define (scale.point/ref p scale)
  (error "Not implemented"))

;;; Line scaling with the origin

(define (scale.line/O l scale)
  (error "Not implemented"))

;;; Line scaling with a reference point

(define (scale.line/ref l scale)
  (error "Not implemented"))

;;; Ray scaling with the origin

(define (scale.ray/O r scale)
  (error "Not implemented"))

;;; Ray scaling with a reference point

(define (scale.ray/ref ref r scale)
  (error "Not implemented"))

;;; Segment scaling with the origin

(define (scale.segment/O r scale)
  (error "Not implemented"))

;;; Segment scaling with a refernce point

(define (scale.segment/ref r scale)
  (error "Not implemented"))

;;; Segment scaling with the origin

(define (scale.pseq/O r scale)
  (error "Not implemented"))

;;; Segment scaling with a reference point

(define (scale.pseq/ref r scale)
  (error "Not implemented"))

;-------------------------------------------------------------------------------
; Translation along pseq
;-------------------------------------------------------------------------------

;;; Point translation along pseq

(define (translate-along-pseq.point guide p x)
  (error "Not implemented"))

;;; Line translation along pseq

(define (translate-along-pseq.line guide l x)
  (error "Not implemented"))

;;; Ray translation along pseq

(define (translate-along-pseq.ray guide r x)
  (error "Not implemented"))

;;; Segment translation along pseq

(define (translate-along-pseq.segment guide s x)
  (error "Not implemented"))

;;; Pseq translation along pseq

(define (translate-along-pseq.pseq guide pseq x)
  (error "Not implemented"))

