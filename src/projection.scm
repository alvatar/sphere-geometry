;;; Copyright (c) 2012 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Projections
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (error "Not implemented"))

;;; Segment projection on a line

(define (project.line<-segment l s)
  (make-segment
   (project.line<-point l (segment-a s))
   (project.line<-point l (segment-b s))))

;;; Pseq projection on a line

(define (project.line<-pseq l ps)
  (error "Not implemented"))


