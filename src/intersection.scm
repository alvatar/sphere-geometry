;;; Copyright (c) 2012 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Intersections
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Segment-segment intersection

(define (intersect.segment-segment sg1 sg2)
  (%accept (and (segment? sg1) (segment? sg2)))
  (let* ((a1 (car sg1))
         (a2 (cadr sg1))
         (b1 (car sg2))
         (b2 (cadr sg2))
         (ua-t (- (* (- (point-x b2) (point-x b1))
                     (- (point-y a1) (point-y b1)))
                  (* (- (point-y b2) (point-y b1))
                     (- (point-x a1) (point-x b1)))))
         (ub-t (- (* (- (point-x a2) (point-x a1))
                     (- (point-y a1) (point-y b1)))
                  (* (- (point-y a2) (point-y a1))
                     (- (point-x a1) (point-x b1)))))
         (u-b (- (* (- (point-y b2) (point-y b1))
                    (- (point-x a2) (point-x a1)))
                 (* (- (point-x b2) (point-x b1))
                    (- (point-y a2) (point-y a1))))))
    (if (= u-b #e0)
        (if (or (= ua-t #e0) (= ub-t #e0))
            'coincident
          'parallel)
      (let ((ua (/ ua-t u-b))
            (ub (/ ub-t u-b)))
        (if (and (<= #e0 ua)
                 (<= ua #e1)
                 (<= #e0 ub)
                 (<= ub #e1))
            (make-point (* (+ (point-x a1) ua)
                             (- (point-x a2) (point-x a1)))
                          (* (+ (point-y a1) ua)
                             (- (point-y a2) (point-y a1))))
          'no-intersection)))))

;;; Segment - Pseq intersection

(define (intersect.segment-pseq seg pol)
  (define (append-next intersections pol-rest)
    (let ((inters (intersect.segment-segment seg
                                             (make-segment (car pol-rest)
                                                           (cadr pol-rest)))))
      (if (or (null? pol-rest) (< (length pol-rest) 3))
          (append intersections (list inters))
          (if (point? inters)
              (append-next (append intersections (list inters)) (cdr pol-rest))
              (append-next intersections (cdr pol-rest))))))
  (%accept (and (segment? seg) (pseq? pol)))
  (append-next '() pol))

(define intersect.pseq-segment intersect.segment-pseq)

;;; Ray - Segment intersection

(define (intersect.ray-segment r s)
  (error "Not implemented"))

(define intersect.segment-ray intersect.ray-segment)

;;; Ray - Infinite line intersection

(define (intersect.ray-line r l)
  (error "Not implemented"))

(define intersect.line-ray intersect.ray-line)

;;; Infinite line - segment intersection

(define (intersect.line-segment line seg)
  (%accept (and (line? line) (segment? seg)))
  (aif int point? (intersect.line-line line (segment->line seg))
       (if (segment:point-collinear&on? seg int)
           int
           'projection-intersection)
       int))

(define intersect.segment-line intersect.line-segment)

;;; Infinite line - pseq intersections

(define (intersect.line-pseq line pseq)
  (%accept (and (line? line) (pseq? pseq)))
  (pair-fold-2 (lambda (tail acc)
                 (aif int point?
                      (intersect.line-segment line (make-segment (car tail)
                                                                 (cadr tail)))
                      (cons int acc)
                      acc))
               '()
               pseq))

(define intersect.pseq-line intersect.line-pseq)

;;; Infinite line - infinite line intersection

(define (intersect.line-line l1 l2)
  (%accept (and (line? l1) (line? l2)))
  (let ((l1a (line-a l1))
        (l1b (line-b l1))
        (l1c (line-c l1))
        (l2a (line-a l2))
        (l2b (line-b l2))
        (l2c (line-c l2)))
    (aif den zero? (- (* l1a l2b)
                      (* l2a l1b))
          (if (and (zero? (- (* l1a l2c)
                             (* l2a l1c)))
                   (zero? (- (* l1b l2c)
                             (* l2b l1c))))
              'line
            'no-intersection)
      (aif nom1 finite? (- (* l1b l2c)
                           (* l2b l1c))
        (aif nom2 finite? (- (* l2a l1c)
                             (* l1a l2c))
          (make-point (/ nom1 den)
                      (/ nom2 den))
          'no-intersection)
        'no-intersection))))


