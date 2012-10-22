(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

;((frame-coord-map a-frame) (make-vect 0 0))

;2.46
(define (make-vect x1 y1)
  (cons x1 y1))
(define (xcor-vect v)
  (car v))
(define (ycor-vect v)
  (cdr v))
(define (add-vect v1 v2)
  (let ((x1 (xcor-vect v1))
        (y1 (ycor-vect v1))
        (x2 (xcor-vect v2))
        (y2 (ycor-vect v2)))
    (make-vect
     (+ x1 x2)
     (+ y1 y2))))
(define (sub-vect v1 v2)
  (let ((x2 (xcor-vect v2))
        (y2 (ycor-vect v2)))
    (add-vect v2 
              (make-vect
               (- x2)
               (- y2)))))

(define (scale-vect v s)
  (let ((x (xcor-vect v))
        (y (ycor-vect v)))
    (make-vect 
     (* x s)
     (* y s))))

;2.47
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame f)
  (car f))
(define (edge1-frame f)
  (car (cdr f)))
(define (edge2-frame f)
  (car (cdr (cdr f))))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

;2.48
(define (make-segment v-start v-end)
  (cons v-start v-end))
(define (start-segment seg)
  (car seg))
(define (end-segment seg)
  (cdr seg))

;2.49
(define outline-segments
  (list
    (make-segment
      (make-vect 0.0 0.0)
      (make-vect 0.0 1.0))
    (make-segment
      (make-vect 0.0 0.0)
      (make-vect 1.0 0.0))
    (make-segment
      (make-vect 1.0 0.0)
      (make-vect 1.0 1.0))
    (make-segment
      (make-vect 0.0 1.0)
      (make-vect 1.0 1.0))))
(define outline-painter (segments->painter outline-segments))
(define x-segments
  (list
    (make-segment
      (make-vect 1.0 0.0)
      (make-vect 0.0 1.0))
    (make-segment
      (make-vect 0.0 0.0)
      (make-vect 1.0 1.0))))
(define x-painter (segments->painter x-segments))
(define diamond-segments
  (list
    (make-segment
      (make-vect 0.5 0.0)
      (make-vect 1.0 0.5))
    (make-segment
      (make-vect 1.0 0.5)
      (make-vect 0.5 1.0))
    (make-segment
      (make-vect 0.0 0.5)
      (make-vect 0.5 0.0))
    (make-segment
      (make-vect 0.0 0.5)
      (make-vect 0.5 1.0))))
(define diamond-painter (segments->painter diamond-segments))

