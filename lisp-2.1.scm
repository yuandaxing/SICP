(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))
(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-half (make-rat 1 2))
;(print-rat one-half)
(define one-third (make-rat 1 3))
;(print-rat (add-rat one-half one-third))
;(print-rat (mul-rat one-half one-third))
;(print-rat (add-rat one-third one-third))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

;2.1
(define (make-rat n d)
  (let ((d1 (abs d))
        (n1 (abs n)))
    (let ((g (gcd d1 n1))
          (d2 (if (> (* n d) 0)
                  d1
                  (- d1))))
      (cons (/ n1 g) (/ d2 g)))))

;(define a (make-rat -1 -2))
;(print-rat a)          
;(print-rat (add-rat one-third one-third))
(define (make-rat n d)
  (cons n d))
(define (numer x)
  (let ((g (gcd (car x) (cdr x))))
    (/ (car x) g)))
(define (denom x)
  (let ((g (gcd (car x) (cdr x))))
    (/ (cdr x) g)))

;2.2
(define (make-segment x y)
  (cons x y))
(define (start-segment seg)
  (car seg))
(define (end-segment seg)
  (cdr seg))
(define (make-point x y)
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))
(define (mid-point seg)
  (let ((s (start-segment seg))
        (e (end-segment seg)))
    (make-point
     (/ (+ (x-point s) (x-point e)) 2)
     (/ (+ (y-point s) (y-point e)) 2))))

;(print-point (mid-point s1))


(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
(define (print-segment seg)
  (newline)
  (display "start:")
  (print-point (start-segment seg))
  (newline)
  (display "end:")
  (print-point (end-segment seg)))
(define x1 (make-point 0 0))
(define x2 (make-point 0 1))
(define s1 (make-segment x1 x2))

;(print-point x1)
;(print-segment s1)
;(print-point (mid-point s1))

;2.3
(define (square x) (* x x))
(define (seg-length seg)
  (let ((s (start-segment seg))
        (e (end-segment seg)))
    (let ((x1 (x-point s))
          (y1 (y-point s))
          (x2 (x-point e))
          (y2 (y-point e)))
      (sqrt (+
             (square (- x2 x1))
             (square (- y2 y1)))))))
(seg-length s1)

(define (make-rect length width)
  (cons length width))
(define (r-length r)
  (car r))
(define (r-width r)
  (cdr r))

(define (area-rect r)
  (let ((len (r-length r))
        (width (r-width r)))
    (* (seg-length len)
       (seg-length width))))
(define (cir-rect r)
  (let ((len (r-length r))
        (width (r-width r)))
    (* (+
        (seg-length len)
        (seg-length width))
       2)))

(define x2 (make-point 0 0))
(define y2 (make-point 2 0))
(define s2 (make-segment x2 y2))
(define r1 (make-rect s1 s2))

;(seg-length s2)
;(area-rect r1)
;(cir-rect r1)

(define (cons-d x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1 -- CONS" m))))
  dispatch)
(define (car-d z) (z 0))
(define (cdr-d z) (z 1))

;(define x1 (cons-d 3 1))
;(x1 1) 
;(x1 0)

(define (my-cons x y)
  (lambda (m) (m x y)))

(define (my-car z)
  (z (lambda (p q) p)))

(define (my-cdr z)
  (z (lambda (p q) q)))

;(define  x4 (my-cons 4 4))
;(my-car x4)


;2.5
(define (my-cons a b)
  (* (expt 2 a) (expt 3 b)))
(define (devide? n d)
  (if (= 0 (remainder n d)) #t
      #f))
(define (find-exp x e i)
  (if (devide? x e) (find-exp (/ x e) e (+ i 1))
      i))
(define (my-car x)
  (find-exp x 2 0))
(define (my-cdr x)
  (find-exp x 3 0))
;(my-car 20)

;; ex 2.6
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one
  (add-1 zero))
;==>
;(lambda (f) (lambda (x) (f ((zero f) x))))
;(lambda (f) (lambda (x) (f x)))

(define two
  (add-1 one))
;==>
;(lambda (f) (lambda (x) (f ((one f) x))))
;(lambda (f) (lambda (x) (f (f x))))


;2.1.4
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
(define (div-interval x y)
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))
(define (make-interval a b) (cons a b))
(define (upper-bound a) (max (car a) (cdr a)))
(define (lower-bound a) (min (car a) (cdr a)))
(define (sub-interval x y)
  (let ((new-i
         (make-interval (- (lower-bound x)) (- (upper-bound x)))))
    (add-interval x new-i)))

(define i1 (make-interval 1 4))
(define i2 (make-interval 3 4))
(define (print-i interval)
  (display (lower-bound interval))
  (display ",")
  (display (upper-bound interval))
  (newline))

;(upper-bound i1)
;(lower-bound i2)
;(print-i (add-interval i1 i2))
;(print-i (add-interval i1 i2))
;(print-i (mul-interval i1 i2))
;(print-i (div-interval i1 i2))

(define (div-interval-1 x y)
  (if (and (< (lower-bound y) 0)
           (> (upper-bound y) 0))
      (error "could not div")
      (mul-interval 
       x
       (make-interval (/ 1.0 (upper-bound y))
                      (/ 1.0 (lower-bound y))))))

(define i3 (make-interval -1 4))
;(print-i (div-interval-1 i1 i3))
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;2.12
(define (make-center-percent c p)
  (make-center-width c (/ (* c p) 100.0)))

(define (percent i)
  (let ((c (center i))
        (w (width i)))
    (* (/ w c) 100)))

(define i4 (make-center-percent 100 1))
;(print-i i4)
;(percent i4)

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1))) 
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))
(define i5 (make-center-percent 100 1))
(print-i (par1 i4 i5))
(print-i (par2 i4 i5))
