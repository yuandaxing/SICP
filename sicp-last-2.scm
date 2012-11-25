#lang racket
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner
       (term a)
       (accumulate combiner null-value term (next a) next b))))


(define (accumulate-iter combiner result term a next b)
  (if (> a b)
      result
      (accumulate combiner (combiner
                            result
                            (term a))
                  term (next a) next b)))
(define (sum a b)
  (accumulate-iter
   + 
   0
   (lambda (x) x) 
   a 
   (lambda (x) (+ x 1))
   b))
(sum 1 100)

(define (accumulate-filter  filter combiner null-value term a next b)
  (if (> a b)
      null-value
      (if (filter a)
          (combiner (term a)
                    (accumulate-filter filter combiner null-value term (next a) next b))
          (accumulate-filter filter combiner null-value term (next a) next b))))
  
(define (GCD a b)
  (if (= b 0)
      a
      (GCD b (remainder a b))))
(define (sum-two-primes n)
  (define (twin-prime k)
    (= (GCD k n) 1))
  (accumulate-filter twin-prime + 0 identity 2 (lambda (x) (+ x 1)) n))

;3 7 9 11 13 17 19 = 30
(sum-two-primes 20)

(define (cont-frac n d k)
  (define (recursive-cont-frac n d i)
    (if (> i k)
        0
        (/ n (+ d (recursive-cont-frac n d (+ i 1))))))
  (recursive-cont-frac n d 1))
(cont-frac 1 1 20)

(define (cont-frac-generic f1 x1 f2 x2 k)
  (define (recursive f1 x1 f2 x2 i)
    (if (> i k)
        0
        (/ (f1 x1 i) (+ (f2 x2 i) (recursive f1 x1 f2 x2 (+ i 1))))))
  (recursive f1 x1 f2 x2 1))

(define (dfc-e k)
  (cont-frac-generic (lambda (x i) 1.0) 1 (lambda (x i) (if (not (= (remainder i 3) 2))
                                                            1
                                                            (/ (* (+ i 1) 2) 3)))
                     1 
                     k))

(dfc-e 10)

(define (tan x k)
  (cont-frac-generic (lambda (x i) (if (= i 1) x (- (* x x))))
                     x
                     (lambda (x i) (- (* i 2.0) 1))
                     1
                     k))

(tan (/ pi 4) 10000)


;;to demenstrate function closure

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define dx 0.000001)
(define (compose-n f n)
   (if (= n 1) 
       f
       (compose f (compose-n f (- n 1)))))

(define f1
  (lambda (x) (* x x)))
(define f4 (compose-n f1 4))
(f4 2)
(print "here")
(define f2
  (lambda (x) (* 2 x)))

(define (smooth f)
  (lambda (x)
    (/ (+ (f x) (f (+ x dx)) (f (- x dx))) 3)))     
(define (smooth-n f n)
  ((compose-n smooth n) f))
((compose (compose-n f1 2) f2) 10)
((compose f2 (compose-n f1 2)) 10)
((smooth-n f1 2) 0)

