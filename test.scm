;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname test) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define (odds n)
  (if (= n 0)
      '()
      (cons (- (* n 2) 1) (odds (- n 1)))))

(define x '(1 2 3 4 5))

(define size 2)

(define PI 3.14159)

(define radius 10)

(define circum (* 2 PI radius))

(define (square x) (* x x))

(define (sum-of-square x y)
  (+ (square x) (square y)))

(define (f a) 
  (sum-of-square (+ a 1) (+ 2 a)))


(define (myabs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(define (myabs1 x)
  (cond ((< x 0) (- x))
        (else x)))

(define (myabs2 x)
  (if (< x 0)
      (- x)
      x))

(define (max2 x y z)
  (cond ((and (< x y) (< x z)) (+ y z))
        ((and (< y x) (< y z)) (+ x z))
        (else (+ x y))))

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(define (p x) (p x))
(define (test x y)
  (if (= x 0)
      0
      y))


(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (mysqrt x)
  (sqrt-iter 1.0 x))
(define (new-if pred then-clause else-clause)
  (cond (pred then-clause)
        (else else-clause)))

(define (new-sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (new-sqrt-iter (improve guess x)
                         x)))

(define (new-good-enough? guess x)
  (< (abs (- guess (improve guess x))) 0.0000001))
(define (my-sqrt-iter guess x)
  (if (new-good-enough? guess x)
      guess
      (my-sqrt-iter (improve guess x)
                    x)))



(define (triple-good-enough? guess x)
  (< (abs (- guess (triple-improve guess x))) 0.01))
(define (triple-improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess))
     3))
(define (my-triple-iter1 guess x)
  (if (triple-good-enough? guess x)
      guess
      (my-triple-iter1 (triple-improve guess x) x)))
      
