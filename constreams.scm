#lang racket
(define (sum-primes a b)
  (define (iter count accum)
    (cond ((> count b) accum)
          ((prime? count) (iter (+ count 1) (+ count accum)))
          (else (iter (+ count 1) accum))))
  (iter a 0))

(sum-primes 10000 1000000)

(define (sum-primes a b)
  (accumulate +
              0
              (filter prime? (enumerate-interval a b))))
 

(define (smallest-divisor n)
  (find-divisor n 2)) 
(define (find-divisor n start)
  (cond ((> (square start) n) n)
        ((divide? start n) start)
        (else (find-divisor n (+ start 1)))))
(define (divide? m n)
  (= (remainder n m) 0)) 
(define (prime? n)
  (= n (smallest-divisor n)))


(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence))))) 