
(define (smallest-divisor n)
  (find-divisor n 2)) 
(define (square x)
  (* x x))
(define (find-divisor n start)
  (cond ((> (square start) n) n)
        ((divide? start n) start)
        (else (find-divisor n (+ start 1)))))
(define (divide? m n)
  (= (remainder n m) 0)) 
(define (prime? n)
  (= n (smallest-divisor n)))

(define (sum-primes a b)
  (define (iter count accum)
    (cond ((> count b) accum)
          ((prime? count) (iter (+ count 1) (+ count accum)))
          (else (iter (+ count 1) accum)))) 
  (iter a 0)) 



;actually cons is delay/lazy computing
(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence))))) 
(define (sum-primes-2 a b)
  (accumulate +
              0
              (filter prime? (enumerate-interval a b))))
