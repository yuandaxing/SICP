(define (sqrt1 x)
  (define (good-enough? guess x)
    (< (abs (- (square guess ) x)) 0.001))
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))
  (sqrt-iter 1.0 x))

(define (square x)
  (* x x))
(define (average x y)
  (/ (+ x y) 2))

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(define (factorial1 n)
  (fact-iter 1 1 n))
(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

;1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))
(define (k n) (* 5 n n ))

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))
(define (fib2 n)
  (define (fib-iter a b count)
    (if (= count n)
        a
        (fib-iter b (+ a b) (+ count 1))))
  (fib-iter 0 1 0))


;
(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

;1.11
(define  (f11 n)
  (if (< n 3)
      n
      (+ (f11 (- n 1)) (* 2 (f11 (- n 2))) (* 3 (f11 (- n 3))))))

(define (f11-au n)
  (define (helper a b c count)
    (if (= count 2)
        a
        (helper (+ a (* 2 b) (* 3 c)) a b (- count 1))))
  (helper 2 1 0 n))

;1.12 
(define (pascal row col)
  (cond ((= col 1) 1)
        ((= row col) 1)
        (else (+ (pascal (- row 1) col) (pascal (- row 1) (- col 1))))))
;(define (print-p row)
 ; (if (= row 1) (print-row row)
  ;    (print

;1.14 space O(n), time, exponential(n)
;1.15

(define (cube x)
  (* x x x))

(define (p x)
 ; (format t "p ~f~%" x)
  (-  (* 3 x)
      (* 4 (cube x))))

(define (sine angle)
  (if (not (> (abs angle) 0.001))
    angle
    (p (sine (/ angle 3.0)))))
                   
(define (expt1 b n)
  (if (= n 0)
      1
      (* b (expt1 b (- n 1)))))

(define (expt2 b n)
  (expt-iter b n 1))
(define (expt-iter b n product)
  (if (= n 0)
      product
      (expt-iter b (- n 1) (* b product))))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))
(define (myeven? n)
  (= (reminder n 2) 0))

; 1.16
(define (fast-expt-iter b n a)
  (cond ((= n 0) a)
        ((even? n) (fast-expt-iter (square b) (/ n 2) a))
        (else (fast-expt-iter b (- n 1) (* b a)))))
(define (myfast-expt b n)
  (fast-expt-iter b n 1))

(define (mydouble b)
  (+ b b))
(define (mymul a b)
  (define (mul-iter a b t)
    (cond ((= b 0) t)
          ((even? b) (mul-iter (mydouble a) (/ b 2) t))
          (else (mul-iter a (- b 1) (+ t a)))))
  (mul-iter a b 0))

;1.19
(define (fib4 n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p q) (* q q))      ; compute p'
                   (+ (* 2 p q) (* q q))      ; compute q'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

;1.2.5
(define (GCD a b)
  (if (= b 0)
      a
      (GCD b (remainder a b))))

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
(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                   m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-primes? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-primes? n (- times 1)))
        (else false)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (find-min n)
  (if (prime? n) n
      (find-min (+ n 1))))

(define (full-fermat-test n)
  (define (fermat-test-1 st)
    (cond ((= st n) true)
          ((not (= (expmod st n n) st)) false)
          (else (fermat-test-1 (+ st 1)))))
  (fermat-test-1 2))

(define (expmod-aux base exponent m)
  (cond ((= exponent 0) 1)
        ((even? exponent)
          (define candidate (expmod-aux base (/ exponent 2) m))
          (define root (remainder (square candidate) m))
          (if (and (not (= candidate 1)) (not (= candidate (- m 1))) (= root 1))
                  0
                  root))
        (else
          (remainder  (* base (expmod-aux base (- exponent 1) m))
                m))))

; Returns T from numbers that are probably prime
(define (miller-rabin-test n)
  (define testnum (+ 1 (random (- n 1))))
  (= (expmod-aux testnum (- n 1) n) 1))
(define (my-miller n times)
  (cond ((= times 0) false)
        ((not (miller-rabin-test n)) false)
        (else (my-miller n (- times 1)))))
