(define (cube x)
  (* x x x))

(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ 1 a) b))))
(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))
(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a b))) (pi-sum (+ a 4) b)))) 

(define (sum  term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))
(define (inc n) (+ n 1))
(define (sum-cubes1 a b)
  (sum cube a inc b))
(define (identity1 x) x)
(define (sum-integers1 a b)
  (sum identity1 a inc b))

(define (pi-sum1 a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

;1.29
(define (simpson a b f n)
  (define dx (/ (- a b) (* 1.0 n)))
  (define (term1 k)
    (define factor  
      (cond ((or (= k 0) (= k n)) 1)
            ((odd? k) 4)
            (else 2)))
    (* (f (+ a (* k dx))) factor))
   (* (sum term1 0 inc n) (/ dx 3)))

;1.30
(define (new-sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))
(define (new-simpson a b f n)
  (define dx (/ (- a b) (* 1.0 n)))
  (define (term1 k)
    (define factor  
      (cond ((or (= k 0) (= k n)) 1)
            ((odd? k) 4)
            (else 2)))
    (* (f (+ a (* k dx))) factor))
   (* (new-sum term1 0 inc n) (/ dx 3)))

(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))
(define (factorial1 n)
  (product identity 1 inc n))

(define (pi-term k)
  (if (odd? k)
      (/ (+ k 3) (+ k 2))
      (/ (+ k 2) (+ k 3))))
(define (com-pi n)
  (* 4 (product pi-term 0 inc n)))


;1.32
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) 
                (accumulate combiner null-value term (next a) next b))))
(define (accumulate-iter combiner term a next b result)
  (if (> a b)
      result
      (accumulate-iter combiner term (next a) next b (combiner (term a) result)))) 

(define (sum-a term a next b)
  (accumulate + 0 term a next b))
(define (product-a term a next b)
  (accumulate * 1 term a next b))
;test
(define (com-pi-a n)
  (* 4 (product-a pi-term 0 inc n)))
(sum-a identity 1 inc 10)


;1.33
(define (filter-accumulate filter combiner null-value term a next b)
  (cond ((> a b) null-value)
        ((filter a) (combiner (term a)
                              (filter-accumulate filter combiner null-value term (next a) next b)))
        (else (filter-accumulate filter combiner null-value term (next a) next b))))


(define (sum-primes a b)
  (filter-accumulate prime? + 0 identity a inc b))

(define (sum-two-primes n)
  (define (twin-prime k)
    (= (GCD k n) 1))
  (filter-accumulate twin-prime + 0 identity 2 inc n))

;copy
(define (GCD a b)
  (if (= b 0)
      a
      (GCD b (remainder a b))))
;(define (GCD a b)
  (if (= b 0)
      a
      (GCD b (remainder a b)))

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
(define (square x)
  (* x x))


;1.3.2
(define (pi-sum-l a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

(define (integral-l f a b dx)
  (* (sum f
          (+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b)
     dx))


(define (f x y)
  (define (f-helper a b)
    (+ (* x (square a))
       (* y b)
       (* a b)))
  (f-helper (+ 1 (* x y)) 
            (- 1 y)))


(define (f x y)
  ((lambda (a b)
     (+ (* x (square a))
        (* y b)
        (* a b)))
   (+ 1 (* x y))
   (- 1 y)))

(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

;1.34
(define (f g)
  (g 2))
(f square)
(f (lambda (z) (* z (+ z 1))))


;1.3.3
(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))
(define (close-enough? x y)
  (< (abs (- x y)) 0.0001))
(define (average x y)
  (/ (+ x y) 2.0))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))

(half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3))
                      1.0
                      2.0)

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
(fixed-point cos 1.0)
(fixed-point (lambda (y) (+ (sin y) (cos y)))
             1.0)

(define (sqrt5 x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))

;1.35
(define gold-splite
  (fixed-point (lambda (x) (+ (/ 1 x) 1)) 1.0))

;1.36
(define result
  (fixed-point (lambda (x) (/ (log 1000) (log x))) 10.0))

(define result1
  (fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 10.0))

;1.37
(define (cont-frac n d k)
  (define (frac i)
    (if (= i k)
        (/ n d)
        (/ n (+ d (frac (+ i 1))))))
  (frac 1))

(define (cont-frac-iter n d k)
  (define (iter i result)
    (cond ((= i 0) result)
          ((= i k) (iter (- i 1) (+ result (/ n d))))
          (else (iter (- i 1) (/ n (+ d result))))))
  (iter k 0))

;1.38



;1.3.4
(define (average-damp f)
  (lambda (x) (average x (f x))))

;((average-damp square) 10)

(define (sqrt11 x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define (deriv g)
  (define dx 0.00001)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))
;((deriv cube) 5)


(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt12 x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt13 x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-damp
                            1.0))
(define (sqrt14 x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
                            newton-transform
                            1.0))


;1.40
(define (cube1 a b c)
  (lambda(x) (+ (* x x x) (* a x x) (* b x) c)))
(define (find-root a b c)
  (newtons-method (cube1 a b c ) 1))

;1.41
(define (double g)
  (lambda(x) (g (g x))))
;(((double (double double)) inc) 5)
;What does (double double) do ? Applies f twice to its argument, 
;when f is a function that applies some function twice to its argument.
;Therefore, it applies the function it receives 4 times. 
;((double (double double)) applies the function it receives 4*4 = 16 times. Therefore:


;1.42
(define (compose1 f g)
  (lambda(x) (f (g x))))
((compose1 square inc) 6)

;1.43
(define (repeated f i)
  (if (= i 1) f
      (compose1 f (repeated f  (- i 1)))))
;test 
;((repeated square 2) 5)

(define dx 0.1)
(define (smooth f)
  (lambda(x) (/ (+ (f x) (f (- x dx)) (f (+ x dx))) 3)))
;((smooth square) 0)

(define (smooth-n f n)
  ((repeated smooth n) f))

;((smooth square) 0)
;((smooth (smooth square)) 0)
;((smooth-n square 2) 0)


(define (cube10 x)
  ((smooth-n cube 10) x))
;(cube10 0)

(define (square10 x)
  ((smooth-n square 10) x))
;(square10 0)
;((smooth(smooth square)) 0)

;(define (expn n)
 ; (lambda (x) (if (= 0 n) 1
                  (* x ((expn (- n 1)) x)))))
;(define (dempen-root x n)
 ; (fixed-point