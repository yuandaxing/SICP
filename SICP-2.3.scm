(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (make-sum a1 a2) (list '+ a1 a2))
(define (make-product m1 m2) (list '* m1 m2))
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))
;(display (deriv '(* (* x y) (+ x 3)) 'x))

(define (make-sum-helper a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))
(define (=number? exp num)
  (and (number? exp) (= exp num)))
;(newline)
;(display (deriv '(* (* x y) (+ x 3)) 'x))

(define (exponentation? x)
  (and (pair? x) (eq? (car x) '**)))
(define (base x)
  (cadr x))
(define (exponent x)
  (caddr x))



(define (make-exponentation base n)
  (cond ((=number? n 0) 0)
        ((=number? n 1) base)
        ((and (number? base) (number? n))
         (expt base n))
        (else (list '** base n)))) 

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentation? exp)
         (make-product 
          (make-product
           (exponent exp)
           (make-exponentation
            (base exp)
            (if (number? (exponent exp))
                (- (exponent exp) 1)
                (list '- (exponent exp) 1))))
          (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

;(display (deriv (list '** (list '** 'x 2) 'k)  'x))

;2.57
(define (number-sum l)
  (define (ss x)
    (if (null? x)
        0
        (+ (car x) (ss (cdr x)))))
  (ss (map (lambda (x) (if (number? x) x 0)) l)))
(display (number-sum (list 'a 1 2 3 '+)))
(newline)
(define (accumulate l)
  (cond ((null? l) null)
        ((or (eq? (car l) '+) (number? (car l))) (accumulate (cdr l)))
        (else (cons (car l) (accumulate (cdr l))))))

(define (could-add? s)
  (or (number? s) (and (pair? s) (eq? (car s) '+))))

(display (accumulate (list 'a '+ 1 2 3 'b)))
(newline)
(define (make-sum s1 s2)
  (cond ((or (not (could-add? s1)) (not (could-add? s1)))
         (append (list '+) (list s1) (list s2)))
        (else 
         (let ((l1 (if (pair? s1) s1 (list s1)))
               (l2 (if (pair? s2) s2 (list s2))))
           (let ((sum (+ (number-sum l1) (number-sum l2)))
                 (li (append (accumulate l1) (accumulate l2))))
             (cond ((null? li) sum)
                   ((= sum 0) (append (list '+) li))
                   (else (append (list '+) (append (list sum) li)))))))))

;acutually the mutiply is the same
          
;(display (make-sum 1 2 3 4))
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (augend x)
  (let ((aug (cddr x)))
    (if (= (length aug) 1)
        (car aug)
        (append (list '+) aug))))
(display (deriv '(+ x (** x 2)  x  (* 2 x) (+ x x)) 'x))

(define (make-sum a b)
  (list a '+ b))
(define (make-product a b)
  (list a '* b))
(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))
(define (addend s)
  (car s))
(define (augend s)
  (caddr s))
(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))
(define (multiplier x)
  (car x))
(define (multiplicand s)
  (caddr s))

(newline)
(display (deriv '(x + (x * x)) 'x))

(define a 123)