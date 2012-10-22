(define (square x) (* x x))
;(display (map square (list 1 2 3 4 5)))
(define (my-filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

;(display (my-filter odd? (list 1 2 3 4 5)))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
;(accumulate + 0 (list 1 2 3 4 5))
;(accumulate * 1 (list 1 2 3 4 5))
;(display (accumulate cons null (list 1 2 3 4 5)))

(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))

;(display (enumerate-interval 2 7))
(define (enumerate-tree tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))
;(display (enumerate-tree (list 1 (list 2 (list 3 4)) 5)))
(define (sum-odd-squares tree)
  (accumulate +
              0
              (map square
                   (filter odd?
                           (enumerate-tree tree)))))
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+
               (fib (- n 1))
               (fib (- n 2))))))
(define (even-fibs n)
  (accumulate cons
              nil
              (filter even?
                      (map fib
                           (enumerate-interval 0 n)))))

(define (list-fib-squares n)
  (accumulate cons
              null
              (map square
                   (map fib
                        (enumerate-interval 0 n)))))

;(display (list-fib-squares 10))
(define (product-of-squares-of-odd-elements sequence)
  (accumulate *
              1
              (map square
                   (filter odd? sequence))))
;(display (product-of-squares-of-odd-elements (list 1 2 3 4 5)))
(define (salary-of-highest-paid-programmer records)
  (accumulate max
              0
              (map salary
                   (filter programmer? records))))

;2.33
(define (my-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) null sequence))
(define (my-append seq1 seq2)
  (accumulate cons seq2 seq1))
(define (my-length sequence)
  (accumulate (lambda (x y) (+ 1 y))  0 sequence))

;2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) 
                (+ (* higher-terms x) this-coeff))
              0
              coefficient-sequence))

;(display (horner-eval 2 (list 1 3 0 5 0 1)))

;2.35
(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (node)
                     (if (pair? node) (count-leaves node) 1))
                   t)))
;(display (count-leaves (list (list 1 2 3) (list (list 1 2) (list 2)) 2 3 (list 1 2))))
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))
(define listoflist (list (list 1 2 3)
                         (list 4 5 6)
                         (list 7 8 9)
                         (list 10 11 12)))
;(display (accumulate-n + 0 listoflist))

;2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))
(define l1 (list 1 2 3))
;(dot-product l1 l1)
(define (matrix-*-vector m v)
  (map (lambda (row) 
         (dot-product v row)) m))
(define (transpose m)
  (accumulate-n cons null m))

(define (matrix-*-matrix m n)
  (let ((t-n (transpose n)))
    (map (lambda (r)
           (matrix-*-vector t-n r)) m)))
; test
(define matrix (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))
(define matrix (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))
(define identity-vector (list 1 1 1 1))
(define double-vector (list 2 2 2 2))

;(display (matrix-*-vector matrix identity-vector))
;(display (matrix-*-vector matrix double-vector))
;(transpose matrix)
;(display (matrix-*-matrix matrix (transpose matrix)))

;2.38
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))
(define (fold-right op initial sequence)
  (accumulate op initial sequence))
;(display (fold-right / 1 (list 1 2 3)))
;(display (fold-left / 1 (list 1 2 3)))
(newline)
;(display (fold-right list null (list 1 2 3)))

(display (fold-left list null (list 1 2 3)))
(newline)
(define (my-reverse sequence)
  (fold-right (lambda (x y) 
                (append y
                        (list x)))
              null 
              sequence))
(display (my-reverse (list 1 2 3)))
(newline)
(define (my-reverse sequence)
  (fold-left (lambda (x y) 
               (cons y x)) 
             null 
             sequence))
;(display (my-reverse (list 1 2 3)))

;
;(display (accumulate append
;            null
;            (map (lambda (i)
;                   (map (lambda (j) (list i j))
;                        (enumerate-interval 1 (- i 1))))
;                 (enumerate-interval 1 10))))
(newline)
;two dimension map 
(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))
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
;(display (prime-sum-pairs 10))

(define (permutations s)
  (if (null? s)                    ; empty set?
      (list null)                   ; sequence containing empty set
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (my-remove x s))))
               s)))
(define (my-remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))
;(display (permutations (list 1 2 3)))

;2.40
(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list j i))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))
;(display (unique-pairs 10))
(newline)
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))
;(display (prime-sum-pairs 5))

;2.42
; Write a procedure to find all ordered triples of distinct positive integers
; i, j, and k less than or equal to a given integer n that sum to a given
; integer s.
(define (ordered-triples n s)
  (filter (lambda (triple) (= (fold-left + 0 triple) s))
          (flatmap (lambda (i)
                     (flatmap (lambda (j)
                                (map (lambda (k) (list i j k))
                                     (enumerate-interval 1 j)))
                              (enumerate-interval 1 i)))
                   (enumerate-interval 1 n))))

(display (ordered-triples 10 10))


;-- 2.42
; Env:
(define nil '())
(define (enumerate-interval a b)
  (if (> a b)
    nil
    (append (list a) (enumerate-interval (+ a 1) b))))
;(define (filter predicate sequence)
 ; (cond ((null? sequence) nil)
  ;      ((predicate (car sequence))
   ;      (cons (car sequence)
    ;           (filter predicate (cdr sequence))))
     ;   (else (filter predicate (cdr sequence)))))
(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))
; Helper functions:

(define (adjoin-position new-row k rest-of-queens)
  (append (list (cons k new-row)) rest-of-queens))
(define (same-line queen-a queen-b) ; Coordinates are (x,y) pairs. If the y is
  ; the same for both queens, they are on the
  ; same line.
  (= (cdr queen-a) (cdr queen-b)))
(define (same-diagonal queen-a queen-b) ; Two queens are on the same diagonal
  ; if their horizontal distance is equal
  ; to their vertical distance
  (= (- (car queen-a) (car queen-b))
     (abs (- (cdr queen-a) (cdr queen-b)))))
(define (safe? k positions)
  (define (safe-iter queen-pos other-queens)
    (if (null? other-queens)
      #t
      (let ((qtt (car other-queens))) ;qtt = queen to test
        (cond ((same-line queen-pos qtt) #f) ; Same line
              ((same-diagonal queen-pos qtt) #f) ; Diagonal
              (else (safe-iter queen-pos (cdr other-queens)))))))
  (safe-iter (car positions) (cdr positions)))

(define empty-board '())
; Skeleton of the function to implement:
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
      (list empty-board)
      (filter
        (lambda (positions) (safe? k positions))
        (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))
(newline)
;(display (queens 8))

