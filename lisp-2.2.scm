(define one-through-four (list 1 2 3 4))
;one-through-four
(define my-list (cons <a1> (cons <a2> (cons  (cons <an> nil)))))
;(car one-through-four)
;(cdr one-through-four)
;(car (cdr one-through-four))
;(cons 10 one-through-four)
;(cons 5 one-through-four)
(define (my-list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))
(define squares (list 1 4 9 16 25))
;(list-ref squares 3)
(define (my-length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))
(define odds (list 1 3 5 7))
;(my-length odds)
(define (my-length items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))
(define (my-append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (my-append (cdr list1) list2))))


;2.17
(define (my-last-pair list1)
  (let ((len (length list1)))
    (list-ref list1 (- len 1))))

;(last-pair odds)
;(list-ref odds 3)
(my-last-pair odds)

;2.18
(define (my-reverse l)
  (define (re-iter l pre)
    (if (null? l) 
        pre
        (re-iter (cdr l) (cons (car l) pre))))
  (re-iter l null))
;(define l2 (my-reverse odds))
;(my-reverse (list 1 4 9 16 25))

;2.19
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))
(define (no-more? l)
  (null? l))
(define (except-first-denomination l)
  (cdr l))
(define (first-denomination l)
  (car l))

;(cc 100 us-coins)
;(cc 20 uk-coins)
(define (same-parity x . li)
  (let ((par (odd? x)))
    (define (helper l)
      (cond ((null? l) nil)
            ((or (and par (odd? (car l)))
                 (and (not par) (not (odd? (car l)))))
             (cons (car l) (helper (cdr l))))
            (else (helper (cdr l)))))
    (helper li)))

(define l5 (same-parity 2 2 3 4 5 6 7))
(list-ref l5 2)

(define (print-list l)
  (map (lambda (x) 
         (print x))
       l))
;(print-list l5)

(define (scale-list items factor)
  (if (null? items)
      nil
      (cons (* (car items) factor)
            (scale-list (cdr items) factor))))
;(scale-list (list 1 2 3 4 5) 10)
(define (my-map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))
;(map abs (list -10 2.5 -11.6 17))

(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))
(define l6 (map (lambda (x) (* x x))
     (list 1 2 3 4)))
(list-ref l6 3)

;2.21
(define (square-list items)
  (if (null? items)
      nil
      (cons (* (car items) (car items)) (square-list (cdr items)))))


(define (square-list items)
  (map (lambda (x) (* x x)) items))
(define sl (square-list (list 1 2 3 4)))
;(list-ref sl 3)

(define (my-for-each fun lis)
  (cond ((null? lis) #t)
        (else (fun (car lis))
              (my-for-each fun (cdr lis)))))

(define (show-list l) 
  (for-each (lambda (x) (newline) (display x)) l))

(define (count-leaves x)
  (cond ((null? x) 0)  
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))
(define x (cons (list 1 2) (list 3 4)))
;(length x)
;(count-leaves x)

(define (print-tree tree)
  (cond ((null? tree)  true)
        ((not (pair? tree))
         (display tree)
         (display " "))
        (else
         (display "(")
         (print-tree (car tree))
         (print-tree (cdr tree))
         (display ")"))
        ))
;(print-tree x)

(define (deep-reverse li)
  (cond ((null? li) li)
        (else (append (deep-reverse (cdr li))
                      (list (if (pair? (car li))
                              (deep-reverse (car li))
                              (car li)))))))

(define l6 (deep-reverse x))
;(show-list l6)

(define (fringe tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (list tree))
        (else (append (fringe (car tree))
                (fringe (cdr tree))))))

(define l5 (fringe x))
;(list-ref l5 0)
;(show-list l5)
;(display l5)

;-- 2.29
(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))

; a.
(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (car (cdr mobile)))
(define (branch-length branch) (car branch))
(define (branch-structure branch) (car (cdr branch)))


; b.
(define (structure-is-weight? struct)
  (not (pair? struct)))
(define (weight-of-branch b)
  (let ((struct (branch-structure b)))
    (if (structure-is-weight? struct)
        struct
        (weight-of-mobile struct))))
(define (weight-of-mobile mobile)
  (+ (weight-of-branch (left-branch mobile))
     (weight-of-branch (right-branch mobile))))

(define b00 (make-branch 0 0))
(define b01 (make-branch 0 0))
(define nill-tree (make-mobile b00 b01))

; Tests:
;(left-branch (make-mobile 2 3))
;(right-branch (make-mobile 2 3))
(branch-length (make-branch 4 5))
(branch-structure (make-branch 4 5))
(define b1 (make-branch 4 5))
(define b2 (make-branch 4 5))
(define m1  (make-mobile b1 b2))
;(weight-of-mobile m1)

;c
(define (branch-force b)
  (* (branch-length b)
     (weight-of-branch b)))
(define (mobile-balanced? m)
  (let ((lb (left-branch m))
        (rb (right-branch m)))
    (and
     (=
      (branch-force lb)
      (branch-force rb))
     (branch-balanced? lb)
     (branch-balanced? rb))))
(define (branch-balanced? b)
  (let ((struct (branch-structure b)))
    (or
     (structure-is-weight? struct)
     (mobile-balanced? struct))))

;(mobile-balanced? m1)

;I only need to change the accessors left-branch, right-branch, branch-len,
;branch-structure and the predicate structure-is-weight?. The rest of the 
;code builds on top of the abstraction created by these 
;functions and doesn’t need to be changed.
(define (scale-tree tree factor)
  (cond ((null? tree) null)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

(display (scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7))
            10))
(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))

;2.30
(define (scale-tree prog tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree prog sub-tree)
             (prog sub-tree)))
       tree))
(define (square-tree l)
  (scale-tree (lambda (x) (* x x)) l))
(newline)
;(display (square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7))
(define (subsets s)
  (if (null? s)
      (list null)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (set) (cons (car s) set))  rest)))))

;(display (subsets (list 1 2 3 4)))
(define (square x)
  (* x x))
(define (sum-odd-squares tree)
  (cond ((null? tree) 0)  
        ((not (pair? tree))
         (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))
;(display (sum-odd-squares (list 1 2 3 4 5 6)))

(define (fib  n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))
(define (even-fibs n)
  (define (next k)
    (if (> k n)
        null
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))
                    
(display (even-fibs 10))