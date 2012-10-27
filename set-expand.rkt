#lang racket
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))
(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()    
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

;2.61
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) (cons x (cdr set)))
        ((> x (car set)) (cons (car set) (adjoin-set x (cdr set))))
        ((< x (car set)) (cons x set))))
(define set (list 1 2 3 4 5))
(display (adjoin-set 10 set))

;2.62
(define (union-set s1 s2)
  (if (or (null? s1) (null? s2))
      (append s1 s2)
      (let ((f1 (car s1))
            (f2 (car s2)))
        (cond ((< f1 f2) (cons f1 (union-set (cdr s1) s2)))
              ((= f1 f2) (cons f1 (union-set (cdr s1) (cdr s2))))
              ((> f1 f2) (cons f2 (union-set s1 (cdr s2))))))))

(define s2 (list 2 4 6 9))
(newline)
(display (union-set set s2))




