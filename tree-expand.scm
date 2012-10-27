#lang racket
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set) 
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

(define n1 (make-tree 1 null null))
(define n2 (make-tree 5 null null))
(define n4 (make-tree 3 n1   n2))

(display n4)
(newline)
(display (adjoin-set 10 n4))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))
(newline)
(display (tree->list-1 n4))
(newline)
(display (tree->list-2 n4))


(define (list->tree elements)
  (car (partial-tree elements (length elements))))

;the result is tree left
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))


(define set (list 1 2 3 4 5))
;(display (adjoin-set 10 n4))
(define n7 (make-tree 11 null null))
(define n8 (make-tree 9 null n7))
;2.62
(define (union-set s1 s2)
  (if (or (null? s1) (null? s2))
      (append s1 s2)
      (let ((f1 (car s1))
            (f2 (car s2)))
        (cond ((< f1 f2) (cons f1 (union-set (cdr s1) s2)))
              ((= f1 f2) (cons f1 (union-set (cdr s1) (cdr s2))))
              ((> f1 f2) (cons f2 (union-set s1 (cdr s2))))))))

(define (union-tree t1 t2)
  (let ((l1 (tree->list-1 t1))
        (l2 (tree->list-1 t2)))
    (let ((u1 (union-set l1 l2)))
      (list->tree u1))))
(newline)
(display (union-tree n8 n4))

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
(define (join-tree t1 t2)
  (let ((l1 (tree->list-1 t1))
        (l2 (tree->list-1 t2)))
    (let ((i1 (intersection-set l1 l2)))
      (list->tree i1))))
(newline)
(display (join-tree n8 n4))


(define (key x) x)
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else (lookup given-key (cdr set-of-records)))))

;2.66
(define (lookup-binary key tree)
  (cond ((null? tree) false)
        ((eq? (entry tree) key) tree)
        ((< (entry tree) key) (lookup-binary key (right-branch tree)))
        (else (lookup-binary key (left-branch tree)))))

(newline)

;(display (lookup-binary 1 n4))



