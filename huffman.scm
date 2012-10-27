#lang racket
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))

(define l1 '((A 4) (B 2) (C 1) (D 1)) )
(display (make-leaf-set l1))

;2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(newline)
(display (decode sample-message sample-tree))

;2.68
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol s tree)
  (cond ((null? tree) null)
        ((leaf? tree) null)
        (else (let ((left (left-branch tree))
                    (right (right-branch tree)))
                (let ((ls (symbols left))
                      (rs (symbols right)))
                  (cond ((element-of-set? s ls)
                         (cons 0 (encode-symbol s left)))
                        (else (cons 1 (encode-symbol s right)))))))))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((eq? x (car set)) true)
        (else (element-of-set? x (cdr set)))))
(define me '(A D A B B C A))
(newline)
(display sample-tree)
(newline)
(display (encode me sample-tree))


(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))


(define (successive-merge ns)
  (if (= (length ns) 1)
      (car ns)
      (successive-merge (adjoin-set 
                         (make-code-tree (car ns) (cadr ns))
                         (cddr ns)))))
(newline)
(define t1 '((A 4) (B 2) (C 1) (D 1)))

(define  rock-50s-tree 
  (generate-huffman-tree
    '((a 2) (boom 1) (get 2) (job 2)
      (na 16) (sha 3) (yip 9) (wah 1))))
(newline)
(print 
  (encode 
    '(get a job sha na na na na na na na na
      get a job sha na na na na na na na na
      wah yip yip yip yip yip yip yip yip yip
      sha boom)
    rock-50s-tree))

(newline)
(display (generate-huffman-tree t1))