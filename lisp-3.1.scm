(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
(withdraw 25)
(withdraw 25)
(withdraw 60)

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))


(define W1 (make-withdraw 100))
(define W2 (make-withdraw 100))
(W1 50)
(W2 70)
(W2 40)


(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

(define acc (make-account 100))
((acc 'withdraw) 50)
((acc 'withdraw) 60)

;3.1
(define (make-accumulator acc)
  (lambda (x)
    (begin (set! acc (+ acc x))
           acc)))
(define A (make-accumulator 5))
(A 10)
(A 10)

(define (make-monitored f)
  (define (helper f count)
    (define (call x)
      (begin (set! count (+ count 1)) 
             (f x)))
    (define (dispatch m)
      (cond ((eq? m 'how-many-calls?) count)
            (else (call m))))
    dispatch)
  (helper f 0))

(define s (make-monitored sqrt))
;(s 100)
;(s 100)
;(s 'how-many-calls?)

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))
(define (make-account balance . pwd)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (share pwd1 newpwd)
    (cond ((element-of-set? pwd1 pwd) 
           (begin (set! pwd (cons newpwd pwd))
                  (print "create share success")))
          (else 
           (print "create share failed"))))
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          ((eq? m 'share) share)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)
(define (make-joint acc pwd newpwd)
  ((acc 'share) pwd newpwd))
;(define c1 (make-account 100 'abc))
(define peter-acc (make-account 100 'open-sesame))
((peter-acc 'withdraw) 10)
(define paul-acc
  (make-joint peter-acc 'open-sesame 'rosebud))