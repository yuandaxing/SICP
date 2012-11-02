(define (cons-stream  a b)
  (cons a (delay b)))
(define (stream-car stream)
  (car stream))
(define (stream-cdr stream)
  (force (cdr stream)))

(define (numerate-interval a b)
  (if (> a b)
      null
      (cons-stream a (numerate-interval (+ a 1) b))))

(define s1 (numerate-interval 1 100000))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))
(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))
(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))
(define (stream-null? s)
  (null? s))

;test
;(stream-ref s1 20)
;(stream-map (lambda (x) (* x x)) s1)

(define (stream-filter proc stream)
  (if (null? stream)
      null
      (if (proc (stream-car stream))
          (cons-stream (stream-car stream)
                       (stream-filter proc (stream-cdr stream)))
          (stream-filter proc (stream-cdr stream)))))

;3.50
(define (stream-map proc . argstreams)
  (if (null? (car argstreams))
      null
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map 
              proc
              (map stream-cdr argstreams)))))

(define s1 (numerate-interval 10 100))
(define s2 (numerate-interval 20 200))
(define s3 (numerate-interval 30 300))

(define ss (stream-map + s1 s2 s3))
;(stream-ref ss 1)

;3.51

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(stream-ref integers 10000)

(define (divisible? x y) (= (remainder x y) 0))
(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7)))
                 integers))
(stream-ref no-sevens 100)

(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))
(define fibs (fibgen 0 1))


(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible? x (stream-car stream))))
           (stream-cdr stream)))))
(define primes (sieve (integers-starting-from 2)))
(stream-ref primes 100)
