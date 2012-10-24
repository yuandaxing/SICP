(define (Hanoi n)
  (- (expt 2 n) 1))
;(Hanoi 1)
(define (line-splite n)
  (+ (/ (* n (+ n 1)) 2) 1))

;(line-splite 0)
;(line-splite 2)