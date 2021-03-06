#lang slideshow
(circle 10)
(rectangle 10 20)
(hc-append (circle 10) (rectangle 10 20))
(vl-append (circle 10) (rectangle 10 20))
(define (square n)
  ; A semi-colon starts a line comment.
  ; The expression below is the function body.
  (filled-rectangle n n))
(define (four p)
  (define two-p (hc-append p p))
  (vc-append two-p two-p))
(define (checker p1 p2)
  (let ([p12 (hc-append p1 p2)]
        [p21 (hc-append p2 p1)])
    (vc-append p12 p21)))
(square 10)
(define (checkerboard p)
  (let* ([rp (colorize p "red")]
         [bp (colorize p "black")]
         [c (checker rp bp)]
         [c4 (four c)])
    (four c4)))
(checkerboard (square 10))
(checker (colorize (square 10) "red")
           (colorize (square 10) "black"))
(define (series mk)
  (hc-append 4 (mk 5) (mk 10) (mk 20)))
(series circle)
(series square)
(series (lambda (size) (checkerboard (square size))))
(define series1
  (lambda (mk)
    (hc-append 4 (mk 5) (mk 10) (mk 20))))
(series1 square)
(series (lambda (size) (checkerboard (circle size))))

;
(define (rgb-series mk)
  (vc-append
   (series (lambda (sz) (colorize (mk sz) "red")))
   (series (lambda (sz) (colorize (mk sz) "green")))
   (series (lambda (sz) (colorize (mk sz) "blue")))))

(rgb-series circle)
(rgb-series square)


(define (rgb-maker mk)
  (lambda (sz)
    (vc-append (colorize (mk sz) "red")
               (colorize (mk sz) "green")
               (colorize (mk sz) "blue"))))

(series (rgb-maker circle))


(list "red" "green" "blue")
(list (circle 10) (square 10))

(define (rainbow p)
  (map (lambda (color)
         (colorize p color))
       (list "red" "orange" "yellow" "green" "blue" "purple")))

(rainbow (square 5))
(apply vc-append (rainbow (square 5)))


(require slideshow/flash)
(filled-flash 40 30)

(require (planet schematics/random:1:0/random))
(random-gaussian)
(require slideshow/code)
(code (circle 10))