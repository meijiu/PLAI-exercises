#lang slideshow

;; Worked through examples from Quick Racket Tutorial

(define r (rectangle 10 20))
(define c (circle 10))
(hc-append 20 c r c)

(define (square n)
  (filled-rectangle n n))

(define (checker p1 p2)
  (let ([p12 (vc-append p1 p2)])
    (hc-append p12 p12)))

(define (four p)
  (define two-p (hc-append p p))
  (vc-append two-p two-p))


;; let vs let* comparison. let* allows you to reference earlier bindings
;; in later ones

(define (four2 p)
  (let* ([two-p (hc-append p p)]
        [x (vc-append two-p two-p)]) ; can call two-p is let* is used
        x))

(define (checkerboard p)
  (let* ([rp (colorize p "red")]
        [bp (colorize p "black")]
        [c (checker rp bp)] ; can call rp and bp from earlier binding
        [c4 (four c)])
    (four c4)))



