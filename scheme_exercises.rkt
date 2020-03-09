#lang plai

;; sum-coins: num num num number -> number
;; computes the value of pennies, nickels, dimes,
;; and quarters
(define (sum-coins pennies nickels dimes quarters)
  (+ (* 5 nickels)(* 10 dimes) (* 25 quarters) pennies))

(test(sum-coins 1 2 3 4) 141)
(test(sum-coins 0 1 0 0) 5)

;; area-cylinder: num num -> num
;; computes surface area of cylinder given radius
;; and height.

(define (area-cylinder radius height)
  (+ (* 2 pi radius height) (* 2 pi (* radius radius))))

(test (area-cylinder 5 5) 314.1592)

;; area-pipe: number number number -> number
;; calculates the surface area of an open cylinder

(define (area-pipe r_i len t)
   (* (* 2 pi r_i) len t))


;; tax: num -> num
;; calculates the tax over the gross pay
;; tax should be 0 if gross_pay is less than or equal to 240
;; tax should be 15 percent if gross_pay is between 240 and 480
;; tax should be 28 percent if gross pay is above 480

(define (tax gross_pay)
  (cond
  [(<= gross_pay 240) 0]
  [(and (> gross_pay 240) (<= gross_pay 480))(* .15 gross_pay)]
  [(> gross_pay 480)(* .28 gross_pay)]))

(define (netpay hours)
  (let ([pay (* 12 hours)])
       (- pay (tax pay))))

;; what-kind: number number number -> symbol
;; computes the number of roots of quadratic expression
;; with coefficients a, b, and c.
(define (what-kind a b c)
  (cond
  [(= a 0) 'degenerate]
  [else (let ([x (quad-form a b c)])
        (if (= (set-count x) 2) 'two 'one))]))

;; quad-form: number number number -> set
;; computes the roots of the quadratic equation with coefficients
;; a, b, and c

(define (quad-form a b c)
   (let ([root1 (/ (+ (* -1 b) (sqrt (- (* b b) (* 4 a c)))) (* 2 a))]
        [root2 (/ (- (* -1 b) (sqrt (- (* b b) (* 4 a c)))) (* 2 a))])
        (set root1 root2)))

(test (what-kind 1 2 1) 'one)
(test (what-kind 0 1 1) 'degenerate)
(test (what-kind 1 0 -1) 'two)

  