#lang plai
(define-type Binding
  [binding (name symbol?) (named-expr WAE?)])

(define-type WAE
  [num (n number?)]
  [add (lhs WAE?)(rhs WAE?)]
  [sub (lhs WAE?)(rhs WAE?)]
  [with (name id?)(named-expr WAE?)(body WAE?)]
  [id (name symbol?)])

;; drop: id  -> symbol
;; Takes id type and returns the symbol associated with it
(define (drop sub-id)
  (match sub-id
    [(id a) a]))

;; parse: sexp -> WAE
;; Consumes an s-expression and generates the corresponding WAE
(define (parse sexp)
  (cond
    [(number? sexp)(num sexp)]
    [(symbol? sexp)(id sexp)]
    [(list? sexp)
     (case (first sexp)
       [(+)(add (parse(second sexp))
               (parse(third sexp)))]
       [(-)(sub (parse(second sexp))
               (parse(third sexp)))]
       [(with)(with (parse(first (second sexp))) (parse(second (second sexp)))
                   (parse(third sexp)))])]))

;; calc: WAE -> number
;; Consumes a WAE representation of an expression and computes
;;    the corresponding result
(define (calc expr)
  (type-case WAE expr
    [num (n) n]
    [add (l r)(+ (calc l)(calc r))]
    [sub (l r)(- (calc l)(calc r))]
    [with (bound-id named-expr bound-body)
          (calc(subst bound-body
                      (drop bound-id) ; must pass symbol type as second arg to subst
                      (num (calc named-expr))))]
    [id (v)(error 'calc "free identifier")]))

;; subst: WAE symbol WAE -> WAE
;; substitutes instances of the second argument for the third argument inside of
;; the first argument

(define (subst expr sub-id val)
  (type-case WAE expr
    [num (n) expr]
    [add (l r)(add(subst l sub-id val)
                  (subst r sub-id val))]
    [sub (l r)(sub(subst l sub-id val)
                  (subst r sub-id val))]
    [with (bound-id named-expr bound-body)
          (if(symbol=? (drop bound-id) sub-id) ; must pass symbol type into if statement
             (with bound-id
                   (subst named-expr sub-id val)
                   bound-body)
             (with bound-id
                   (subst named-expr sub-id val)
                   (subst bound-body sub-id val)))]
    [id (v)(if (symbol=? v sub-id) val expr)]))

;; Tests below:
(test(calc(parse '{with {x 5} {+ x x}})) 10)
(test(calc(parse '{with {x {+ 5 5}}{with {y {- x 3}} {+ y y}}})) 14)
(test(calc(parse '{with {x 5} {+ x {with {x 3} 10}}})) 15)
(test(calc(parse '{with {x 5}{+ x {with {x 3} x}}})) 8)
(test(calc(parse '{with {x 5}{with {y x} y}})) 5)
(test(calc(parse '{with {x 5}{with {x x} x}})) 5)
