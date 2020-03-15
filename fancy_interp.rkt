#lang plai
(define-type Binding
  [binding (name symbol?) (named-expr WAE?)])

(define-type WAE
  [num (n number?)]
  [binop (op procedure?) (lhs WAE?) (rhs WAE?)]
  [with (lob (listof Binding?)) (body WAE?)]
  [id (name symbol?)])

;; drop: id  -> symbol
;; Takes id type and returns the symbol associated with it
(define (drop sub-id)
  (match sub-id
    [(id a) a]))

;; optable: procedure -> symbol
(define (optable sexp)
  (eval sexp))

;; parse: sexp -> WAE
;; Consumes an s-expression and generates the corresponding WAE
(define (parse sexp)
  (cond
    [(number? sexp)(num sexp)]
    [(symbol? sexp)(id sexp)]
    [(id? sexp)(drop sexp)]
    [(list? sexp)
     (case (first sexp)
       [(binop)(binop (optable (second sexp)) (parse(third sexp))
               (parse(fourth sexp)))])]))
    ;   [(with)(with(parse(first (second sexp))) (parse(second (second sexp)))
;                   (parse(third sexp)))])]))

;; calc: WAE -> number
;; Consumes a WAE representation of an expression and computes
;;    the corresponding result
(define (calc expr)
  (type-case WAE expr
    [num (n) n]
    [binop (op l r) (op (calc l) (calc r))]
    [id (v)(error 'calc "free identifier")]))

;; subst: WAE symbol WAE -> WAE
;; substitutes instances of the second argument for the third argument inside of
;; the first argument



;; Tests below:

