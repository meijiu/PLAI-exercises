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
    [(list? sexp)
     (case (first sexp)
       [(binop)(binop (optable (second sexp)) (parse(third sexp))
               (parse(fourth sexp)))]
       [(with)(with (bind (second sexp))
                   (parse(third sexp)))])]))

;; Consumes a list of name and named expression pairs and returns a list of Bindings
(define (bind sexp)
  (map (lambda (pair)
       (binding (first pair) (parse (second pair)) )) sexp))

;; calc: WAE -> number
;; Consumes a WAE representation of an expression and computes
;;    the corresponding result
(define (calc expr)
  (type-case WAE expr
   [num (n) n]
   [binop (op l r) (op (calc l) (calc r))]
   [with (lob body)
         (let ([b (first lob)])
         (if (= (length lob) 1)
         (calc (subst body (binding-name b) (binding-named-expr b)))
         (calc (with (rest lob)
            (subst body (binding-name b) (binding-named-expr b))))))] ;if len(lob) > 1
   [id (v)(error 'calc "free identifier")]))

;; subst: WAE symbol WAE -> WAE
;; substitutes instances of the second argument for the third argument inside of
;; the first argument

(define (subst expr sub-id val)
  (type-case WAE expr
    [num (n) expr]
    [binop (op l r)(binop op (subst l sub-id val)
                  (subst r sub-id val))]
    [with (bound body)
          (let ([bound-id (binding-name (first bound))]
                [named-expr (binding-named-expr (first bound))])
          (if(symbol=? (drop bound-id) sub-id) ; must pass symbol type into if statement
             (with (binding bound-id (subst named-expr sub-id val))
                   body)
             (with (binding bound-id (subst named-expr sub-id val))
                   (subst body sub-id val))))]
    [id (v)(if (symbol=? v sub-id) val expr)]))

;; Tests below: