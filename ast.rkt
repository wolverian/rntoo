#lang typed/racket

(require (prefix-in run/ "runtime.rkt"))

(provide (all-defined-out))

(struct message ((name : String) (args : (Listof Expr))) #:transparent)
(struct call ((receiver : Expr) (msg : message)) #:transparent)
(struct literal ((value : (U Number String))) #:transparent)
(struct op ((name : String) (to : Any) (value : Any)) #:transparent)

(define-type Expr (U message call literal op))
(define-predicate Expr? Expr)