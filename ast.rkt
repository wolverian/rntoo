#lang typed/racket

(require (prefix-in run/ "runtime.rkt"))

(provide (all-defined-out))

(struct message ((name : String) (args : (Listof Expr))) #:transparent)
(struct call ((receiver : Expr) (msg : message)) #:transparent)
(struct literal ((value : (U Number String))) #:transparent)

(define-type Expr (U message call literal))
(define-predicate Expr? Expr)