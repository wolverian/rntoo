#lang typed/racket

(provide (all-defined-out))

(struct number ((value : Number)) #:transparent)
(struct string ((value : String)) #:transparent)
(struct message ((name : String) (args : (Listof Value))) #:transparent)

(define-type Value (U number string message))
(define-predicate Value? Value)

