#lang typed/racket

(provide (all-defined-out))

(struct number ((value : Number)) #:transparent)
(struct string ((value : String)) #:transparent)
(struct message ((name : String) (args : (Listof Value))) #:transparent)
(struct list ([values : (Listof Value)]) #:transparent)
(struct obj ([slots : (HashTable String (-> Value * Any))]) #:transparent)

(define-type Value (U number string message list))
(define-predicate Value? Value)

