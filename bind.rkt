#lang typed/racket

(require (prefix-in run/ "runtime.rkt"))

(provide (all-defined-out))

(define-type Fn (-> run/Value * run/Value))
(struct table ((bindings : (HashTable String Fn))
               (up : (U table #f))))

(: lookup (-> (U table #f) String Fn))
(define/match (lookup env name)
  [(#f name) (error "no name" name)]
  [((table bindings up) name) (hash-ref bindings name (λ () (lookup up name)))])

(: push (-> table table))
(define (push env)
  (table (hash) env))

(: pop (-> table table))
(define (pop env)
  (or (table-up env) env))