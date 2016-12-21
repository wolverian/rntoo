#lang typed/racket

(provide (all-defined-out))

(define-type Fn (-> Any * Any))
(struct environment ((bindings : (HashTable String Fn))
                     (up : (U environment #f))))

(: lookup (-> (U environment #f) String Fn))
(define/match (lookup env name)
  ((#f name) (error "no name" name))
  (((environment bindings up) name) (hash-ref bindings name (λ () (lookup up name)))))

(: push (-> environment environment))
(define (push env)
  (environment (hash) env))

(: pop (-> environment environment))
(define (pop env)
  (or (environment-up env) env))