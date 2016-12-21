#lang racket

(require "parse.rkt"
         (prefix-in ast/ "ast.rkt")
         "bind.rkt")

(define (todo . args)
  (apply error "todo:" args))

(define (rneval s)
  (rneval* (parse-one (open-input-string s)) initial-env))

(define current-context (ast/literal "current-context"))

(define (rneval* exp env)
  (match exp
    [(ast/literal v)
     v]
    [(ast/message msg args)
     (rneval* (ast/call current-context (ast/message msg args)) env)]
    [(ast/call receiver (ast/message msg args))
     (apply (lookup env msg)
            (rneval* receiver env)
            (map (λ (a) (rneval* a env)) args))]))

(define initial-env
  (environment (hash "plus" +
                     "assign" todo)
               #f))

(module+ test
  (require rackunit)

  (check-equal? (rneval "42") 42)
  (check-equal? (rneval "42 + 22") 64)
  (check-exn exn:fail? (λ () (rneval "foo = 42"))))