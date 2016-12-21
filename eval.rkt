#lang racket

(require "parse.rkt"
         "ast.rkt")

(define (todo . args)
  (apply error "todo:" args))

(define (rneval s)
  (rneval* (parse-one (open-input-string s)) initial-env))

(define current-context (literal "current-context"))

(define (rneval* exp env)
  (match exp
    [(literal v)
     v]
    [(message msg args)
     (rneval* (call current-context (message msg args)) env)]
    [(call receiver (message msg args))
     (apply (lookup msg env)
            (rneval* receiver env)
            (map (λ (a) (rneval* a env)) args))]))

(define (lookup n env)
  (hash-ref env n))

(define initial-env
  (hash "plus" +
        "assign" todo))

(module+ test
  (require rackunit)

  (check-equal? (rneval "42") 42)
  (check-equal? (rneval "42 + 22") 64)
  (check-exn exn:fail? (λ () (rneval "foo = 42"))))