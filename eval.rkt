#lang racket

(require (prefix-in parse/ "parse.rkt")
         (prefix-in ast/ "ast.rkt")
         (prefix-in run/ "runtime.rkt")
         "bind.rkt")

(define (todo . args)
  (apply error "todo:" args))

(define (rneval s)
  (rneval* (parse/one (open-input-string s)) initial-env))

(define current-context (ast/literal "current-context"))

(define/contract (rneval* exp env)
  (-> ast/Expr? environment? run/Value?)
  (match exp
    [(ast/literal v)
     (run/number v)]
    [(ast/message msg args)
     (rneval* (ast/send current-context (ast/message msg args)) env)]
    [(ast/send receiver (ast/message msg args))
     (apply (lookup env msg)
            (rneval* receiver env)
            (map (λ (a) (rneval* a env)) args))]))

(define (builtin-+ . args)
  (run/number (apply + (map run/number-value args))))

(define initial-env
  (environment (hash "plus" builtin-+
                     "assign" todo)
               #f))

(module+ test
  (require rackunit)

  (check-equal? (rneval "42") (run/number 42))
  (check-equal? (rneval "42 + 22") (run/number 64))
  (check-exn exn:fail? (λ () (rneval "foo = 42"))))