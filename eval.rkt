#lang typed/racket

(require (prefix-in ast/ "ast.rkt")
         (prefix-in run/ "runtime.rkt")
         (prefix-in bind/ "bind.rkt"))

(require/typed (prefix-in parse/ "parse.rkt")
               [parse/one (-> Input-Port ast/Expr)])

(: todo (-> run/Value * run/Value))
(define (todo . args)
  (apply error "todo:" args))

(: rneval (-> String run/Value))
(define (rneval s)
  (rneval* (parse/one (open-input-string s)) initial-env))

(define current-context (ast/literal "current-context"))

(define-predicate Number? Number)

(: rneval* (-> ast/Expr bind/table run/Value))
(define (rneval* exp env)
  (match exp
    [(ast/literal v)
     (if (Number? v)
         (run/number v)
         (run/string v))]
    [(ast/message msg args)
     (rneval* (ast/send current-context (ast/message msg args)) env)]
    [(ast/send receiver (ast/message msg args))
     (apply (bind/lookup env msg)
            (rneval* receiver env)
            (map (λ ([a : ast/Expr]) (rneval* a env)) args))]))

(: builtin-+ (-> run/number * run/number))
(define (builtin-+ . as)
  (run/number (apply + (map run/number-value as))))

(: initial-env bind/table)
(define initial-env
  (bind/table (hash "plus" (cast builtin-+ bind/Fn)
                    "assign" (ann todo bind/Fn))
              #f))

(module+ test
  (require typed/rackunit)

  (check-equal? (rneval "42") (run/number 42))
  (check-equal? (rneval "42 + 22") (run/number 64))
  (check-exn exn:fail? (λ () (rneval "foo = 42"))))