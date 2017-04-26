#lang typed/racket

(require (prefix-in ast/ "ast.rkt")
         (prefix-in run/ "runtime.rkt")
         (prefix-in bind/ "bind.rkt")
         threading)

(require/typed (prefix-in parse/ "parse.rkt")
               [parse/one (-> Input-Port ast/Expr)])

(: todo (-> run/Value * run/Value))
(define (todo . args)
  (apply error "todo:" args))

(: rneval (-> String run/Value))
(define (rneval s)
  (~>> s
       open-input-string
       parse/one
       (rneval* initial-env)))

(define current-context (ast/literal "current-context"))

(: rneval* (-> bind/table ast/Expr run/Value))
(define (rneval* env exp)
  (match exp
    [(ast/literal v)
     (match v
       [(? number? v) (run/number v)]
       [(? string? v) (run/string v)])]       
    [(? ast/message? msg)
     (~>> (ast/send current-context msg)
          (rneval* env))]
    [(ast/send receiver (ast/message msg args))
     (let ([fun (bind/lookup env msg)]
           [rec (rneval* env receiver)]
           [args (map (curry rneval* env) args)])
       (apply fun rec args))]))

(: builtin-+ (-> run/number * run/number))
(define (builtin-+ . arguments)
  (~>> arguments
       (map run/number-value)
       (apply +)
       run/number))

(: initial-env bind/table)
(define initial-env
  (bind/table (hash "plus" (cast builtin-+ bind/Fn)
                    "assign" (ann todo bind/Fn)
                    "version" (match-lambda*
                                [(list-rest receiver args)
                                 (begin
                                   (display receiver)
                                   (run/string "0.0.1"))]))
              #f))

(module+ test
  (require typed/rackunit)

  (check-equal? (rneval "version") (run/string "0.0.1"))
  (check-equal? (rneval "42") (run/number 42))
  (check-equal? (rneval "42 + 22") (run/number 64))
  (check-exn exn:fail? (λ () (rneval "foo = 42"))))