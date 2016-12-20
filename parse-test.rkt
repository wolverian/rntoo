#lang racket

(require rackunit
         "parse.rkt"
         "ast.rkt")

(define (parses-as? in expected)
  (check-equal? (parse-one (open-input-string in)) expected))

(define (parses-equal? a b)
  (check-equal? (parse-one (open-input-string a))
                (parse-one (open-input-string b))))

(parses-as? "42" (literal 42))
(parses-as? "\"foo\"" (literal "\"foo\""))
(parses-as? "foo" (message "foo" null))
(parses-as? "42 foo" (call (literal 42) (message "foo" null)))
(parses-as? "42 + 22" (call (literal 42) (message "plus" (list (literal 22)))))

(parses-equal? "foo = method(x, x)" "assign(\"foo\", method(x, x))")
(parses-equal? "42 + foo" "42 plus(foo)")