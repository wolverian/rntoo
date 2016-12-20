#lang racket

(require rackunit
         "parse.rkt"
         "ast.rkt")

(define (parses in expected)
  (check-equal? (parse-one (open-input-string in)) expected))

(define (parses-equal a b)
  (check-equal? (parse-one (open-input-string a))
                (parse-one (open-input-string b))))

(parses "42" (literal 42))
(parses "\"foo\"" (literal "\"foo\""))
(parses "foo" (message "foo" null))
(parses "42 foo" (call (literal 42) (message "foo" null)))
(parses "42 + 22" (call (literal 42) (message "plus" (list (literal 22)))))

(parses-equal "foo = method(x, x)" "assign(\"foo\", method(x, x))")
(parses-equal "42 + foo" "42 plus(foo)")