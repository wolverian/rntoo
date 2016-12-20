#lang racket

(require rackunit
         "parse.rkt"
         "ast.rkt")

(define (parses in expected)
  (check-equal? (parse-one (open-input-string in)) expected))

(parses "42" (literal 42))
(parses "\"foo\"" (literal "\"foo\""))
(parses "foo" (message "foo" null))
(parses "42 foo" (call (literal 42) (message "foo" null)))
(check-equal? (parse-one (open-input-string "foo = method(x, x)"))
              (parse-one (open-input-string "assign(\"foo\", method(x, x))")))