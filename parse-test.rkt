#lang racket

(require rackunit
         "parse.rkt"
         "ast.rkt")

(define (parses in expected)
  (check-equal? (parse-one (open-input-string in)) expected))

(parses "42" (literal 42))
(parses "foo" (identifier "foo"))
(parses "42 foo" (call (literal 42) (message "foo" null)))
(parses "foo = method(x, x +(2))" (op '=
                                      (identifier "foo")
                                      (message "method"
                                               (list
                                                (identifier "x")
                                                (call (identifier "x")
                                                      (message "+"
                                                               (list (literal 2))))))))