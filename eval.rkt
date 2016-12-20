#lang racket

(require "parse.rkt"
         "ast.rkt")

(define (rneval s)
  (rneval* (parse-one (open-input-string s))))

(define (rneval* exp)
  (match exp
    [(literal v) v]
    [(call x (message "plus" (list y))) (+ (rneval* x) (rneval* y))]))

(module+ test
  (require rackunit)

  (check-equal? (rneval "42") 42)
  (check-equal? (rneval "42 + 22") 64))