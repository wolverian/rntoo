#lang racket

(provide (all-defined-out))

(struct identifier (name) #:transparent)
(struct message (name args) #:transparent)
(struct call (receiver message) #:transparent)
(struct literal (value) #:transparent)
(struct op (name to value) #:transparent)