#lang racket

(provide (all-defined-out))

(struct identifier (name) #:transparent)
(struct message (name args) #:transparent)
(struct call (receiver message) #:transparent)
(struct literal (value) #:transparent)
(struct assign (to value) #:transparent)