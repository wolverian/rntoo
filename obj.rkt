#lang racket

(require threading)

; primitive types

(struct obj (slots proto) #:transparent)
(struct native-fun (fun) #:transparent)

(define (slot name value)
  (list name value))

(define slot-name first)
(define slot-value second)

(define (slot-lookup target name)
  (~>> target
       obj-slots
       (assoc name)
       second))

; object types

(define (message name args)
  (obj (list (slot "name" name)
             (slot "args" args))
       initial))

(define (call target message)
  (obj (list (slot "target" target)
             (slot "message" message))
       initial))

(define (native-list values)
  (obj (list (slot "values" values)) initial))

(define (native-string s)
  (obj (list (slot "value" s)) initial))

(define (object)
  (obj (list (slot "slotNames"
                   (native-fun
                    (λ (self) (~>> self
                                   obj-slots
                                   (map slot-name)
                                   (map native-string)
                                   native-list)))))
       null))

(define (block scope code)
  (obj (list (slot "scope" scope)
             (slot "code" code))
       initial))

; it all begins here

(define initial (object))

; do a call!

(define (do-call call)
  (let* ([target (slot-lookup call "target")]
         [message (slot-lookup call "message")]
         [name (slot-lookup message "name")]
         [code (slot-lookup target name)])
    (if (native-fun? code)
        ((native-fun-fun code) target)
        (error "unimplemented"))))

(module+ test
  (require typed/rackunit)
  (check-equal? (do-call (call initial (message "slotNames" null)))
                (native-list (list (native-string "slotNames")))))