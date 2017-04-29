#lang racket

(require threading
         (prefix-in ast/ "ast.rkt"))

; primitive types

(struct obj (type slots proto) #:transparent)
(struct native-fun (fun) #:transparent)

(define (slot name value)
  (list name value))

(define slot-name first)
(define slot-value second)

(define/contract (slot-lookup target name)
  (-> obj? string? any)
  (let ([immediate (assoc name (obj-slots target))])
    (if (list? immediate)
        (second immediate)
        (slot-lookup (obj-proto target) name))))

; object types

(define (message name args)
  (obj "message"
       (list (slot "name" name)
             (slot "args" args))
       initial))

(define (call target message)
  (obj "call"
       (list (slot "target" target)
             (slot "message" message))
       initial))

(define (rn-list values)
  (obj "list" (list (slot "values" values)) initial))

(define (rn-string s)
  (obj "string" (list (slot "value" s)) initial))

(define (primitive-own-slot-names self)
  (~>> self
       obj-slots
       (map slot-name)
       (map rn-string)
       rn-list))

(define (rn-append a b)
  (rn-list (append (slot-lookup a "values") (slot-lookup b "values"))))

(define (primitive-all-slot-names self)
  (rn-append (primitive-own-slot-names self)
             (if (obj? (obj-proto self))
                 (primitive-own-slot-names (obj-proto self))
                 (rn-list null))))

(define (object)
  (obj "object"
       (list (slot "ownSlotNames" (native-fun primitive-own-slot-names))
             (slot "allSlotNames" (native-fun primitive-all-slot-names)))
       null))

(define (block scope code)
  (obj "block"
       (list (slot "scope" scope)
             (slot "code" code))
       initial))

; it all begins here

(define initial (object))

; do a call!

(define (do-call call)
  (let* ([target (slot-lookup call "target")]
         [message (slot-lookup call "message")]
         [name (slot-lookup message "name")]
         [real-name (slot-lookup name "value")]
         [code (slot-lookup target real-name)])
    (if (native-fun? code)
        ((native-fun-fun code) target)
        (error "unimplemented"))))

; evaluate ast

(define/match (rn-eval expr)
  [((ast/literal value)) (rn-string value)]
  [((ast/send receiver message)) (call (rn-eval receiver)
                                       (rn-eval message))]
  [((ast/message name args)) (message (rn-string name)
                                      (map rn-eval args))])

(module+ test
  (require rackunit)
  
  (check-equal? (rn-eval (ast/literal "foo"))
                (rn-string "foo"))
  (check-equal? (rn-eval (ast/message "foo" null))
                (message (rn-string "foo") null))
  (let ([code (rn-eval (ast/send (ast/literal "foo") (ast/message "ownSlotNames" null)))]
        [expected (call (rn-string "foo") (message (rn-string "ownSlotNames") null))])
    (check-equal? code expected)
    (check-equal? (do-call code) (rn-list (list (rn-string "value"))))))