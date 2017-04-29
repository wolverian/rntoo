#lang racket

(require threading
         (prefix-in ast/ "ast.rkt")
         (prefix-in parse/ "parse.rkt"))

; primitive types

(define/contract (slot/c s)
  (-> any/c contract?)
  (slot? s))

(define/contract (native-type/c t)
  (-> any/c contract?)
  (native-type? t))

(define-struct/contract obj ([type string?] [slots (listof slot/c)] [proto any/c]) #:transparent)
(define-struct/contract slot ([name string?] [value (or/c native-type/c obj?)]) #:transparent)

(define/contract (slot-lookup target name)
  (-> obj? string? any)
  (let ([immediate (memf (λ (slot) (equal? (slot-name slot) name)) (obj-slots target))])
    (if (list? immediate)
        (slot-value (first immediate))
        (slot-lookup (obj-proto target) name))))

; object types

(define native-type "*native-type*")

(define (native-type? obj)
  (or (string? obj)
      (number? obj)
      (list? obj)
      (procedure? obj)
      (equal? (obj-proto obj) native-type)))

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

(define (native-fun fun)
  (obj "native-fun"
       (list (slot "fun" fun))
       native-type))

(define (rn-list values)
  (obj "list" (list (slot "values" values)) initial))

(define (native-string-up-case s)
  (rn-string (string-upcase (slot-lookup s "value"))))

(define (rn-string s)
  (obj "string" (list (slot "value" (string-trim s "\""))
                      (slot "upperCase" (native-fun native-string-up-case)))
       initial))

(define (rn-number n)
  (obj "number" (list (slot "value" n)) initial))

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
             (slot "allSlotNames" (native-fun primitive-all-slot-names))
             (slot "clone" (native-fun native-clone))
             (slot "list" (native-fun rn-list)))
       null))

(define (native-clone target)
  (obj (obj-type target) null target))

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
         [value (slot-lookup target real-name)])
    (match value
      [(? string?) value]
      [(obj "native-fun" _ _) ((slot-lookup value "fun") target)]
      [_ (error "unimplemented")])))

; evaluate ast

(define/match (rn-eval expr)
  [((ast/literal value)) (if (number? value) (rn-number value) (rn-string value))]
  [((ast/send receiver message)) (call (rn-eval receiver)
                                       (rn-eval message))]
  [((ast/message name args)) (message (rn-string name)
                                      (map rn-eval args))])

(define/match (rn-do-calls expr)
  [((obj "call" slots proto)) (do-call expr)]
  [((obj "message" slots proto)) (do-call (call initial expr))] ; lonely message implicitly calls lobby
  [(_) expr])

(define/contract (slots-equal? a b)
  (-> slot? slot? boolean?)
  (and (equal? (slot-name a)
               (slot-name b))
       (if (and (native-type? (slot-value a)) (native-type? (slot-value b)))
           (equal? (slot-value a) (slot-value b))
           (and (not (native-type? (slot-value a))) (not (native-type? (slot-value b)))
                (rn-equal? (slot-value a) (slot-value b))))))

(define/contract (rn-equal? a b)
  (-> obj? obj? boolean?)
  (let ([a-slots (sort (obj-slots a) string<? #:key slot-name)]
        [b-slots (sort (obj-slots b) string<? #:key slot-name)])
    (and (= (length a-slots) (length b-slots))
         (let* ([slots-agree (map slots-equal? a-slots b-slots)]
                [a-proto (obj-proto a)]
                [b-proto (obj-proto b)])
           (and slots-agree
                (or (and (null? a-proto) (null? b-proto))
                    (rn-equal? a-proto b-proto)))))))

(define/match (rn-pretty-print value)
  [((obj "string" _ _)) (~v (slot-lookup value "value"))]
  [((obj "list" _ _)) (string-append "list(" (apply ~a (map rn-pretty-print (slot-lookup value "values")) #:separator ", ") ")")]
  [((obj "call" _ _)) (string-append (rn-pretty-print (slot-lookup value "target")) " " (rn-pretty-print (slot-lookup value "message")))]
  [((obj "message" _ _)) (string-append (rn-pretty-print (slot-lookup value "name")) "(" (apply ~a (map rn-pretty-print (slot-lookup value "args")) #:separator ", ")  ")")])

(module+ test
  (require rackunit)

  (define-simple-check (rn-check? actual expected)
    (rn-equal? (rn-do-calls (rn-eval (parse/string actual)))
               expected))
  
  ; sanity check for the parser
  (check-equal? (parse/string "42 foo")
                (ast/send (ast/literal 42) (ast/message "foo" null)))

  (rn-check? "\"foo\" upperCase" (rn-string "FOO"))
  (rn-check? "\"foo\" ownSlotNames" (rn-list (map rn-string (list "upperCase" "value"))))
  (rn-check? "list(1, 2, 3)" (rn-list (map rn-number (list 1 2 3))))
  (let ([code (rn-eval (ast/send (ast/literal "foo") (ast/message "ownSlotNames" null)))]
        [expected (call (rn-string "foo") (message (rn-string "ownSlotNames") null))])
    (check-equal? code expected)
    (check-equal? (do-call code) (rn-list (list (rn-string "value") (rn-string "upperCase"))))))