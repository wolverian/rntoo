#lang racket

(require parser-tools/lex         
         parser-tools/cfg-parser
         (prefix-in : parser-tools/lex-sre)         
         (prefix-in ast/ "./ast.rkt"))

(provide parse parse-one)

(define-tokens non-terminals
  (<identifier>
   <number>
   <string>))

(define-empty-tokens terminals
  (<eof>
   <lparen>
   <rparen>
   <comma>))

(define-empty-tokens operators
  (<assign>))

(define-empty-tokens sugars
  (<plus>))

(define-lex-abbrevs
  (identifier (:: (:or alphabetic symbolic "-")
                  (:* (:or alphabetic numeric symbolic "-"))))
  (string (:* any-string)))

(define rnlex
  (lexer
   [(eof) (token-<eof>)]
   [whitespace (rnlex input-port)]
   [(:+ numeric) (token-<number> (string->number lexeme))]
   ["=" (token-<assign>)]
   ["(" (token-<lparen>)]
   [")" (token-<rparen>)]
   ["," (token-<comma>)]
   ["+" (token-<plus>)]
   [(:: "\"" string "\"") (token-<string> lexeme)]
   [identifier (token-<identifier> lexeme)]))

(define rnparse
  (cfg-parser
   (start start)
   (end <eof>)
   (tokens non-terminals terminals operators sugars)
   (error (λ (a name val) (error "wut" a name val)))
   (grammar
    (start [() #f]
           [(exp) $1])
    (exp [(<number>) (ast/literal $1)]
         [(<string>) (ast/literal $1)]
         [(message) $1]
         [(exp <assign> exp) (ast/message "assign"
                                          (list (ast/literal (string-append "\"" (ast/message-name $1) "\""))
                                                $3))]
         [(exp message) (ast/call $1 $2)])
    (message [(<identifier> <lparen> arglist <rparen>) (ast/message $1 (reverse $3))]
             [(<plus> exp) (ast/message "plus" (list $2))]
             [(<identifier>) (ast/message $1 null)])
    (arglist [() null]
             [(exp) (list $1)]
             [(arglist <comma> exp) (cons $3 $1)]))))

(define (parse s)
  (let ([result (parse-one s)])
    (when result
      (printf "~a\n" result)
      (parse s))))

(define (parse-one s)
  (rnparse (λ () (rnlex s))))
