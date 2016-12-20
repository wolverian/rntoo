#lang racket

(require parser-tools/lex         
         parser-tools/cfg-parser
         (prefix-in : parser-tools/lex-sre)         
         (prefix-in ast/ "./ast.rkt"))

(define-tokens non-terminals
  (<identifier>
   <number>))

(define-empty-tokens terminals
  (<eof>
   <lparen>
   <rparen>
   <comma>
   <assign>))

(define-lex-abbrevs
  (identifier (:: (:or alphabetic symbolic "-")
                  (:* (:or alphabetic numeric symbolic "-")))))

(define rnlex
  (lexer
   [(eof) (token-<eof>)]
   [whitespace (rnlex input-port)]
   [(:+ numeric) (token-<number> (string->number lexeme))]
   ["=" (token-<assign>)]
   ["(" (token-<lparen>)]
   [")" (token-<rparen>)]
   ["," (token-<comma>)]
   [identifier (token-<identifier> lexeme)]))

(define rnparse
  (cfg-parser
   (start start)
   (end <eof>)
   (tokens non-terminals terminals)
   (error (λ (a name val) (error "wut" a name val)))
   (grammar
    (start [() #f]
           [(exp) $1])
    (exp [(<identifier>) (ast/identifier $1)]
         [(<number>) (ast/literal $1)]
         [(message) $1]
         [(exp <assign> exp) (ast/assign $1 $3)]
         [(exp message) (ast/call $1 $2)])
    (message [(<identifier> <lparen> arglist <rparen>) (ast/message $1 (reverse $3))])
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