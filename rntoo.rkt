#lang racket

(require parser-tools/lex
         parser-tools/lex-sre
         parser-tools/yacc
         (prefix-in ast/ "./ast.rkt"))

(define-tokens non-terminals
  (<identifier>
   <number>))

(define-empty-tokens terminals
  (<eof>
   <lparen>
   <rparen>
   <comma>))

(define-lex-abbrevs
  (identifier (: (or alphabetic symbolic "-") (* (or alphabetic numeric symbolic "-")))))

(define rnlex
  (lexer
   [(eof) (token-<eof>)]
   [whitespace (rnlex input-port)]
   [(+ numeric) (token-<number> (string->number lexeme))]
   [identifier (token-<identifier> lexeme)]
   ["(" (token-<lparen>)]
   [")" (token-<rparen>)]
   ["," (token-<comma>)]))

(define rnparse
  (parser
   (start start)
   (end <eof>)
   (tokens non-terminals terminals)
   (error (λ (a b c) (void)))
   (grammar
    (start [() #f]
           [(error start) $2]
           [(exp) $1])
    (exp [(<identifier>) (ast/identifier $1)]
         [(<number>) (ast/literal $1)]
         [(message) $1]
         [(exp message) (ast/call $1 $2)])
    (message [(<identifier> <lparen> <rparen>) (ast/message $1)]))))

(define (parse s)
  (let ([result (rnparse (λ () (rnlex s)))])
    (when result
      (printf "~a\n" result)
      (parse s))))