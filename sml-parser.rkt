#lang racket

(require parser-tools/yacc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre))
(provide (all-defined-out))

(define (sml-lexer ip)
  (read-line ip))

(define (sml-parser thunk)
  (thunk))
