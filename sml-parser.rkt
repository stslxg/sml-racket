#lang racket
(provide (all-defined-out))

(define (sml-lexer ip)
  (read-line ip))

(define (sml-parser thunk)
  (thunk))
