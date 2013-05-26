#lang racket

(require "sml-parser.rkt"
         "translator.rkt")

;;assemble all the components
 
(define (sml->racket input-port)
  (port-count-lines! input-port)
  (racket-translator (sml-parser (lambda () (sml-lexer input-port)))))
  
;;for testing

(display (sml->racket (open-input-file "test.sml" #:mode 'text)))
