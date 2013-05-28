#lang racket

(require parser-tools/yacc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide (all-defined-out))

(define-tokens data (DATUM ID))
(define-empty-tokens delim (OP CP LOP LCP DOT EOF))
  
(define sml-lexer
  (lexer
    [(:or sml-whitespace comment) (token-DATUM #\space)] 
    [(:: #\# #\" any-char #\") (token-DATUM (caddr (string->list lexeme)))]
    ["#\\space" (token-DATUM #\space)]
    ["#\\newline" (token-DATUM #\newline)]
    [#\" (token-DATUM (list->string (get-string-token input-port)))]
    [#\( 'OP]
    [#\) 'CP]
    [#\[ 'LOP]
    [#\] 'LCP]
    [identifier (token-ID lexeme)]
    [int10 (token-DATUM (string->number 
                         (string-replace lexeme "~" "-") 10))]
    [float10 (token-DATUM (string->number 
                           (string-replace 
                            (string-replace
                             (string-replace lexeme "e" "e+")
                             "+~" "-") "~" "-") 10))]
    ["." 'DOT]
    [(eof) 'EOF]))
  
(define get-string-token
  (lexer
    [(:~ #\" #\\) (cons (car (string->list lexeme))
                        (get-string-token input-port))]
    [(:: #\\ #\\) (cons #\\ (get-string-token input-port))]
    [(:: #\\ #\") (cons #\" (get-string-token input-port))]
    [#\" null]))
  
(define-lex-abbrevs
  [letter (:or (:/ "a" "z") (:/ #\A #\Z))]
  [digit (:/ #\0 #\9)]
  [sml-whitespace (:or #\newline #\return #\tab #\space #\vtab)]
  [comment (:: #\( #\* (:* (:~ #\* #\))) #\* #\))]
  [num10 (:+ digit)]
  [int10 (:: sign num10)]
  [float10 (:or (:: sign num10 #\. num10)
                (:: sign num10 (:or "" (:: #\. num10)) "e" sign num10))]
  [sign (:or "" "~")]
  [identifier (:or alphanumeric-id symbolic-id)]
  [alphanumeric-id (:: letter (:* (:or letter digit #\' #\_)))]
  [symbolic-id (:+ (:or #\! #\% #\& #\$ #\+ #\- #\/ #\:
                        #\< #\> #\= #\? #\@ #\\ #\~ #\`
                        #\^ #\| #\*))])
  
(define sml-parser
  (parser
   (start start)
   (end newline EOF)
   (tokens value-tokens op-tokens)
   (error (lambda (a b c) (void)))

   (precs (right =)
          (left - +)
          (left * /)
          (left NEG)
          (right ^))
   
   (grammar
    
    (start [() #f]
           ;; If there is an error, ignore everything before the error
           ;; and try to start over right after the error
           [(error start) $2]
           [(exp) $1])
    
    (exp [(NUM) $1]
         [(VAR) (hash-ref vars $1 (lambda () 0))]
         [(VAR = exp) (begin (hash-set! vars $1 $3)
                             $3)]
         [(FNCT OP exp CP) ($1 $3)]
         [(exp + exp) (+ $1 $3)]
         [(exp - exp) (- $1 $3)]
         [(exp * exp) (* $1 $3)]
         [(exp / exp) (/ $1 $3)]
         [(- exp) (prec NEG) (- $2)]
         [(exp ^ exp) (expt $1 $3)]
         [(OP exp CP) $2]))))
