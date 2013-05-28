#lang racket

(require parser-tools/yacc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide (all-defined-out))

(define-tokens data (DATUM ID))
(define-empty-tokens delim (fun SPACE COMMA OP CP LOP LCP DOT EOF))
  
(define sml-lexer
  (lexer
    [(:or sml-whitespace comment) (sml-lexer input-port)] 
    [(:: #\# #\" any-char #\") (token-DATUM (caddr (string->list lexeme)))]
    [#\" (token-DATUM (list->string (get-string-token input-port)))]
    ;;["fun" 'FUN]
    [#\, 'COMMA]
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
   (end EOF)
   (tokens data delim)
   (error (lambda (a b c) (void)))

   (precs (right OP))
   
   (grammar
    
    (start [() #f]
           [(error start) #f]
           [(exp start) #f])
    
    (exp [(DATUM) (print $1)]
         [(ID) (print $1)]
         ;;[(delim) (void)]
         [(fun ID OP ID COMMA ID CP) (print (list "fun" $2 "(" $4 "," $6 ")"))]
         ))))

;;for testing

(define (sml-parser-test input-port)
  (port-count-lines! input-port)
  (sml-parser (lambda () (sml-lexer input-port))))

(display (sml-parser-test (open-input-file "test.sml" #:mode 'text)))
