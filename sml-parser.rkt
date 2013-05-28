#lang racket

(require parser-tools/yacc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide (all-defined-out))

(define-tokens data (DATUM ID))
(define-empty-tokens delim (= VAL FUN SPACE SEMI-CO COMMA OP CP LOP LCP DOT EOF))
  
(define sml-lexer
  (lexer
    [(:or sml-whitespace comment) (sml-lexer input-port)] 
    [(:: #\# #\" any-char #\") (token-DATUM (caddr (string->list lexeme)))]
    [#\" (token-DATUM (list->string (get-string-token input-port)))]
    ["val" 'VAL]
    ["fun" 'FUN]
    [#\; 'SEMI-CO]
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
   (start prog)
   (end EOF)
   (tokens data delim)
   (error (lambda (a b c) (void)))

   (precs (right OP))
   
   (grammar
    
    (prog [() '()]
          [(error prog) $2]
          [(dec SEMI-CO prog) (cons $1 $3)]
          [(dec prog) (cons $1 $2)])
    
    (dec [(VAL valbind) `(valbind ,@$2)]
         [(FUN funbind) `(funbind ,(first $2) ,(second $2) ,(third $2))]
         [() '()]
         [(dec dec) (cons $1 $2)]
         [(dec SEMI-CO dec) (cons $1 $3)])
    (valbind [(pat = exp) (list $1 $3)])
    (funbind [(funmatch) $1])
    (funmatch [(ID pat = exp) (list $1 $2 $4)])
    (pat [(DATUM) $1]
         [(ID) $1]
         [(OP pat CP) (list $2)]
         [(OP pat pat-tuple) (cons $2 $3)]
         [(LOP pat pat-list) (cons $2 $3)])
    (pat-tuple [(COMMA 
    )))

;;for testing

(define (sml-parser-test input-port)
  (port-count-lines! input-port)
  (sml-parser (lambda () (sml-lexer input-port))))

(display (sml-parser-test (open-input-file "test.sml" #:mode 'text)))
