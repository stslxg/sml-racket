#lang racket

(require parser-tools/yacc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide (all-defined-out))

(define-tokens data (LAB DATUM AID SID))
(define-empty-tokens delim (LAMBDA-FN LAMBDA-ARROW AND IF THEN ELSE ANDALSO ORELSE LET IN END 
                                      VAL FUN SPACE SEMI-CO COMMA OP CP LOP LCP DOT EOF ASSIGNOP))
  
(define sml-lexer
  (lexer
    [(:or sml-whitespace comment) (sml-lexer input-port)] 
    [(:: #\# #\" any-char #\") (token-DATUM (caddr (string->list lexeme)))]
    [#\" (token-DATUM (list->string (get-string-token input-port)))]
    ["fn" 'LAMBDA-FN]
    ["=>" 'LAMBDA-ARROW]
    ["and" 'AND]
    ["val" 'VAL]
    ["fun" 'FUN]
    ["let" 'LET]
    ["in" 'IN]
    ["end" 'END]
    ["andalso" 'ANDALSO]
    ["orelse" 'ORELSE]
    ["if" 'IF]
    ["then" 'THEN]
    ["else" 'ELSE]
    [#\= 'ASSIGNOP]
    [#\; 'SEMI-CO]
    [#\, 'COMMA]
    [#\( 'OP]
    [#\) 'CP]
    [#\[ 'LOP]
    [#\] 'LCP]
    [lab (token-LAB (string->number (string-replace lexeme "#" "")))]
    [symbolic-id (token-SID lexeme)]
    [alphanumeric-id (token-AID lexeme)]
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
  [comment (:: "(*" (complement (:: any-string "*)" any-string)) "*)")]
  [num10 (:+ digit)]
  [int10 (:: sign num10)]
  [float10 (:or (:: sign num10 #\. num10)
                (:: sign num10 (:or "" (:: #\. num10)) "e" sign num10))]
  [sign (:or "" "~")]
  [identifier (:or alphanumeric-id symbolic-id)]
  [lab (:: #\# num10)]
  [alphanumeric-id (:: a-id (:* (:: #\. a-id)))]
  [a-id (:: letter (:* (:or letter digit #\' #\_)))]
  [symbolic-id (:or "mod" "div" (:- (:+ (:or #\! #\% #\& #\$ #\+ #\- #\/ #\:
                                             #\< #\> #\= #\? #\@ #\\ #\~ #\`
                                             #\^ #\| #\*)) #\=))])
  
(define sml-parser
  (parser
   (start prog)
   (end EOF)
   (tokens data delim)
   (error (lambda (tok-ok? tok-name tok-value) (print tok-name)(print tok-value)))
   (debug "yacc.log")
   (suppress)
   (precs (left SEMI-CO)
          (left COMMA)
          (nonassoc VAL)
          (nonassoc FUN)
          (left AND)
          (left LAMBDA-ARROW)
          (left LAMBDA-FN)
          (left ORELSE)
          (left ANDALSO)
          (left LAB)          
          (left ASSIGNOP)
          (left SID)
          (left AID))
   
   (grammar
    
    (prog [() '()]
          [(error prog) $2]
          [(dec SEMI-CO prog) (cons $1 $3)]
          [(dec prog) (cons $1 $2)]
          [(exp SEMI-CO prog) (cons $1 $3)])  
    (dec [(VAL valbind) `(valbind ,(first $2) ,(second $2))]
         [(FUN funbind) `(funbind-mutual ,$2)])
    (let-dec [() '()]
             [(dec SEMI-CO let-dec) (cons $1 $3)]
             [(dec let-dec) (cons $1 $2)])
    (valbind [(pat ASSIGNOP exp) (list $1 $3)])
    (funbind [(funmatch) `((funbind ,(first $1) ,(second $1) ,(third $1)))]
             [(funmatch AND funbind) (cons `(funbind ,(first $1) ,(second $1) ,(third $1)) $3)])
    (funmatch [(AID pat ASSIGNOP exp) (list `(id ,$1) $2 $4)])
    (pat [(DATUM) `(datum ,$1)]
         [(AID) `(id ,$1)]
         [(OP pat CP) `(pat-tuple ,(list $2))]
         [(pat-tuple pat CP) `(pat-tuple ,(reverse (cons $2 $1)))])
    (pat-tuple [(OP pat COMMA) (list $2)]
               [(pat-tuple pat COMMA) (cons $2 $1)])
    (exp [(DATUM) `(datum ,$1)]
         [(AID) `(id ,$1)]
         [(exp exp) (prec SID) `(app ,$1 ,$2)]
         [(LAB) `(lab ,$1)]
         [(exp SID exp) `(app (id ,$2) ,$1 ,$3)]
         [(exp ASSIGNOP exp) `(app (id ,"=") ,$1 ,$3)]
         [(OP exp CP) $2]
         [(exp-tuple exp CP) `(exp-tuple ,(reverse (cons $2 $1)))]
         [(exp-list exp LCP) `(exp-list ,(reverse (cons $2 $1)))]
         [(LOP LCP) '(exp-list ())]
         [(exp-seq exp CP) `(seq ,(reverse (cons $2 $1)))]
         [(LET let-dec IN exp-let END) `(let ,$2 ,(reverse $4))]
         [(exp ANDALSO exp) `(and ,$1 ,$3)]
         [(exp ORELSE exp) `(or ,$1 ,$3)]
         [(IF exp THEN exp ELSE exp) `(if ,$2 ,$4 ,$6)]
         [(LAMBDA-FN pat LAMBDA-ARROW exp) `(lambda ,$2 ,$4)])
    (exp-tuple [(OP exp COMMA) (list $2)]
               [(exp-tuple exp COMMA) (cons $2 $1)])
    (exp-list [(LOP) '()]
              [(exp-list exp COMMA) (cons $2 $1)])
    (exp-seq [(OP exp SEMI-CO) (list $2)]
             [(exp-seq exp SEMI-CO) (cons $2 $1)])
    (exp-let [(exp) (list $1)]
             [(exp-let SEMI-CO exp) (cons $3 $1)])
    )))

;;for testing

(define (sml-parser-test input-port)
  (port-count-lines! input-port)
  (sml-parser (lambda () (sml-lexer input-port))))

;(print (sml-parser-test (open-input-file "test.sml" #:mode 'text)))
