#lang racket
(provide (all-defined-out))

(define (trans-id id env)
  (if (member id env)
      id
      (match id
        ["true" "#t"]
        ["false" "#f"]
        
        ;; For List
        [(or "List.null" "null") "null?"]
        ["::" "cons"]
        ["@" "append"]
        ["hd" "car"]
        ["tl" "cdr"]
        ["List.length" "length"]
        ["List.last" "last"]
        ["List.take" "take"]
        ["List.drop" "drop"]
        [(or "List.nth" "nth") "list-ref"]
        [(or "List.rev" "rev") "reverse"]
        [(or "List.concat" "concat") "flatten"]
        ["List.map" "map"]
        ["List.filter" "filter"]
        ["List.foldl" "foldl"]
        ["List.foldr" "foldr"]
        [(or "List.all" "all") "andmap"]
        [(or "List.exists" "exists") "ormap"]
        [(or "List.find" "find") "findf"]
        
        ; For String 
        ["String.size" "string-length"]
        ["String.sub" "string-ref"]
        ["String.substring" "substring"]
        ["String.concat" "string-append*"]
        [(or "String.implode" "implode") "list->string"]
        [(or "String.explode" "explode") "string->list"]
        ["^" "string-append"]
        ["Int.toString" "number->string"]
        ["String.toInt" "string->number"]
        
        ; Misc
        ["mod" "remainder"]
        ["div" "quotient"]
        ["<>" "(compose not =)"]
        ["print" "display"]
        [_ id])))

(define (trans-lab num)
  (define trans-lab-table (list "first" "second" "third" "forth" "firth" "sixth" "seventh" "eighth" "ninth" "tenth"))
  (format "(lambda (tuple) \n(if (eq? (car tuple) 'exp-tuple) (~a (cadr tuple)) (void)))\n" 
          (list-ref trans-lab-table (- num 1))))

(define (racket-translator-without-env source)
  (match source
    [`(datum ,data)
     (if (string? data)
         (string-append "\"" data "\"")
         (number->string data))]
    [`(id ,id) id]                        ; may have some error
    [`(lab ,num) (trans-lab num)]))

(define (racket-translator source env)
  (match source
    [`(datum ,data)
     (if (string? data)
         (string-append "\"" data "\"")
         (number->string data))]
    [`(id ,id) (trans-id id env)]
    [`(lab ,num) (trans-lab num)]
    [`(valbind ,val ,body)
     (define val-name (racket-translator-without-env val))
     (format "(define ~a ~a)" val-name (racket-translator body (cons val-name env)))]
    [`(funbind-mutual (,i ...))            ; haven't deal with mutual functions up to now
     (apply string-append (for/list ([j i])
                            (racket-translator j env)))]
    [`(funbind ,val (pat-tuple (,i ...)) ,body)     ; haven't deal with functions up to now
     (format "(define (~a ~a)\n ~a)" (racket-translator-without-env val)
             (apply string-append (for/list ([j i])
                                    (string-append (racket-translator j) " ")))
             (racket-translator body))]
    [`(app ,fun ,exp1 ,exp2)
     (format "(~a ~a ~a)" (racket-translator-inner fun env) (racket-translator-inner exp1 env) (racket-translator-inner exp2 env))]
    [`(app ,fun (exp-tuple (,i ...)))
      (format "(~a ~a)" (racket-translator-inner fun env)
              (apply string-append (for/list ([j i])
                                     (string-append (racket-translator-inner j env) " "))))]
    [`(app ,fun ,exp)
     (format "(~a ~a)" (racket-translator-inner fun env) (racket-translator-inner exp env))]
    [`(exp-list (,i ...))
     (if (null? i)
         "'()"
         (format "(list ~a)" (apply string-append (for/list ([j i])
                                                    (string-append (racket-translator-inner j env) " ")))))]
    [`(exp-tuple (,i ...))
     (format "(list 'exp-tuple (list ~a))" (apply string-append (for/list ([j i])
                                                             (string-append (racket-translator-inner j env) " "))))]
    [`(seq (,i ...))
     (format "(begin\n ~a)" (apply string-append (for/list ([j i])
                                                   (string-append (racket-translator j env) "\n"))))]
    [`(let (,let-dec ...) (,exp-let ...))      ; haven't deal with let up to now
     (format "~a ~a"
             (apply string-append (for/list ([j let-dec])
                                    (string-append (racket-translator j) "\n")))
             (apply string-append (for/list ([j exp-let])
                                    (string-append (racket-translator j) "\n"))))]
    [`(and ,exp1 ,exp2)
     (format "(and ~a ~a)" (racket-translator-inner exp1 env) (racket-translator-inner exp2 env))]
    [`(or ,exp1 ,exp2)
     (format "(or ~a ~a)" (racket-translator-inner exp1 env) (racket-translator-inner exp2 env))]
    [`(if ,clause ,then ,else)
     (format "(if ~a\n ~a\n ~a)" (racket-translator-inner clause env) (racket-translator-inner then env) (racket-translator-inner else env))]
    [`(lambda (pat-tuple (,i ...)) ,body)      ; haven't deal with lambda up to now
     (format "(lambda (~a) ~a)\n" (apply string-append (for/list ([j i])
                                                         (string-append (racket-translator j) " ")))
             (racket-translator body))]
    [`(lambda ,id ,body)
     (define id-name (racket-translator-without-env id))
     (format "(lambda (~a) ~a)\n" id-name (racket-translator body (cons id-name env)))]
    [(list i ...)                        ; haven't deal with sequences up to now
      (apply string-append (for/list ([i source])
                             (string-append (racket-translator i env) "\n")))]
    [_ "(void)"]))
(define (racket-translator-inner source env)
  (match source
    [`(let (,let-dec ...) (,exp-let ...))         ; haven't deal with recursive let up to now
     (format "(letrec (~a) ~a)"
             (apply string-append (for/list ([j let-dec])
                                    (string-append (racket-translator-inner j) "\n")))
             (apply string-append (for/list ([j exp-let])
                                    (string-append (racket-translator j) "\n"))))]
    [`(valbind ,val ,body)
     (define val-name (racket-translator-without-env val))
     (format "[~a ~a]" val-name (racket-translator-inner body (cons val-name env)))]
    [`(funbind-mutual (,i ...))                  ; haven't deal with mutual functions up to now
     (apply string-append (for/list ([j i])
                            (racket-translator-inner j)))]
    [`(funbind ,val (pat-tuple (,i ...)) ,body)  ; haven't deal with functions up to now
     (format "[~a (lambda (~a) ~a)]" 
             (racket-translator val)
             (apply string-append (for/list ([j i])
                                    (string-append (racket-translator j) " ")))
             (racket-translator body))]
    [_ (racket-translator source env)]))