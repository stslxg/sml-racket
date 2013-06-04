#lang racket
(provide (all-defined-out))

(define (trans-id id)
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
    [_ id]))

(define (trans-lab num)
  (define trans-lab-table (list "first" "second" "third" "forth" "firth" "sixth" "seventh" "eighth" "ninth" "tenth"))
  (format "(lambda (tuple) \n(if (eq? (car tuple) 'exp-tuple) (~a (cadr tuple)) (void)))\n" 
          (list-ref trans-lab-table (- num 1))))

(define (racket-translator source)
  (match source
    [`(datum ,data)
     (if (string? data)
         (string-append "\"" data "\"")
         (number->string data))]
    [`(id ,id) (trans-id id)]
    [`(lab ,num) (trans-lab num)]
    [`(valbind ,val ,body)
     (format "(define ~a ~a)" (racket-translator val) (racket-translator body))]
    [`(funbind-mutual (,i ...))
     (apply string-append (for/list ([j i])
                            (racket-translator j)))]
    [`(funbind ,val (pat-tuple (,i ...)) ,body)
     (format "(define (~a ~a)\n ~a)" (racket-translator val)
             (apply string-append (for/list ([j i])
                                    (string-append (racket-translator j) " ")))
             (racket-translator body))]
    [`(app ,fun ,exp1 ,exp2)
     (format "(~a ~a ~a)" (racket-translator-inner fun) (racket-translator-inner exp1) (racket-translator-inner exp2))]
    [`(app ,fun (exp-tuple (,i ...)))
      (format "(~a ~a)" (racket-translator-inner fun)
              (apply string-append (for/list ([j i])
                                     (string-append (racket-translator-inner j) " "))))]
    [`(app ,fun ,exp)
     (format "(~a ~a)" (racket-translator-inner fun) (racket-translator-inner exp))]
    [`(exp-list (,i ...))
     (if (null? i)
         "'()"
         (format "(list ~a)" (apply string-append (for/list ([j i])
                                                    (string-append (racket-translator-inner j) " ")))))]
    [`(exp-tuple (,i ...))
     (format "(list 'exp-tuple (list ~a))" (apply string-append (for/list ([j i])
                                                             (string-append (racket-translator-inner j) " "))))]
    [`(seq (,i ...))
     (format "(begin\n ~a)" (apply string-append (for/list ([j i])
                                                   (string-append (racket-translator j) "\n"))))]
    [`(let (,let-dec ...) (,exp-let ...))
     (format "~a ~a"
             (apply string-append (for/list ([j let-dec])
                                    (string-append (racket-translator j) "\n")))
             (apply string-append (for/list ([j exp-let])
                                    (string-append (racket-translator j) "\n"))))]
    [`(and ,exp1 ,exp2)
     (format "(and ~a ~a)" (racket-translator-inner exp1) (racket-translator-inner exp2))]
    [`(or ,exp1 ,exp2)
     (format "(or ~a ~a)" (racket-translator-inner exp1) (racket-translator-inner exp2))]
    [`(if ,clause ,then ,else)
     (format "(if ~a\n ~a\n ~a)" (racket-translator-inner clause) (racket-translator-inner then) (racket-translator-inner else))]
    [`(lambda (pat-tuple (,i ...)) ,body)
     (format "(lambda (~a) ~a)\n" (apply string-append (for/list ([j i])
                                                         (string-append (racket-translator j) " ")))
             (racket-translator body))]
    [`(lambda ,id ,body)
     (format "(lambda (~a) ~a)\n" (racket-translator id) (racket-translator body))]
    [(list i ...)
      (apply string-append (for/list ([i source])
                             (string-append (racket-translator i) "\n")))]
    [_ "(void)"]))
(define (racket-translator-inner source)
  (match source
    [`(let (,let-dec ...) (,exp-let ...))
     (format "(letrec (~a) ~a)"
             (apply string-append (for/list ([j let-dec])
                                    (string-append (racket-translator-inner j) "\n")))
             (apply string-append (for/list ([j exp-let])
                                    (string-append (racket-translator j) "\n"))))]
    [`(valbind ,val ,body)
     (format "[~a ~a]" (racket-translator val) (racket-translator-inner body))]
    [`(funbind-mutual (,i ...))
     (apply string-append (for/list ([j i])
                            (racket-translator-inner j)))]
    [`(funbind ,val (pat-tuple (,i ...)) ,body)
     (format "[~a (lambda (~a) ~a)]" 
             (racket-translator val)
             (apply string-append (for/list ([j i])
                                    (string-append (racket-translator j) " ")))
             (racket-translator body))]
    [_ (racket-translator source)]))