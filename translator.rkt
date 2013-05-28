#lang racket
(provide (all-defined-out))

(define (trans-id id)
  (match id
    ["null" "null?"]
    ["::" "cons"]
    ["hd" "car"]
    ["tl" "cdr"]
    [_ id]))

(define (racket-translator source)
  (match source
    [`(datum ,data)
     (if (string? data)
         (string-append "\"" data "\"")
         (number->string data))]
    [`(id ,id) (trans-id id)]
    [`(valbind ,val ,body)
     (format "(define ~a ~a)" (racket-translator val) (racket-translator body))]
    [`(funbind ,val (pat-tuple (,i ...)) ,body)
     (format "(define (~a ~a)\n ~a)" (racket-translator val)
             (apply string-append (for/list ([j i])
                                    (string-append (racket-translator j) " ")))
             (racket-translator body))]
    [`(app ,fun ,exp1 ,exp2)
     (format "(~a ~a ~a)" (racket-translator fun) (racket-translator exp1) (racket-translator exp2))]
    [`(app ,fun (exp-tuple (,i ...)))
      (format "(~a ~a)" (racket-translator fun)
              (apply string-append (for/list ([j i])
                                     (string-append (racket-translator j) " "))))]
    [`(app ,fun ,exp)
     (format "(~a ~a)" (racket-translator fun) (racket-translator exp))]
    [`(exp-list (,i ...))
     (if (null? i)
         "'()"
         (format "(list ~a)" (apply string-append (for/list ([j i])
                                                    (string-append (racket-translator j) " ")))))]
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
     (format "(and ~a ~a)" (racket-translator exp1) (racket-translator exp2))]
    [`(or ,exp1 ,exp2)
     (format "(or ~a ~a)" (racket-translator exp1) (racket-translator exp2))]
    [`(if ,clause ,then ,else)
     (format "(if ~a\n ~a\n ~a)" (racket-translator clause) (racket-translator then) (racket-translator else))]
    [(list i ...)
      (apply string-append (for/list ([i source])
                             (string-append (racket-translator i) "\n")))]
    [_ "(void)"]))