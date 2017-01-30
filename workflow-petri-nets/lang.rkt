#lang racket
(provide (rename-out [modbeg #%module-begin])
         (except-out (all-from-out racket)
                     #%module-begin))
(require "ui.rkt" redex/reduction-semantics (for-syntax syntax/parse racket/syntax))
(begin-for-syntax
  (define-splicing-syntax-class arc
    #:datum-literals (<- ->)
    (pattern (~seq (~seq f:id -> f2:id)))
    (pattern (~seq (~seq f2:id <- f:id)))))
(define-syntax modbeg
  (syntax-parser
    #:datum-literals (*)
    [(_
      (~optional (~seq #:draw draw*:str) #:defaults ([draw* #'#f]))
      #:places p*:id ...
      #:transitions t*:id ...
      #:arcs a:arc ...
      #:initials (~seq n:nat * s*:id) ...)
     #:with (p ...) (map (lambda (x) (format-symbol "p:~a" x)) (syntax->list #'(p* ...)))
     #:with (t ...) (map (lambda (x) (format-symbol "t:~a" x)) (syntax->list #'(t* ...)))
     #:with (s ...) (map (lambda (x) (format-symbol "p:~a" x)) (syntax->list #'(s* ...)))
     #:with draw (if (syntax-e #'draw*) #'draw* "circo")
     #'(#%module-begin (ui (term (((p ...) (t ...) ((a.f a.f2) ...)) ((s n) ...))) #:draw  'draw))]))
