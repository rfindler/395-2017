#lang racket/base

(require redex)
(provide (all-defined-out))

(module+ test
  (require rackunit))

(define-language L
  (op ::= + * < = div mod)

  (e ::=
     (λ (x ...) e)
     (λ x (x ...) e)
     (e e ...)
     (if e e e)
     x
     boolean
     number
     (op e e))

  (s ::=
     var
     (op v s)
     (op s v)
     (op s s))

  (v* ::= number boolean)

  (v ::=
     v*
     (λ (x ...) e)
     (λ x (x ...) e))

  (c ::=
     (assert s)
     (assert-! s))

  (var ::= (variable-prefix s))

  (E ::=
     hole
     (v ... E e ...)
     (if E e e)
     (op E e) (op v E))

  (Γ ::= concolic-environment)
  (M ::= concolic-virtual-machine)
  (x ::= variable-not-otherwise-mentioned)

  #:binding-forms
  (λ (x ...) e #:refers-to (shadow x ...))
  (λ x_1 (x_2 ...) e #:refers-to (shadow x_1 x_2 ...)))

(define λc-βv
  (reduction-relation
   L
   #:domain L
   (--> ((in-hole E ((λ (x ..._1) e) v ..._1))       Γ (pc= c ...))
        ((in-hole E (substitutes e (x ...) (v ...))) Γ (pc= c ...))
        "βv")

   (--> ((in-hole E ((λ x_f (x ...) e) v ...)) Γ (pc= c ...))
        ((in-hole E (substitutes e (x_f x ...) ((λ x_f (x ...) e) v ...)))
         Γ
         (pc= c ...))
        "βv-fix")))

(define-metafunction L
  δ : op number number -> boolean or number
  [(δ +    number_1 number_2) ,(+         (term number_1) (term number_2))]
  [(δ *    number_1 number_2) ,(*         (term number_1) (term number_2))]
  [(δ <    number_1 number_2) ,(<         (term number_1) (term number_2))]
  [(δ =    number_1 number_2) ,(=         (term number_1) (term number_2))]
  [(δ div  number_1 number_2) ,(quotient  (term number_1) (term number_2))]
  [(δ mod  number_1 number_2) ,(remainder (term number_1) (term number_2))])

(define-metafunction L
  [(substitutes e () ()) e]
  [(substitutes e (x_1 x_2 ...) (any_1 any_2 ...))
   (substitute (substitutes e (x_2 ...) (any_2 ...)) x_1 any_1)])

(module+ test
  (check-not-false
   (redex-match L
                s
                (term (= (+ sx 20) (* sx 2)))))

  (check-not-false
   (redex-match L
                e
                (term
                 (if (= 3 ((λ (x) (+ x 1)) y))
                     1
                     0)))))
