#lang racket/base

(require "commons.rkt")
(require redex)

(provide (all-defined-out))

(define-extended-language M L
  (guard ::= guard/t guard/f)

  (e ::= ....
     (if (e / s) e e)
     (guard s e))

  (F ::=
     hole
     (v ... F e ...)
     (if F e e)
     (op F e) (op v F)
     (if (F / s) e e))

  (G ::= hole
     (guard s G))

  (E ::= (in-hole G F))

  (v ::= ....
     s)

  (Γ ::= ([var ↦ v* / s] ...))
  (M ::=
     (e Γ (pc= c ...))))

(define conc
  (extend-reduction-relation
   λc-βv
   M
   #:domain M

   (--> ((in-hole E (op number_1 number_2))   Γ (pc=))
        ((in-hole E (δ op number_1 number_2)) Γ (pc=))
        "δ")


   (--> ((in-hole E (if #t e_1 e_2)) Γ (pc=))
        ((in-hole E e_1)             Γ (pc=))
        "if-#t")

   (--> ((in-hole E (if #f e_1 e_2)) Γ (pc=))
        ((in-hole E e_2)             Γ (pc=))
        "if-#f")

   (--> ((in-hole E (if  s                         e_1 e_2)) Γ (pc=))
        ((in-hole E (if ((concrete-term Γ s) / s) e_1 e_2)) Γ (pc=))
        "if-sym")

   (--> ((in-hole E (if (#t / s) e_1 e_2)) Γ (pc=))
        ((in-hole E (guard/t  s  e_1))     Γ (pc=))
        "if-st")

   (--> ((in-hole E (if (#f / s) e_1 e_2)) Γ (pc=))
        ((in-hole E (guard/f  s      e_2)) Γ (pc=))
        "if-sf")

   (--> ((in-hole E (if (guard s e_1) e_2 e_3)) Γ (pc=))
        ((in-hole E (guard s (if e_1 e_2 e_3))) Γ (pc=))
        "if-if-lift")

   (--> ((in-hole E (op v_1 (guard s e_2))) Γ (pc=))
        ((in-hole E (guard s (op v_1 e_2))) Γ (pc=))
        "op-if-lift-r")

   (--> ((in-hole E (op (guard s e_1) e_2)) Γ (pc=))
        ((in-hole E (guard s (op e_1 e_2))) Γ (pc=))
        "op-if-lift-l")

   (--> ((in-hole E (v ... (guard s e_1) e_2 ...)) Γ (pc=))
        ((in-hole E (guard s (v ... e_1 e_2 ...))) Γ (pc=))
        "ap-if-lift")))

(define-metafunction M
  concrete-term : Γ s -> e
  [(concrete-term ([var ↦ v / s_1] ...) s_2)
   (substitutes s_2 (var ...) (v ...))])

(define concres
  (reduction-relation
   M
   (--> ((guard/t s e) Γ (pc= c ...)) (e Γ (pc= c ... (assert   s))))
   (--> ((guard/f s e) Γ (pc= c ...)) (e Γ (pc= c ... (assert-! s))))
   (--> ((in-hole G v) Γ (pc= c ...))
        ((final-value Γ v) (e Γ (pc= c ...))))))

(define-metafunction M
  final-value : Γ e -> number or boolean
  [(final-value ([var ↦ v / s_1] ...) v_2)
   ,(eval (term (let ([mod remainder] [div quotient])
                  (let ([var v] ...)
                    v_2)))
          (make-base-namespace))])

(define (pp v port width text)
  (term-let ([(e ([var ↦ number / s] ...) (pc= c ...)) v])
    (default-pretty-printer (term (e [var ↦ number] ... )) port width text)))
