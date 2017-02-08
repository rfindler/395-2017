#lang racket/base

(require "commons.rkt")
(require redex)

(provide (all-defined-out))

(define-extended-language M L
  (e ::= ....)

  (e* ::=
      v
      (if s e* e*))

  (F ::=
     hole
     (v ... F e ...)
     (if F e e)
     (op F e) (op v F))

  (G ::= hole
     (if s G e)
     (if s e* G))

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

   (--> ((in-hole E (if (if s e_1 e_2) e_3 e_4))              Γ (pc=))
        ((in-hole E (if s (if e_1 e_3 e_4) (if e_2 e_3 e_4))) Γ (pc=))
        "if-if-lift")

   (--> ((in-hole E (op v_1 (if s e_1 e_2)))          Γ (pc=))
        ((in-hole E (if s (op v_1 e_1) (op v_1 e_2))) Γ (pc=))
        "op-if-lift-r")

   (--> ((in-hole E (op (if s e_1 e_2) e_3))          Γ (pc=))
        ((in-hole E (if s (op e_1 e_3) (op e_2 e_3))) Γ (pc=))
        "op-if-lift-l")

   (--> ((in-hole E (v ... (if s e_1 e_2) e_3 ...))                 Γ (pc=))
        ((in-hole E (if s (v ... e_1 e_3 ...) (v ... e_2 e_3 ...))) Γ (pc=))
        "ap-if-lift")

   ;; E[if s e_1 e_2]  ->  if s E[e_1] E[e_2]
   #;
   (--> ((in-hole G (in-hole F (if s e_1 e_2)))             Γ (pc=))
        ((in-hole G (if s (in-hole F e_1) (in-hole F e_2))) Γ (pc=))
        "F-if-lift")))

(define concres
  (reduction-relation
   M
   (--> (e Γ (pc= c ...))
        ((final-value Γ e) (e Γ (pc= c ...))))))

(define-metafunction M
  final-value : Γ e -> number or boolean
  [(final-value ([var ↦ v / s_1] ...) e)
   ,(eval (term (let ([mod remainder] [div quotient])
                  (let ([var v] ...)
                    e)))
          (make-base-namespace))])

(define (pp v port width text)
  (term-let ([(e ([var ↦ number / s] ...) (pc= c ...)) v])
    (default-pretty-printer (term (e)) port width text)))
