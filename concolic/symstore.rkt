#lang racket/base

(require "commons.rkt")
(require redex)

(provide (all-defined-out))

(define-extended-language M L
  (v ::= ....
     var)

  (Γ ::= ([var ↦ v / s] ...))
  (M ::=
     (e Γ (pc= c ...))))

(define conc
  (extend-reduction-relation
   λc-βv
   M
   #:domain M

   (--> ((in-hole E (op v_1 v_2)) Γ (pc= c ...))
        ((in-hole E s)
         (extend Γ [s ↦
                      (δ op (concrete-value Γ v_1) (concrete-value Γ v_2)) /
                      (op (symbolic-value Γ v_1) (symbolic-value Γ v_2))])
         (pc= c ...))

        (fresh s)
        (judgment-holds (is-symbolic? (op v_1 v_2)))
        "δ-sym")

   (--> ((in-hole E (op number_1 number_2)) Γ (pc= c ...))
        ((in-hole E (δ op number_1 number_2)) Γ (pc= c ...))
        "δ")

   (--> ((in-hole E (if var e_1 e_2)) Γ (pc= c ...))
        ((in-hole E (if (concrete-value Γ var) e_1 e_2))
         Γ
         (pc= c ... (make-assert Γ var)))
        "if-pc")

   (--> ((in-hole E (if #t e_1 e_2)) Γ (pc= c ...))
        ((in-hole E e_1) Γ (pc= c ...))
        "if-#t")

   (--> ((in-hole E (if #f e_1 e_2)) Γ (pc= c ...))
        ((in-hole E e_2) Γ (pc= c ...))
        "if-#f")))

(define-metafunction M
  make-assert : Γ var -> c

  [(make-assert Γ var)
   (assert (symbolic-value Γ var))
   (where #t (concrete-value Γ var))]

  [(make-assert Γ var)
   (assert-! (symbolic-value Γ var))
   (where #f (concrete-value Γ var))])

(define-judgment-form M
  #:mode (is-symbolic? I)
  #:contract (is-symbolic? e)

  [----------------------------- "sym-var-l"
   (is-symbolic? (op var_1 v_2))]

  [----------------------------- "sym-var-r"
   (is-symbolic? (op v_1 var_2))])

(define-metafunction M
  concrete-value : Γ v -> number or boolean
  [(concrete-value Γ number) number]
  [(concrete-value Γ boolean) boolean]
  [(concrete-value ([var_1 ↦ v_1 / s_1] [var_2 ↦ v_2 / s_2] ...)
                   var_1)
   v_1]
  [(concrete-value ([var_1 ↦ v_1 / s_1] [var_2 ↦ v_2 / s_2] ...)
                   var_3)
   (concrete-value ([var_2 ↦ v_2 / s_2] ...)
                   var_3)])

(define-metafunction M
  symbolic-value : Γ v -> s or v
  [(symbolic-value Γ number) number]
  [(symbolic-value Γ boolean) boolean]
  [(symbolic-value ([var_1 ↦ v_1 / s_1] [var_2 ↦ v_2 / s_2] ...)
                   var_1)
   s_1]
  [(symbolic-value ([var_1 ↦ v_1 / s_1] [var_2 ↦ v_2 / s_2] ...)
                   var_3)
   (symbolic-value ([var_2 ↦ v_2 / s_2] ...)
                   var_3)])

(define-metafunction M
  [(extend ([var_1 ↦ v_1 / s_1] ...) [var_2 ↦ v_2 / s_2])
   ([var_2 ↦ v_2 / s_2] [var_1 ↦ v_1 / s_1] ...)])

(define concres
  (reduction-relation
   M
   (--> (e Γ (pc= c ...))
        ((concrete-value Γ e) (e Γ (pc= c ...))))))

(define pp default-pretty-printer)

(module+ test
  (require rackunit)

  (define Γ1
    (term ([sc ↦ 2 / sc] [sw ↦ -1 / sw] [sc ↦ 3 / sc])))

  (check-equal?
   (term (concrete-value ,Γ1 12))
   12)

  (check-equal?
   (term (concrete-value ,Γ1 sc))
   2)

  (check-equal?
   (term (concrete-value ,Γ1 sw))
   -1)

  (define Γ2
    (term ([sc ↦ 2 / sc] [sw ↦ -1 / sw] [sc ↦ 3 / sc])))

  (check-equal?
   (term (symbolic-value ,Γ2 12))
   12)

  (check-equal?
   (term (symbolic-value ,Γ2 sc))
   (term sc))

  (check-equal?
   (term (symbolic-value ,Γ2 sw))
   (term sw)))
