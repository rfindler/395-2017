#lang racket
(require redex/reduction-semantics)
(provide (all-defined-out))
(define-syntax quasiquote (make-rename-transformer #'term))
(module+ test (require rackunit))

(define-language Net
  ;; machines
  (S ::= (PN M))
  (M state ::=  {(p natural) ...})
  (PN petrie-net ::= (P T F))
  (P places ::= {p ...})
  (T transitions ::= {t ...})
  (F arcs flow-relation::= {f ...})
  (p place ○ ::= (variable-prefix p:))
  (t transition □ ::= (variable-prefix t:))
  (f arc flow ::= (p t) (t p))
  (n node ::= p t)
  (nat ::= natural)
  ;;
  (σ ::= (t ...))
  (C path ::= (n ...)))




(define-metafunction Net
  enabled? : S t -> boolean
  ;; forall p that are inputs to T, (M p) isn't zero
  [(enabled? (PN M) t)
   #t
   (where (P T F) PN)
   (where (p ...) (· t F))
   (where (#t ...)
          ((good p M) ...))]
  [(enabled? (PN M) t) #f])

(define-metafunction Net
  good : p M -> boolean
  [(good p ((p_0 natural_0) ... (p natural) (p_2 natural_2) ...))
   ,(positive? `natural)]
  [(good p M) #f])

(define-metafunction Net
  · : t F -> (p ...)
  [(· t ()) ()]
  [(· t ((p t) f ...))
   (p p_* ...)
   (where (p_* ...) (· t (f ...)))]
  [(· t (f_not f ...))
   (· t (f ...))])

(define-metafunction Net
  outputs : t F -> (p ...)
  [(outputs t ()) ()]
  [(outputs t ((t p) f ...))
   (p p_* ...)
   (where (p_* ...) (outputs t (f ...)))]
  [(outputs t (f_not f ...))
   (outputs t (f ...))])

(define single-traffic
  (term
   (((p:red p:green p:orange)
     (t:to-green t:to-orange t:to-red)
     ((p:red t:to-green)
      (t:to-green p:green)
      (p:green t:to-orange)
      (t:to-orange p:orange)
      (p:orange t:to-red)
      (t:to-red p:red)))
    ((p:red 1) (p:green 0) (p:orange 0)))))

(module+ test
  (check-equal? `(· t:to-green ,(third (first single-traffic)) ) `(p:red))
  (check-true `(good p:red ,(second single-traffic)))
  (check-true `(enabled? ,single-traffic t:to-green))
  (define multi-test
    `(((p:i p:o1 p:o2)
       (t:t)
       ((p:i t:t)
        (t:t p:o1)
        (t:t p:o2)))
     ((p:i 1)
      (p:o1 0)
      (p:o2 0))))
  (check-equal?
   `(outputs t:t ,(third (first multi-test)))
   `(p:o1 p:o2))
  (test-judgment-holds
   (-->t
    ,multi-test
    t:t
    (((p:i p:o1 p:o2)
      (t:t)
      ((p:i t:t)
       (t:t p:o1)
       (t:t p:o2)))
     ((p:i 0)
      (p:o1 1)
      (p:o2 1))))))

(define-judgment-form Net
  #:contract (-->t S t S)
  #:mode     (-->t I I O)
  [(where (P (t_f ... t t_r ...) F) PN)
   (side-condition (enabled? (PN M) t))
   (where S_* (update-outputs t (update-inputs t (PN M))))
   ---------------
   (-->t (PN M) t S_*)])

(define-metafunction Net
  update-inputs : t S -> S
  [(update-inputs t ((P T F) M))
   (take* (· t F) ((P T F) M))])

(define-metafunction Net
  update-outputs : t S -> S
  [(update-outputs t ((P T F) M))
   (give* (outputs t F) ((P T F) M))])

(define-metafunction Net
  take* : (p ...) S -> S
  [(take* () S) S]
  [(take* (p p_r ...) S)
   (take* (p_r ...) (take p S))])

(define-metafunction Net
  give* : (p ...) S -> S
  [(give* () S) S]
  [(give* (p p_r ...) S)
   (give* (p_r ...) (give p S))])

(define-metafunction Net
  take : p S -> S
  [(take p (PN ((p_0 nat_0) ...
                (p nat)
                (p_2 nat_2) ...)))
   (PN ((p_0 nat_0) ...
        (p ,(sub1 `nat))
        (p_2 nat_2) ...))])

(define-metafunction Net
  give : p S -> S
  [(give p (PN ((p_0 nat_0) ...
                (p nat)
                (p_2 nat_2) ...)))
   (PN ((p_0 nat_0) ...
        (p ,(add1 `nat))
        (p_2 nat_2) ...))])
#|
(define -->
  (reduction-relation
   Net
   #:domain S
   (-->
    (PN M)
    (PN M_*)
    (where (P (□_f ... t □_r ...) F) PN)
    (side-condition `(enabled? S t))
    (where M* (update t (update-inputs t M))))))



(define-judgment-form Net
  #:contract (--> M t M)
  #:mode     (--> I O)
  [(where (P (□_f ... t □_r ...) F) PN)
   (-->t (PN M) t (PN M_*))
   ----------------
   (--> (PN M) (PN M_*))])

(define-judgment-form Net
  #:contract (-->σ M σ M)
  #:mode     (-->σ I O)

  [-------------------
   (-->σ M () M)]
  [(-->t M □ M_**)
   (-->σ M (t ...) M_**)
   -------------------
   (-->σ M (□ t ...) M_*)])

(define-judgment-form Net
  #:contract (-->* M M)
  #:mode     (-->* I I)
  [(where (t ...)
          ;TODO computing this is hard
          )
   (-->σ M (t ...) M_*)
   -------------------
   (-->* M M_*)])



(define-metafunction Net
  update-inputs : t S -> S
  )

(define-metafunction Net
  update : t S -> S
  )

(define-metafunction enabled?
  (S))


;; TODO test live
;; TODO test bounded

(define-metafunction Net
  elementary? : path PN -> boolean)
|#
