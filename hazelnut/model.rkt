#lang racket

(require redex)

(define-language H
  (τ ::= (-> τ τ)
         num
         ())
  (e ::=  x
          (λ (x) e)
          (e e)
          n
          (+ e e)
          (: e τ)
          ()
          (e))

  (n ::= number)
  (x ::= variable-not-otherwise-mentioned)

  ;; Environment
  (Γ ::= ·
         ([x τ] Γ)))

(define-extended-language Z H
  (τ^ ::= (▹ τ ◃)
          (-> τ^ τ)
          (-> τ τ^))

  (e^ ::= (▹ e ◃)
          (λ (x) e^)
          (e^ e)
          (e e^)
          (+ e^ e)
          (+ e e^)
          (: e^ τ)
          (: e τ^)
          (e^))
  (eτ^ ::= e^ τ^)
  (eτ ::= e τ))

(define-extended-language HZ Z
  ;; Actions
  (α ::= (move δ)
         (construct ψ)
         del
         finish)

  ;; Dir
  (δ ::= (child n*)
         parent)

  (n* ::= 1 2)

  ;; Shape
  (ψ ::= arrow
         num
         asc
         (var x)
         (lam x)
         ap
         (lit n)
         plus
         nehole)

  (α* ::= ·
          (α α*)))


(define-metafunction HZ
  lookup : Γ x -> τ or #f
  [(lookup · x) #f]
  [(lookup ([x τ] Γ) x) τ]
  [(lookup ([x_* τ] Γ) x) (lookup Γ x)])

(module+ test
  (test-equal (term (lookup ([x num] ([y ()] ·)) x)) (term num))
  (test-equal (term (lookup ([x num] ([y ()] ·)) y)) (term ()))
  (test-equal (term (lookup ([x num] ([y ()] ·)) z)) #f)
  (test-equal (term (lookup · x)) #f))

(define-metafunction HZ
  extend : Γ [x τ] -> Γ
  [(extend Γ [x τ]) ([x τ] Γ)])

(module+ test
  (redex-check
   HZ
   (Γ x τ)
   (equal? (term (lookup (extend Γ [x τ]) x)) (term τ))))


(define-judgment-form HZ
  #:mode (I I I . ⇒ . O)
  #:contract (Γ ⊢ e . ⇒ . τ)
  [(Γ ⊢ e . ⇐ . τ)
   ----------------------------------------------------------------------------- "SAsc"
   (Γ ⊢ (: e τ) . ⇒ . τ)]
 
  [(where τ (lookup Γ x))
   -----------------------------------------------------------------------------  "SVar"
   (Γ ⊢ x . ⇒ . τ)]

  [(Γ ⊢ e_1 . ⇒ . τ_1)
   (τ_1 . ▶_→ . (-> τ_2 τ))
   (Γ ⊢ e_2 . ⇐ . τ_2)
   ----------------------------------------------------------------------------- "SAp"
   (Γ ⊢ (e_1 e_2) . ⇒ . τ)]

  [----------------------------------------------------------------------------- "SNum"
   (Γ ⊢ n . ⇒ . num)]

  [(Γ ⊢ e_1 . ⇐ . num)
   (Γ ⊢ e_2 . ⇐ . num)
   ----------------------------------------------------------------------------- "SPlus"
   (Γ ⊢ (+ e_1 e_2) . ⇒ . num)]

  [----------------------------------------------------------------------------- "SHole"
   (Γ ⊢ () . ⇒ . ())]

  [(Γ ⊢ e . ⇒ . τ)
   ----------------------------------------------------------------------------- "SNEHole"
   (Γ ⊢ (e) . ⇒ . ())])

(module+ test
  (test-judgment-holds (· ⊢ (: 1 num) . ⇒ . num))
  (test-judgment-holds (([x num] ·) ⊢ x . ⇒ . num))
  (test-judgment-holds (([x num] ·) ⊢ (: x num) . ⇒ . num))
  (test-judgment-holds (· ⊢ ((: (λ (x) 1) (-> num num)) 2) . ⇒ . num))
  (test-judgment-holds (· ⊢ 5 . ⇒ . num))
  (test-judgment-holds (([x num] ·) ⊢ (+ 3 x) . ⇒ . num))
  (test-judgment-holds (· ⊢ () . ⇒ . ()))
  (test-judgment-holds (([x num] ·) ⊢ ((: (λ (y) (+ x y)) (-> num num))) . ⇒ . ())))

(define-judgment-form HZ
  #:mode (I I I . ⇐ . I)
  #:contract (Γ ⊢ e . ⇐ . τ)
  [(Γ ⊢ e . ⇒ . τ_*)
   (τ . ∼ . τ_*)
   ----------------------------------------------------------------------------- "ASubsume"
   (Γ ⊢ e . ⇐ . τ)]
  [(τ . ▶_→ . (-> τ_1 τ_2))
   ((extend Γ [x τ_1]) ⊢ e . ⇐ . τ_2)
   ----------------------------------------------------------------------------- "ALam"
   (Γ ⊢ (λ (x) e) . ⇐ . τ)])

(module+ test
  (test-judgment-holds (· ⊢ 5 . ⇐ . ()))
  (test-judgment-holds (· ⊢ (λ (x) 5) . ⇐ . (-> num ()))))

(define-judgment-form HZ
  #:mode (I . ∼ . I)
  #:contract (τ . ∼ . τ)
  [----------------------------------------------------------------------------- "TCRefl"
   (τ . ∼ . τ)]
  [----------------------------------------------------------------------------- "TCHole1"
   (τ . ∼ . ())]
  [----------------------------------------------------------------------------- "TCHole2"
   (() . ∼ . τ)]
  [(τ_1 . ∼ . τ_1*)
   (τ_2 . ∼ . τ_2*)
   ----------------------------------------------------------------------------- "TCArr"
   ((-> τ_1 τ_2) . ∼ . (-> τ_1* τ_2*))])

(module+ test
  (test-judgment-holds (∼ num num))
  (test-judgment-holds (∼ num ()))
  (test-judgment-holds (∼ (-> (-> num num) (-> () num)) (-> () (-> num num)))))


(define-judgment-form HZ
  #:mode (I . ≁ . I)
  #:contract (τ . ≁ . τ)
  [----------------------------------------------------------------------------- "ICNumArr1"
   (num . ≁ . (-> τ_1 τ_2))]
  [----------------------------------------------------------------------------- "ICNumArr2"
   ((-> τ_1 τ_2) . ≁ . num)]
  [(τ_1 . ≁ . τ_3)
   ----------------------------------------------------------------------------- "ICArr1"
   ((-> τ_1 τ_2) . ≁ . (-> τ_3 τ_4))]
  [(τ_2 . ≁ . τ_4)
   ----------------------------------------------------------------------------- "ICArr2"
   ((-> τ_1 τ_2) . ≁ . (-> τ_3 τ_4))])

(module+ test
  (test-judgment-holds (≁ num (-> num num)))
  (test-judgment-holds (≁ num (-> () ())))
  (test-judgment-holds (≁ (-> num num) num))
  (test-judgment-holds (≁ (-> num ()) (-> (-> () ()) num))))


(define-judgment-form HZ
  #:mode (I . ▶_→ . O)
  #:contract (τ . ▶_→ . τ)
  [----------------------------------------------------------------------------- "MAHole"
   (() . ▶_→ . (-> () ()))]
  [----------------------------------------------------------------------------- "MAArr"
   ((-> τ_1 τ_2) . ▶_→ . (-> τ_1 τ_2))])

(module+ test
  (test-judgment-holds (▶_→ () (-> () ())))
  (test-judgment-holds (▶_→ (-> num num) (-> num num)))
  (test-judgment-holds (▶_→ (-> num (-> () num)) (-> num (-> () num)))))

(define-metafunction HZ
  erase : eτ^ -> eτ
  ;; types
  [(erase (▹ τ ◃)) τ]
  [(erase  (-> τ^ τ)) (-> (erase τ^) τ)]
  [(erase (-> τ τ^)) (-> τ (erase τ^))]
  ;; expressions
  [(erase (▹ e ◃)) e]
  [(erase (λ (x) e^)) (λ (x) (erase e^))]
  [(erase (e^ e)) ((erase e^) e)]
  [(erase (e e^)) (e (erase e^))]
  [(erase (+ e^ e)) (+ (erase e^) e)]
  [(erase (+ e e^)) (+ e (erase e^))]
  [(erase (: e^ τ)) (: (erase e^) τ)]
  [(erase (: e τ^)) (: e (erase τ^))]
  [(erase (e^)) ((erase e^))])

(module+ test
  (test-equal (term (erase (▹ num ◃))) (term num))
  (test-equal (term (erase (-> (▹ num ◃) ()))) (term (-> num ())))
  (test-equal (term (erase (-> () (-> num (▹ () ◃))))) (term (-> () (-> num ()))))
  (test-equal (term (erase (▹ 7 ◃))) (term 7))
  (test-equal (term (erase (λ (x) (▹ (+ x x) ◃)))) (term (λ (x) (+ x x))))
  (test-equal (term (erase ((λ (x) 1) (▹ 2 ◃)))) (term ((λ (x) 1) 2)))
  (test-equal (term (erase ((▹ (λ (x) 1) ◃) 2))) (term ((λ (x) 1) 2)))
  (test-equal (term (erase (+ (▹ x ◃) y))) (term (+ x y)))
  (test-equal (term (erase (+ y (▹ x ◃)))) (term (+ y x)))
  (test-equal (term (erase (: (▹ 5 ◃) num))) (term (: 5 num)))
  (test-equal (term (erase (: 5 (▹ () ◃)))) (term (: 5 ())))
  (test-equal (term (erase ((▹ 5 ◃)))) (term (5))))





(define-judgment-form HZ
  #:mode (--> I I O)
  #:contract (--> α eτ^ eτ^)
  ;; Type Actions

  ;; Type Movement
  [----------------------------------------------------------------------------- "TMArrChild1"
   (--> (move (child 1)) (▹ (-> τ_1 τ_2) ◃) (-> (▹ τ_1 ◃) τ_2))]
  [----------------------------------------------------------------------------- "TMArrChild2"
   (--> (move (child 2)) (▹ (-> τ_1 τ_2) ◃) (-> τ_1 (▹ τ_2 ◃)))]
  [----------------------------------------------------------------------------- "TMArrParent1"
   (--> (move parent) (-> (▹ τ_1 ◃) τ_2) (▹ (-> τ_1 τ_2) ◃))]
  [----------------------------------------------------------------------------- "TMArrParent2"
   (--> (move parent) (-> τ_1 (▹ τ_2 ◃)) (▹ (-> τ_1 τ_2) ◃))]

  ;; Type Deletion
  [----------------------------------------------------------------------------- "TMDel"
   (--> del (▹ τ ◃) (▹ () ◃))]

  ;; Type Construction
  [----------------------------------------------------------------------------- "TMConArrow"
   (--> (construct arrow) (▹ τ ◃) (-> τ (▹ () ◃)))]
  [----------------------------------------------------------------------------- "TMConNum"
   (--> (construct num) (▹ () ◃) (▹ num ◃))]

  ;; Zipper Cases
  [(--> α τ^ τ^_*)
   ----------------------------------------------------------------------------- "TMArrZip1"
   (--> α (-> τ^ τ) (-> τ^_* τ))]
  [(--> α τ^ τ^_*)
   ----------------------------------------------------------------------------- "TMArrZip2"
   (--> α (-> τ τ^) (-> τ τ^_*))]

  
  ;; Expression Movement Actions

  ;; Ascription
  [----------------------------------------------------------------------------- "EMAscChild1"
   (--> (move (child 1)) (▹ (: e τ) ◃) (: (▹ e ◃) τ))]
  [----------------------------------------------------------------------------- "EMAscChild2"
   (--> (move (child 2)) (▹ (: e τ) ◃) (: e (▹ τ ◃)))]
  [----------------------------------------------------------------------------- "EMAscParent1"
   (--> (move parent) (: (▹ e ◃) τ) (▹ (: e τ) ◃))]
  [----------------------------------------------------------------------------- "EMAscParent2"
   (--> (move parent) (: e (▹ τ ◃)) (▹ (: e τ) ◃))]

  ;; Lambda
  [----------------------------------------------------------------------------- "EMLamChild1"
   (--> (move (child 1)) (▹ (λ (x) e) ◃) (λ (x) (▹ e ◃)))]
  [----------------------------------------------------------------------------- "EMLamParent"
   (--> (move parent) (λ (x) (▹ e ◃)) (▹ (λ (x) e) ◃))]

  ;; Plus
  [----------------------------------------------------------------------------- "EMPlusChild1"
   (--> (move (child 1)) (▹ (+ e_1 e_2) ◃) (+ (▹ e_1 ◃) e_2))]
  [----------------------------------------------------------------------------- "EMPlusChild2"
   (--> (move (child 2)) (▹ (+ e_1 e_2) ◃) (+ e_1 (▹ e_2 ◃)))]
  [----------------------------------------------------------------------------- "EMPlusParent1"
   (--> (move parent) (+ (▹ e_1 ◃) e_2) (▹ (+ e_1 e_2) ◃))]
  [----------------------------------------------------------------------------- "EMPlusParent2"
   (--> (move parent) (+ e_1 (▹ e_2 ◃)) (▹ (+ e_1 e_2) ◃))]

  ;; Application
  [----------------------------------------------------------------------------- "EMApChild1"
   (--> (move (child 1)) (▹ (e_1 e_2) ◃) ((▹ e_1 ◃) e_2))]
  [----------------------------------------------------------------------------- "EMApChild2"
   (--> (move (child 2)) (▹ (e_1 e_2) ◃) (e_1 (▹ e_2 ◃)))]
  [----------------------------------------------------------------------------- "EMApParent1"
   (--> (move parent) ((▹ e_1 ◃) e_2) (▹ (e_1 e_2) ◃))]
  [----------------------------------------------------------------------------- "EMApParent2"
   (--> (move parent) (e_1 (▹ e_2 ◃)) (▹ (e_1 e_2) ◃))]

  ;; Non-Empty Hole
  [----------------------------------------------------------------------------- "EMNEHoleChild1"
   (--> (move (child 1)) (▹ (e) ◃) ((▹ e ◃)))]
  [----------------------------------------------------------------------------- "EMNEHoleParent"
   (--> (move parent) ((▹ e ◃)) (▹ (e) ◃))])

(module+ test
  (test-judgment-holds (--> (move (child 1)) (▹ (-> num num) ◃) (-> (▹ num ◃) num)))
  (test-judgment-holds (--> (move (child 2)) (▹ (-> num num) ◃) (-> num (▹ num ◃))))
  (test-judgment-holds (--> (move parent) (-> (▹ num ◃) num) (▹ (-> num num) ◃)))
  (test-judgment-holds (--> (move parent) (-> num (▹ num ◃)) (▹ (-> num num) ◃)))
  (test-judgment-holds (--> del (▹ (-> (-> num ()) ()) ◃) (▹ () ◃)))
  (test-judgment-holds (--> (construct arrow) (▹ num ◃) (-> num (▹ () ◃))))
  (test-judgment-holds (--> (construct num) (▹ () ◃) (▹ num ◃)))

  (redex-check
   HZ
   #:satisfying (--> α τ^_1 τ^_1*)
   (term-let ([τ_1 (generate-term HZ τ 5)])
     (judgment-holds (--> α (-> τ^_1 τ_1) (-> τ^_1* τ_1)))))

  (redex-check
   HZ
   #:satisfying (--> α τ^_1 τ^_1*)
   (term-let ([τ_1 (generate-term HZ τ 5)])
             (judgment-holds (--> α (-> τ_1 τ^_1) (-> τ_1 τ^_1*)))))

  (test-judgment-holds (--> (move (child 1)) (▹ (: 5 num) ◃) (: (▹ 5 ◃) num)))
  (test-judgment-holds (--> (move (child 2)) (▹ (: 5 num) ◃) (: 5 (▹ num ◃))))
  (test-judgment-holds (--> (move parent) (: (▹ 5 ◃) num) (▹ (: 5 num) ◃)))
  (test-judgment-holds (--> (move parent) (: 5 (▹ num ◃)) (▹ (: 5 num) ◃)))

  (test-judgment-holds (--> (move (child 1)) (▹ (λ (x) 5) ◃) (λ (x) (▹ 5 ◃))))
  (test-judgment-holds (--> (move parent) (λ (x) (▹ 5 ◃)) (▹ (λ (x) 5) ◃)))

  (test-judgment-holds (--> (move (child 1)) (▹ (+ 5 6) ◃) (+ (▹ 5 ◃) 6)))
  (test-judgment-holds (--> (move (child 2)) (▹ (+ 5 6) ◃) (+ 5 (▹ 6 ◃))))
  (test-judgment-holds (--> (move parent) (+ (▹ 5 ◃) 6) (▹ (+ 5 6) ◃)))
  (test-judgment-holds (--> (move parent) (+ 5 (▹ 6 ◃)) (▹ (+ 5 6) ◃)))

  (test-judgment-holds (--> (move (child 1)) (▹ ((λ (x) 1) 6) ◃) ((▹ (λ (x) 1) ◃) 6)))
  (test-judgment-holds (--> (move (child 2)) (▹ ((λ (x) 1) 6) ◃) ((λ (x) 1) (▹ 6 ◃))))
  (test-judgment-holds (--> (move parent) ((▹ (λ (x) 1) ◃) 6) (▹ ((λ (x) 1) 6) ◃)))
  (test-judgment-holds (--> (move parent) ((λ (x) 1) (▹ 6 ◃)) (▹ ((λ (x) 1) 6) ◃)))

  (test-judgment-holds (--> (move (child 1)) (▹ (5) ◃) ((▹ 5 ◃))))
  (test-judgment-holds (--> (move parent) ((▹ 5 ◃)) (▹ (5) ◃))))


;; NOTE: These seem a bit weird to write like this, because it feels like the
;; input is morally a separate judgment rather than arbitrary eτ^ terms
(define-judgment-form HZ
  #:mode (-->s I I I I O)
  #:contract (-->s α Γ ⊢ (e^ ⇒ τ ) (e^ ⇒ τ))
  ;; Movement
  [(--> (move δ) e^ e^_*)
   ----------------------------------------------------------------------------- "SAMove"
   (-->s (move δ) Γ ⊢ [e^ ⇒ τ] [e^_* ⇒ τ])]

  ;; Deletion
  [----------------------------------------------------------------------------- "SADel"
   (-->s del Γ ⊢ [(▹ e ◃) ⇒ τ] [(▹ () ◃) ⇒ ()])]

  ;; Construction
  [----------------------------------------------------------------------------- "SAConAsc"
   (-->s (construct asc) Γ ⊢ [(▹ e ◃) ⇒ τ] [(: e (▹ τ ◃)) ⇒ τ])]
  [(where τ (lookup Γ x))
   ----------------------------------------------------------------------------- "SAConVar"
   (-->s (construct (var x)) Γ ⊢ [(▹ () ◃) ⇒ ()] [(▹ x ◃) ⇒ τ])]
  [----------------------------------------------------------------------------- "SAConLam"
   (-->s (construct (lam x))
         Γ
         ⊢
         [(▹ () ◃) ⇒ ()]
         [(: (λ (x) ()) (-> (▹ () ◃) ())) ⇒ (-> () ())])]
  [(τ . ▶_→ . (-> τ_1 τ_2))
   ----------------------------------------------------------------------------- "SAConApArr"
   (-->s (construct ap) Γ ⊢ [(▹ e ◃) ⇒ τ] [(e (▹ () ◃)) ⇒ τ_2])]
  [(τ . ≁ . (-> () ()))
   ----------------------------------------------------------------------------- "SAConApOtw"
   (-->s (construct ap) Γ ⊢ [(▹ e ◃) ⇒ τ] [((e) (▹ () ◃)) ⇒ ()])]
  [----------------------------------------------------------------------------- "SAConNumLit"
   (-->s (construct (lit n)) Γ ⊢ [(▹ () ◃) ⇒ ()] [(▹ n ◃) ⇒ num])]
  [(τ . ∼ . num)
   ----------------------------------------------------------------------------- "SAConPlus1"
   (-->s (construct plus) Γ ⊢ [(▹ e ◃) ⇒ τ] [(+ e (▹ () ◃)) ⇒ num])]
  [(τ . ≁ . num)
   ----------------------------------------------------------------------------- "SAConPlus2"
   (-->s (construct plus) Γ ⊢ [(▹ e ◃) ⇒ τ] [(+ (e) (▹ () ◃)) ⇒ num])]
  [----------------------------------------------------------------------------- "SAConNEHole"
   (-->s (construct nehole) Γ ⊢ [(▹ e ◃) ⇒ τ] [((▹ e ◃)) ⇒ ()])]

  ;; Finishing
  [(Γ ⊢ e . ⇒ . τ_*)
   ----------------------------------------------------------------------------- "SAFinish"
   (-->s finish Γ ⊢ [(▹ (e) ◃) ⇒ ()] [(▹ e ◃) ⇒ τ_*])]

  ;; Zipper Cases
  [(-->a α Γ ⊢ e^ e^_* ⇐ τ)
   ----------------------------------------------------------------------------- "SAZipAsc1"
   (-->s α Γ ⊢ [(: e^ τ) ⇒ τ] [(: e^_* τ) ⇒ τ])]
  [(where τ_erased (erase τ^))
   (--> α τ^ τ^_*)
   (where τ_erased* (erase τ^_*))
   (Γ ⊢ e . ⇐ . τ_erased*)
   ----------------------------------------------------------------------------- "SAZipAsc2"
   (-->s α Γ ⊢ [(: e τ^) ⇒ τ_erased] [(: e τ^_*) ⇒ τ_erased*])]
  [(Γ ⊢ (erase e^) . ⇒ . τ_2)
   (-->s α Γ ⊢ [e^ ⇒ τ_2] [e^_* ⇒ τ_3])
   (τ_3 . ▶_→ . (-> τ_4 τ_5))
   (Γ ⊢ e . ⇐ . τ_4)
   ----------------------------------------------------------------------------- "SAZipApArr"
   (-->s α Γ ⊢ [(e^ e) ⇒ τ_1] [(e^_* e) ⇒ τ_5])]
  [(Γ ⊢ e . ⇒ . τ_2)
   (τ_2 . ▶_→ . (-> τ_3 τ_4))
   (-->a α Γ ⊢ e^ e^_* ⇐ τ_3)
   ----------------------------------------------------------------------------- "SAZipApAna"
   (-->s α Γ ⊢ [(e e^) ⇒ τ_1] [(e e^_*) ⇒ τ_4])]
  [(-->a α Γ ⊢ e^ e^_* ⇐ num)
   ----------------------------------------------------------------------------- "SAZipPlus1"
   (-->s α Γ ⊢ [(+ e^ e) ⇒ num] [(+ e^_* e) ⇒ num])]
  [(-->a α Γ ⊢ e^ e^_* ⇐ num)
   ----------------------------------------------------------------------------- "SAZipPlus2"
   (-->s α Γ ⊢ [(+ e e^) ⇒ num] [(+ e e^_*) ⇒ num])]
  [(Γ ⊢ (erase e^) . ⇒ . τ)
   (-->s α Γ ⊢ [e^ ⇒ τ] [e^_* → τ_*])
   ----------------------------------------------------------------------------- "SAZipHole"
   (-->s α Γ ⊢ [(e^) ⇒ ()] [(e^_*) ⇒ ()])])

(module+ test
  
  (test-judgment-holds (-->s (move (child 1)) · ⊢ [(▹ (+ 5 6) ◃) ⇒ num] [(+ (▹ 5 ◃) 6) ⇒ num]))
  (test-judgment-holds (-->s (move (child 2)) · ⊢ [(▹ (+ 5 6) ◃) ⇒ num] [(+ 5 (▹ 6 ◃)) ⇒ num]))
  (test-judgment-holds (-->s (move parent) · ⊢ [(+ (▹ 5 ◃) 6) ⇒ num] [(▹ (+ 5 6) ◃) ⇒ num]))
  (test-judgment-holds (-->s (move parent) · ⊢ [(+ 5 (▹ 6 ◃)) ⇒ num] [(▹ (+ 5 6) ◃) ⇒ num]))

  (test-judgment-holds (-->s del · ⊢ [(▹ 5 ◃) ⇒ num] [(▹ () ◃) ⇒ ()]))
  (test-judgment-holds (-->s (construct asc) · ⊢ [(▹ 5 ◃) ⇒ num] [(: 5 (▹ num ◃)) ⇒ num]))
  (test-judgment-holds (-->s (construct (var x)) ([x num] ·) ⊢ [(▹ () ◃) ⇒ ()] [(▹ x ◃) ⇒ num]))
  (test-judgment-holds
   (-->s (construct (lam x)) · ⊢  [(▹ () ◃) ⇒ ()] [(: (λ (x) ()) (-> (▹ () ◃) ())) ⇒ (-> () ())]))
  (test-judgment-holds
   (-->s (construct ap) ([f (-> num num)] ·) ⊢ [(▹ f ◃) ⇒ (-> num num)] [(f (▹ () ◃)) ⇒ num]))
  (test-judgment-holds
   (-->s (construct ap) · ⊢ [(▹ 5 ◃) ⇒ num] [((5) (▹ () ◃)) ⇒ ()]))
  (test-judgment-holds
   (-->s (construct (lit 5)) · ⊢ [(▹ () ◃) ⇒ ()] [(▹ 5 ◃) ⇒ num]))
  (test-judgment-holds
   (-->s (construct plus) ([x ()] ·) ⊢ [(▹ x ◃) ⇒ ()] [(+ x (▹ () ◃)) ⇒ num]))
  (test-judgment-holds
   (-->s (construct plus) ([f (-> num num)] ·) ⊢ [(▹ f ◃) ⇒ (-> num num)] [(+ (f) (▹ () ◃)) ⇒ num]))
  (test-judgment-holds
   (-->s (construct nehole) · ⊢ [(▹ 5 ◃) ⇒ num] [((▹ 5 ◃)) ⇒ ()]))
  (test-judgment-holds
   (-->s finish ([f (-> num num)] ·) ⊢ [(▹ ((f 5)) ◃) ⇒ ()] [(▹ (f 5) ◃) ⇒ num]))
  ;; Zipper cases: TODO
  )


;; NOTE: This seems like a reasonable mode, but it's hard to tell
;; from the paper if this is actually the right thing ...
(define-judgment-form HZ
  #:mode (-->a I I I I O I I)
  #:contract (-->a α Γ ⊢ e^ e^ ⇐ τ)

  ;; Subsumption
  [(Γ ⊢ (erase e^) . ⇒ . τ_*)
   (-->s α Γ ⊢ [e^ ⇒ τ_*] [e^_* ⇒ τ_**])
   (τ . ∼ . τ_**)
   ----------------------------------------------------------------------------- "AASubsume"
   (-->a α Γ ⊢ e^ e^_* ⇐ τ)]

  ;; Movement
  [(--> (move δ) e^ e^_*)
   ----------------------------------------------------------------------------- "AAMove"
   (-->a (move δ) Γ ⊢ e^ e^_* ⇐ τ)]

  ;; Deletion
  [----------------------------------------------------------------------------- "AADel"
   [-->a del Γ ⊢ (▹ e ◃) (▹ () ◃) ⇐ τ]]

  ;; Construction
  [----------------------------------------------------------------------------- "AAConAsc"
   (-->a (construct asc) Γ ⊢ (▹ e ◃) (: e (▹ τ ◃)) ⇐ τ)]
  [(where τ_* (lookup Γ x))
   (τ . ≁ . τ_*)
   ----------------------------------------------------------------------------- "AAConVar"
   (-->a (construct (var x)) Γ ⊢ (▹ () ◃) ((▹ x ◃)) ⇐ τ)]
  [(τ . ▶_→ . (-> τ_1 τ_2))
   ----------------------------------------------------------------------------- "AAConLam1"
   (-->a (construct (lam x)) Γ ⊢ (▹ () ◃) (λ (x) (▹ () ◃)) ⇐ τ)]
  [(τ . ≁ . (-> () ()))
   ----------------------------------------------------------------------------- "AAConLam2"
   (-->a (construct (lam x)) Γ ⊢ (▹ () ◃) ((: (λ (x) ()) (-> (▹ () ◃) ()))) ⇐ τ)]
  [(τ . ≁ . num)
   ----------------------------------------------------------------------------- "AAConNumLit"
   (-->a (construct (lit n)) Γ ⊢ (▹ () ◃) ((▹ n ◃)) ⇐ τ)]

  ;; Finishing
  [(Γ ⊢ e . ⇐ . τ)
   ----------------------------------------------------------------------------- "AAFinish"
   (-->a finish Γ ⊢ (▹ (e) ◃) (▹ e ◃) ⇐ τ)]

  ;; Zipper Cases
  [(τ . ▶_→ . (-> τ_1 τ_2))
   (-->a α (extend Γ [x τ_1]) ⊢ e^ e^_* ⇐ τ_2)
   ----------------------------------------------------------------------------- "AAZipLam"
   (-->a α Γ ⊢ (λ (x) e^) (λ (x) e^_*) ⇐ τ)])


(module+ test
  ;; subsumption : TODO

  (test-judgment-holds (-->a (move (child 1)) · ⊢ (▹ (+ 5 6) ◃) (+ (▹ 5 ◃) 6) ⇐ num))
  (test-judgment-holds (-->a (move (child 2)) · ⊢ (▹ (+ 5 6) ◃) (+ 5 (▹ 6 ◃)) ⇐ num))
  (test-judgment-holds (-->a (move parent) · ⊢ (+ (▹ 5 ◃) 6) (▹ (+ 5 6) ◃) ⇐ num))
  (test-judgment-holds (-->a (move parent) · ⊢ (+ 5 (▹ 6 ◃)) (▹ (+ 5 6) ◃) ⇐ num))

  (test-judgment-holds
   (-->a del · ⊢ (▹ 5 ◃) (▹ () ◃) ⇐ num))

  ;; Seems like the judgment that 5 checks against num is really a pre-condition here
  ;; otherwise the rule allows any type in its place
  (test-judgment-holds
   (-->a (construct asc) · ⊢ (▹ 5 ◃) (: 5 (▹ num ◃)) ⇐ num))
  
  (test-judgment-holds
   (-->a (construct (var x)) ([x num] ·) ⊢ (▹ () ◃) ((▹ x ◃)) ⇐ (-> num num)))
  (test-judgment-holds
   (-->a (construct (lam x)) · ⊢ (▹ () ◃) (λ (x) (▹ () ◃)) ⇐ (-> num num)))
  (test-judgment-holds
   (-->a (construct (lam x)) · ⊢ (▹ () ◃) ((: (λ (x) ()) (-> (▹ () ◃) ()))) ⇐ num))
  (test-judgment-holds
   (-->a (construct (lit 5)) · ⊢ (▹ () ◃) ((▹ 5 ◃)) ⇐ (-> num num)))
  (test-judgment-holds
   (-->a finish · ⊢ (▹ (5) ◃) (▹ 5 ◃) ⇐ num))

  ;; Zipper cases : TODO
  
  )


;; Iterated Action Judgments

(define-judgment-form HZ
  #:mode (-->* I I O)
  #:contract (-->* α* τ^ τ^)
  [----------------------------------------------------------------------------- "DoRefl"
   (-->* · τ^ τ^)]
  [(--> α τ^ τ^_*)
   (-->* α* τ^_* τ^_**)
   ----------------------------------------------------------------------------- "DoType"
   (-->* (α α*) τ^ τ^_**)])

(define-judgment-form HZ
  #:mode (-->*s I I I I O)
  #:contract (-->*s α* Γ ⊢ (e^ ⇒ τ) (e^ ⇒ τ))
  [----------------------------------------------------------------------------- "DoRefl"
   (-->*s · Γ ⊢ [e^ ⇒ τ] [e^ ⇒ τ])]
  [(-->s α Γ ⊢ [e^ ⇒ τ] [e^_* ⇒ τ_*])
   (-->*s α* Γ ⊢ [e^_* ⇒ τ_*] [e^_** ⇒ τ_**])
   ----------------------------------------------------------------------------- "DoSynth"
   (-->*s (α α*) Γ ⊢ [e^ ⇒ τ] [e^_** ⇒ τ_**])])

(define-judgment-form HZ
  #:mode (-->*a I I I I O I I)
  #:contract (-->*a α* Γ ⊢ e^ e^ ⇐ τ)
  [
   ; This rule looks weird, it shouldn't be true at any type
   ; seems like it needs a judgment as input
   ----------------------------------------------------------------------------- "DoRefl"
   (-->*a · Γ ⊢ e^ e^ ⇐ τ)]
  [(-->a α Γ ⊢ e^ e^_* ⇐ τ)
   (-->*a α* Γ ⊢ e^_* e^_** ⇐ τ)
   ----------------------------------------------------------------------------- "DoAna"
   (-->*a (α α*) Γ ⊢ e^ e^_** ⇐ τ)])

(define-judgment-form HZ
  #:mode (movements I)
  #:contract (movements α*)
  [----------------------------------------------------------------------------- "AM[]"
   (movements ·)]
  [(movements α*)
   ----------------------------------------------------------------------------- "AM::"
   (movements ((move δ) α*))])


(define-judgment-form HZ
  #:mode (action-sensibility-1-pre I I I I I I)
  #:contract (action-sensibility-1-pre Γ e^ τ α e^ τ)
  [(Γ ⊢ (erase e^) . ⇒ . τ)
   (-->s α Γ ⊢ [e^ ⇒ τ] [e^_* ⇒ τ_*])
   ----------------------------------------------------------------------------- "AS"
   (action-sensibility-1-pre Γ e^ τ α e^_* τ_*)])

(define-judgment-form HZ
  #:mode (action-sensibility-1-post I I I)
  #:contract (action-sensibility-1-post Γ e^ τ)
  [(Γ ⊢ (erase e^_*) . ⇒ . τ_*)
   ----------------------------------------------------------------------------- "AS"
   (action-sensibility-1-post Γ e^_* τ_*)])

#;
(redex-check
 HZ
 #:satisfying (action-sensibility-1-pre Γ e^ τ α e^_* τ_*)
 (begin
   (when #f
     (printf "Γ = ~a\n" (term Γ))
     (printf "α = ~a\n" (term α))
     (printf "e^ = ~a\n" (term e^))
     (printf "τ = ~a\n" (term τ))
     (printf "e^_* = ~a\n" (term e^_*))
     (printf "τ_* = ~a\n" (term τ_*))
     (printf "\n\n"))
   (judgment-holds (action-sensibility-1-post Γ e^_* τ_*)))
 #:attempts 1000)

(define-judgment-form HZ
  #:mode (action-sensibility-2-pre I I I I I)
  #:contract (action-sensibility-2-pre Γ e^ τ α e^_*)
  [(Γ ⊢ (erase e^) . ⇐ . τ)
   (-->a α Γ ⊢ e^ e^_* ⇐ τ)
   ----------------------------------------------------------------------------- "AS"
   (action-sensibility-2-pre Γ e^ τ α e^_*)])

(define-judgment-form HZ
  #:mode (action-sensibility-2-post I I I)
  #:contract (action-sensibility-2-post Γ e^ τ)
  [(Γ ⊢ (erase e^_*) . ⇐ . τ)
   ----------------------------------------------------------------------------- "AS"
   (action-sensibility-2-post Γ e^_* τ)])


;; This doesn't generate any (move (child n*)) terms ...
#;
(redex-check
 HZ
 #:satisfying (action-sensibility-2-pre Γ e^ τ α e^_*)
 (begin
   (when #f
     (printf "Γ = ~a\n" (term Γ))
     (printf "α = ~a\n" (term α))
     (printf "e^ = ~a\n" (term e^))
     (printf "τ = ~a\n" (term τ))
     (printf "e^_* = ~a\n" (term e^_*))
     (printf "τ_* = ~a\n" (term τ_*))
     (printf "\n\n"))
   (judgment-holds (action-sensibility-2-post Γ e^_* τ)))
 #:attempts 1000)

;; movement erasure invariance 1
#;
(redex-check
 HZ
 #:satisfying (--> (move δ) τ^ τ^_*)
 (begin
   (when #t
     (printf "τ^   = ~a\n" (term τ^))
     (printf "τ^_* = ~a\n" (term τ^_*))
     (printf "δ    = ~a\n" (term δ))
     (printf "\n\n"))
   (equal? (term (erase τ^)) (term (erase τ^_*)))))

(module+ test
  (test-results))

(define-metafunction HZ
  F : τ -> α*
  [(F num) ((construct num) ·)]
  [(F (-> τ_1 τ_2))
   (append
    (append
     ((construct arrow) (F τ_2))
     ((move parent) 
      ((move (child 1))
       (F τ_1))))
    ((move parent) ·))]
   
  [(F ()) ·])

(define-metafunction HZ
  append : α* α* -> α*
  [(append · α*) α*]
  [(append (α α*) α*_2)
   (α (append α* α*_2))])

(redex-check
 HZ
 τ
 (equal? (judgment-holds (-->* (F τ) (▹ () ◃) (▹ τ_1 ◃)) τ_1)
         (list (term τ))))















          
         