#lang debug mf-apply racket
(require redex)
(require rackunit)
(module+ test)
(define-syntax quasiquote (make-rename-transformer #'term))

(define-language Littler
  (e ::=
     N b x (lambda x e) (e ...) (if e e e)
     op
     (let x e e))
  (N ::= ((n ^ t) α))
  (α ::= · !)
  (op ::= pi not cos sin arccos arcsin + - * / =)
  ;; values
  (v w ::= (n ^ t) s b (lambda x e) op)
  (t ::= l (! l) (op t ...))
  (l ::= variable-not-otherwise-mentioned natural)
  (x ::= variable-not-otherwise-mentioned)
  (n k ::= number)
  (L ::= {l ...})
  (b ::= true false)
  ;; value contexts
  (V ::= (• n) (n ^ t) b (lambda x e))
  (ρ ::= {(l n) ...})
  #:binding-forms
  (lambda x e_b #:refers-to x))

(define-metafunction Littler
  Vsubst : V v n -> V
  [(Vsubst (• n) v n) v]
  [(Vsubst (• n) v n_2) (• n)]
  [(Vsubst (n ^ t) v n_2) (n ^ t)]
  [(Vsubst b v n_2) b]
  [(Vsubst (lambda x e) v n_2) (lambda x e)])


(define-judgment-form Littler
  #:contract (⇓ e v)
  #:mode     (⇓ I O)
  [----------
   (⇓ v v)]

  [----------
   (⇓ ((n ^ t) ·) (n ^ t))]

  [----------
   (⇓ ((n ^ l) !) (n ^ (! l)))]

  [(⇓ e_1 op)
   (⇓ e (n_v ^ t_v)) ...
   (where n (δ (op n_v ...)))
   (where t (op t_v ...))
   -----------------------------------
   (⇓ (e_1 e ...) (n ^ t))]

  [(⇓ e_1 op)
   (⇓ e (n_v ^ t_v)) ...
   (where b (δ (op n_v ...)))
   (where t (op t_v ...))
   -----------------------------------
   (⇓ (e_1 e ...) b)]

  [(⇓ e_1 (lambda x e_b))
   (⇓ e_2 (n_v ^ t_v))
   (⇓ (substitute e_b x ((n_v ^ t_v) ·)) v)
   -----------------------------------
   (⇓ (e_1 e_2) v)]

  [(⇓ ((lambda x e_2) e_1) v)
   -----------------------------------
   (⇓ (let x e_1 e_2) v)]

  [(⇓ e_1 true)
   (⇓ e_2 v)
   -----------------------------------
   (⇓ (if e_1 e_2 e_3) v)]

  [(⇓ e_1 false)
   (⇓ e_3 v)
   -----------------------------------
   (⇓ (if e_1 e_2 e_3) v)])


(define-metafunction Littler
  δ : (op n ...) -> n or b
  [(δ (pi)) ,pi]
  [(δ (not true)) false]
  [(δ (not false)) true]
  [(δ (cos n)) ,(cos `n)]
  [(δ (sin n)) ,(sin `n)]
  [(δ (arccos n)) ,(acos `n)]
  [(δ (arcsin n)) ,(asin `n)]
  [(δ (+ n_1 n_2)) ,(+ `n_1 `n_2)]
  [(δ (- n_1 n_2)) ,(- `n_1 `n_2)]
  [(δ (* n_1 n_2)) ,(* `n_1 `n_2)]
  [(δ (/ n_1 n_2)) ,(/ `n_1 `n_2)]
  [(δ (= n_1 n_2)) ,(if (= `n_1 `n_2) `true `false)])

(module+ test
  (test-judgment-holds
   (⇓ ((1 ^ x) !) (1 ^ (! x))))

  (test-judgment-holds
   (⇓ ((1 ^ x) ·) (1 ^ x)))

  (check-true (redex-match? Littler e `(+ ((1 ^ y) ·) ((1 ^ x) ·))))
  (test-judgment-holds
   (⇓ (+ ((1 ^ y) ·) ((1 ^ x) ·))
      (2 ^ (+ y x))))

  (test-judgment-holds
   (⇓
    (= ((2 ^ h) !) ((2 ^ j) !))
    true))
  (test-judgment-holds
   (⇓
    (if (= ((2 ^ h) !) ((2 ^ h) !)) true false)
    true))
  (test-judgment-holds
   (⇓ (let k (+ ((1 ^ x) ·) ((1 ^ y) ·))
        (if (= k ((2 ^ h) !)) (+ k ((12 ^ g) !)) ((5 ^ q) !)))
      (14 ^ (+ (+ x y) (! g))))))

(define-metafunction Littler
  Tr : (n ^ t) -> t
  [(Tr (n ^ t)) t])

(define-metafunction Littler
  Locs : t -> L
  [(Locs l) {l}]
  [(Locs (! l)) {}]
  [(Locs (op t ...))
   ,(set->list (list->set `(l ... ...)))
   (where ({l ...} ...) ((Locs t) ...))])

(define-metafunction Littler
  ComputeMapping : e -> ρ
  [(ComputeMapping b) {}]
  [(ComputeMapping op) {}]
  [(ComputeMapping ((n ^ l) α)) {(l ↦ n)}]
  [(ComputeMapping (e ...))
   {(l ↦ n) ... ...}
   (where ({(l ↦ n) ...} ...)
          ((ComputeMapping e) ...))]
  [(ComputeMapping (if e ...))
   {(l ↦ n) ... ...}
   (where ({(l ↦ n) ...} ...)
          ((ComputeMapping e) ...))]
  [(ComputeMapping (let x e ...))
   {(l ↦ n) ... ...}
   (where ({(l ↦ n) ...} ...)
          ((ComputeMapping e) ...))]
  [(ComputeMapping (lambda x e))
   (ComputeMapping e)])

(define-judgment-form Littler
  #:contract (synthesize-plausable ρ {(n = t) ...}  ρ)
  #:mode     (synthesize-plausable I I              O)
  [(where (L ...) (#{Locs t} ...))
   (members L l) ...
   (where (k ...) (#{Solve ρ_0 l (n = t)} ...))
   (where ρ_2  #{map-set* ρ_0 (l k) ...})
   -----------------------------------
   (synthesize-plausable ρ_0 {(n = t) ...} ρ_2)])

(define-judgment-form Littler
  #:contract (synthesize-faithful ρ {(n = t) ...}  ρ)
  #:mode     (synthesize-faithful I I              O)
  [(where (L ...) (#{Locs t} ...))
   (where (L_* ...) #{filter-common (L ...) ()})
   (members L_* l) ...
   (where (k ...) (#{Solve ρ_0 l (n = t)} ...))
   (where ρ_2  #{map-set* ρ_0 (l k) ...})
   -----------------------------------
   (synthesize-faithful ρ_0 {(n = t) ...} ρ_2)])

(define-metafunction Littler
  filter-common : (L ...) (L ...) -> (L ...)
  #;TODO
  )

(define-judgment-form Littler
  #:contract (members L l)
  #:mode     (members I O)
  [---------------------
   (members (l_0 ... l_1 l_2 ...) l_1)])

(define-metafunction Littler
  map-set* : ρ  (l k) ... -> ρ
  [(map-set* ρ) ρ]
  [(map-set* ρ (l_h k_h) (l k) ...)
   (map-set* #{map-set ρ l_h k_h} (l k) ...)])
(define-metafunction Littler
  map-set : ρ l k -> ρ
  [(map-set ((l_0 k_0) ... (l any) (l_2 k_2) ...) l k)
   ((l_0 k_0) ... (l k) (l_2 k_2) ...)])
(define-metafunction Littler
  map-ref : ρ l -> n
  [(map-ref (any_0 ... (l n) any_1 ...) l) n])

(define-extended-language Littler/PO Littler
  (PO ::= l (+ PO PO)))

(define-metafunction Littler/PO
  Solve : ρ l (n = t) -> n
  [(Solve ρ l (n = PO))
   (Solve_A ρ l (n = PO))]
  [(Solve ρ l (n = t))
   k
   (where k #{Solve_B ρ l (n = t)})])

;; TODO random tests for when Solve_A and Solve_B differ

(define-metafunction Littler
  Solve_A : ρ l (n = t) -> n
  [(Solve_A ρ l (n = t))
   ,(/ (- `n `n_s) `n_c)
   (where (n_c n_s) #{WalkPlus ρ l t})])

(define-metafunction Littler
  WalkPlus : ρ l t -> (n n)
  [(WalkPlus ρ l l) (1 0)]
  [(WalkPlus ρ l l_2) (0 #{map-ref ρ l_2})]
  [(WalkPlus ρ l (+ t_1 t_2))
   (,(+ `n_c1 `n_c2) ,(+ `n_s1 `n_s2))
   (where (n_c1 n_s1) #{WalkPlus ρ l t_1})
   (where (n_c2 n_s2) #{WalkPlus ρ l t_2})])

(define-metafunction Littler
  Solve_B : ρ l (n = t) -> n
  [(Solve_B ρ l (n = l)) n]
  [(Solve_B ρ l (n = (op t)))
   (Solve_B ρ l (#{Inv op n} = t))]
  [(Solve_B ρ l (n = (op t_1 t_2)))
   #{Solve_B ρ l (#{InvL op n_1 n} = t_2)}
   (where (l_1 ... l l_2 ...) #{Locs t_2})
   (where n_1 #{t-eval ρ t_1})]
  [(Solve_B ρ l (n = (op t_1 t_2)))
   #{Solve_B ρ l (#{InvR op n_2 n} = t_1)}
   (where (l_1 ... l l_2 ...) #{Locs t_1})
   (where n_2 #{t-eval ρ t_2})])

;; try-eval
(define-metafunction Littler
  t-eval : ρ t -> n
  [(t-eval ρ l) #{map-ref ρ l}]
  [(t-eval ρ (! l)) #{map-ref ρ l}]
  [(t-eval ρ (op t ...))
   #{δ (op n ...)}
   (where (n ...) (#{t-eval ρ t} ...))])
;; Inv
(define-metafunction Littler
  Inv : op n -> n
  [(Inv cos n) ,(acos `n)]
  [(Inv sin n) ,(asin `n)]
  [(Inv acos n) ,(cos `n)]
  [(Inv asin n) ,(sin `n)])
;; InvR
(define-metafunction Littler
  InvR : op n n -> n
  [(InvR + n_2 n) ,(- `n `n_2)]
  [(InvR - n_2 n) ,(+ `n_2 `n)]
  [(InvR * n_2 n) ,(/ `n `n_2)]
  [(InvR / n_2 n) ,(* `n `n_2)])
;; InvL
(define-metafunction Littler
  InvL : op n n -> n
  [(InvL + n_1 n) ,(- `n `n_1)]
  [(InvL - n_1 n) ,(- `n_1 `n)]
  [(InvL * n_1 n) ,(/ `n `n_1)]
  [(InvL / n_1 n) ,(/ `n_1 `n)])


(define-check (synth2 ρ n=t n=t2)
    (for-each
     (lambda (s)
       (define v1 `#{t-eval ,s ,(last n=t)})
       (define v2 `#{t-eval ,s ,(last n=t2)})
       (unless (and (= v1 (first n=t))
                    (= v2 (first n=t2)))
         (fail-check (~a
                      #:separator " "
                      s
                      `(= ,v1 ,(first n=t))
                      `(= ,v2 ,(first n=t2))))))
     (judgment-holds
      (synthesize-plausable ,ρ {,n=t ,n=t2} any)
      any)))

(define-check (synth ρ n=t)
    (for-each
     (lambda (s)
       (define v `#{t-eval ,s ,(last n=t)})
       (unless (= v (first n=t))
         (fail-check (~a s `(= ,v ,(first n=t))))))
     (judgment-holds
      (synthesize-plausable ,ρ {,n=t} any)
      any)))

(module+ test
  

  (synth
   `((x 1) (y 1) (g 12))
   `(11 = (+ (+ x y) (! g))))
  (synth
   `((x 1) (y 1) (g 12))
   `(11 = (+ (- x y) (! g))))

  (synth
   `((x 1) (y 1) (g 12))
   `(11 = (+ (* x y) (! g))))

  (synth
   `((x 1) (y 1) (g 12))
   `(11 = (+ (/ x y) (! g))))

  ;; fails
  (synth2
   `((x 1) (y 1) (g 12))
   `(11 = (+ (/ x y) (! g)))
   `(3 = x))

  ;; fails
  (synth2
   `((x 1) (y 1) (g 12))
   `(3 = x)
   `(11 = (+ (/ x y) (! g))))

  ;; fails
  (synth2
   `((y 1) (x 1) (g 12))
   `(3 = x)
   `(11 = (+ (/ x y) (! g))))

  (synth2
   `((z 4) (x 1) (y 1) (g 12))
   `(3 = (+ z (! g)))
   `(11 = (+ (/ x y) (! g))))

  ;; fails
  (synth2
   `((z 4) (x 1) (y 1) (g 12))
   `(3 = (+ z g))
   `(11 = (+ (/ x y) g))))
