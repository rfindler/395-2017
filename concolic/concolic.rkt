#lang racket

(require "smtsolver.rkt")
(require (prefix-in r: "redredex.rkt"))
(require (prefix-in s: "symstore.rkt"))
(require (prefix-in p: "symbolic.rkt"))
(require redex)

(struct model (language reduction-relation extract-result pretty-printer))
(define symstore/m (model s:M s:conc s:concres s:pp))
(define redredex/m (model r:M r:conc r:concres r:pp))
(define symbolic/m (model p:M p:conc p:concres p:pp))

(define current-model
  (make-parameter redredex/m))

; normal evaluation
(define ex1
  (term
   (< (+ 5 (* 2 3)) 23)))

(define (run-ex1 #:gui [gui #t])
  (reduce ex1 '() gui))

; show how symbolic expression works
(define ex2
  (term
   (= (+ (* sa sa) (* sb sb)) (* sc sc))))

(define (run-ex2 [sa 5] [sb 12] [sc 13] #:gui [gui #t])
  (reduce ex2 `((sa . ,sa) (sb . ,sb) (sc . ,sc)) gui))

; show how symbolic conditions are turned into assertions
(define ex3
  (term
   (if (= sx 5)
       1
       (if (< sy 11)
           2
           3))))

(define (run-ex3 [sx 4] [sy -1] #:gui [gui #t])
  (reduce ex3 `((sx . ,sx) (sy . ,sy)) gui))

; examples to see how symbolic expressions flow through functions
(define ex4
  (term
   ((λ (abs)
      ((λ (f k) (if (= (f (f sx)) (* k 9)) 417 sy))
       (λ (n) (+ n 8))
       (* 2 (abs sy))))
    (λ (m) (if (< m 0) (* m -1) m)))))

(define (run-ex4 [sx 2001] [sy 0] #:gui [gui #t])
  (reduce ex4 `((sx . ,sx) (sy . ,sy)) gui))

; examples that lifts if out of if
(define ex5
  (term
   (if (if (< st 32)
           #t
           (if (< 212 st)
               #t
               #f))
       67
       #f)))

(define (run-ex5 [st 63] #:gui [gui #t])
  (reduce ex5 `((st . ,st)) gui))

; examples to see branch blows up
(define exif
  (term
   (if (if (if sx sf st)
           sa
           sb)
       sp
       sq)))

(define (run-exif [sx #f] [st #t] [sf #f] [sa #t] [sb #t] [sp #f] [sq #t] #:gui [gui #t])
  (reduce exif `((sx . ,sx) (st . ,st) (sf . ,sf)
                            (sa . ,sa) (sb . ,sb)
                            (sp . ,sp) (sq . ,sq)) gui))

; (factorial 5), collatz, etc
(define fact
  (term
   ((λ factorial (n)
      (if (= n 1) 1
          (* n (factorial (+ n -1)))))
    sm)))

(define (run-fact [sm 3] #:gui [gui #t])
  (reduce fact `((sm . ,sm)) gui))

(define collatz
  (term
   (if (< 0 sn)
       (if (< sn 20)
           ((λ collatz (n len)
              (if (= n 1) len
                  (collatz
                   (if (= (mod n 2) 0)
                       (div n 2)
                       (+ (* 3 n ) 1))
                   (+ len 1))))
            sn 0)
           -1)
       -2)))

(define (run-collatz [sn 10] #:gui [gui #t])
  (reduce collatz `((sn . ,sn)) gui))

(define fizzbuzz
  (term
   (if (= 0 (mod sn 15))
       4048924757
       (if (= 0 (mod sn 5))
           45141
           (if (= 0 (mod sn 3))
               61781
               3735928559)))))

(define (run-fizzbuzz [sn 52] #:gui [gui #t])
  (reduce fizzbuzz `((sn . ,sn)) gui))

(define fizzbuzz2
  (term
   (if (= 0 (mod sn 5))
       (if (= 0 (mod sn 3))
           4048924757
           45141)
       (if (= 0 (mod sn 3))
           (if (= 0 (mod sn 5))
               4048924757
               61781)
           3735928559))))

(define (run-fizzbuzz2 [sn 30] #:gui [gui #t])
  (reduce fizzbuzz2 `((sn . , sn)) gui))

(define ex-misc
  (term
   ((λ (let)

      ((let (λ (x) (+ x 1)))
       (λ (add1)

         ((let (λ (x) (* 2 x)))
          (λ (double)

            (+ 3
               (if (= (+ sw 10) (double (add1 sw)))
                   1
                   0)))))))

    (λ (f) (λ (body) (body f))))))

(define (run-ex-misc [sw 13] #:gui [gui #t])
  (reduce ex-misc `((sw . ,sw)) gui))

; symvals: (listof (cons/c symbol? exact-integer?))
; gui : (or/c #f 'all #t)
(define (reduce trm symvals [gui #t])
  (define conc
    (model-reduction-relation (current-model)))
  (define pp
    (model-pretty-printer (current-model)))
  (define m
    (term (,trm ,(dict-map symvals (λ (key val) `(,key ↦ ,val / ,key))) (pc=))))
  (when (equal? gui #t) (stepper conc m pp))
  (when (equal? gui 'all) (traces conc m #:pp pp))
  (apply-reduction-relation* conc m))

; When there aren't any solution, random results are generated
; When there is a solution, vals is used as the base case
(define (initialize vars vals solved path-constraints)
  (cond
    [(set-empty? path-constraints)
     (values
      (map (λ (_) (- (random 1 42) 21)) vars)
      solved
      path-constraints)]

    [else
     (define-values (pc pcs...)
       (values (set-first path-constraints)
               (set-rest path-constraints)))

     (define solved^ (set-add solved pc))

     (define smts
       (map (match-lambda
              [`(assert ,s) s]
              [`(assert-! ,s) (list 'not s)])
            pc))

     (define-values (solution type)
       (solve (map (λ (var) (cons var 'Int)) vars) smts))

     (if solution
         (values
          (map (λ (var val) (dict-ref solution var val)) vars vals)
          solved^
          pcs...)
         (initialize vars vals solved^ pcs...))]))

(define/match (negate-last pc)
  [(`(,pc ... (assert ,s)))   `(,@pc (assert-! ,s))]
  [(`(,pc ... (assert-! ,s))) `(,@pc (assert ,s))])

; vars: (listof symbol?)
(define (run* trm vars)
  (let loop ([vals #f] [solved (set)] [path-constraints (set)])
    (define-values (vals^ solved^ path-constraints^)
      (initialize vars vals solved path-constraints))

    (display ">>>")
    (for ([var vars] [val^ vals^])
      (printf " ~a = ~a" var val^))
    (newline)

    (match-define `((,v (,e ,Γ (pc= ,pc ...))) . ,_)
      (apply-reduction-relation*
       (model-extract-result (current-model))
       (first (reduce trm (map cons vars vals^) #f))))

    (printf "e = ~a (~a)\npc = ~a\n\n"
            v
            e pc)

    (define path-constraints^^
      (let add-prefixes ([pc pc] [pcs path-constraints^])
        (define pc/negated
          (or (null? pc) (negate-last pc)))
        (cond
          [(null? pc) pcs]
          [(set-member? solved pc/negated)
           (add-prefixes (drop-right pc 1) pcs)]
          [else
           (add-prefixes
            (drop-right pc 1)
            (set-add pcs pc/negated))])))

    (unless (set-empty? path-constraints^^)
      (loop vals^ solved^ path-constraints^^))))
