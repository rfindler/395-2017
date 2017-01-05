#lang racket
(require redex)

(define-language L
  (e ::=
     v
     (not e)
     (concat e ...)
     (or e e)
     (and e e)
     (xor e e)
     (+ e e))

  (b ::= 0 1)
  
  (v ::= (b ...))
  
  (E ::=
     hole
     (not E)
     (or v E)
     (or E e)
     (and v E)
     (and E e)
     (concat v ... E e ...)
     (xor E e)
     (xor v E)
     (+ v E)
     (+ E e)))

(define red
  (reduction-relation
   L
   (--> (in-hole E (concat (b ...) ...))
        (in-hole E (b ... ...))
        "concat")

   (--> (in-hole E (not (0)))
        (in-hole E (1))
        "not 0")
   (--> (in-hole E (not (1)))
        (in-hole E (0))
        "not 1")
   (--> (in-hole E (not ()))
        (in-hole E ())
        "not empty")

   (--> (in-hole E (not (b_1 b_2 b_3 ...)))
        (in-hole E (concat (not (b_1))
                           (not (b_2))
                           (not (b_3)) ...)) 
        "not N")

   
   (--> (in-hole E (and () ()))
        (in-hole E ()))

   (--> (in-hole E (and () (b_1 b_2 ...)))
        (in-hole E ()))
   (--> (in-hole E (and (b_1 b_2 ...) ()))
        (in-hole E ()))
   
   (--> (in-hole E (and (b_2 ... 0)
                        (b_4 ... 0)))
        (in-hole E (concat (and (b_2 ...) (b_4 ...))
                           (0)))
        "and 00")
   (--> (in-hole E (and (b_2 ... 1)
                        (b_4 ... 0)))
        (in-hole E (concat (and (b_2 ...) (b_4 ...))
                           (0)))
        "and 10")
   (--> (in-hole E (and (b_2 ... 0)
                        (b_4 ... 1)))
        (in-hole E (concat (and (b_2 ...) (b_4 ...))
                           (0)))
        "and 01")
   (--> (in-hole E (and (b_2 ... 1)
                        (b_4 ... 1)))
        (in-hole E (concat (and (b_2 ...) (b_4 ...))
                           (1)))
        "and 11")))

(test-->> red
          (term (not (1 0 1)))
          (term (0 1 0)))

(redex-check
 L
 (b ...)
 (equal? (apply-reduction-relation* red (term (not (not (b ...)))))
         (list (term (b ...)))))

(redex-check
 L
 (b ...)
 (let ([what-we-got
        (apply-reduction-relation*
         red
         (term (and (b ...)
                    (0 0 0 0 0))))])
   (and (= 1 (length what-we-got))
        (for/and ([b (in-list (list-ref what-we-got 0))])
          (equal? b 0)))))

(test-results)

(traces
 red
 (term (and (0 1 0 1) (1 1 1 1 1 1 0 0))))
