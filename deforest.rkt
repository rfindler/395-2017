#lang racket

(require rackunit)

;; NOTE(jordan): codepoints:
;; ’ 2019
;; β 03b2

;; Racket namespace nonsense...
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

;; Let's write up the example optimized "all" for practice.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (all’ p xs)
  (if (empty? xs) #t
      (and (p (first xs)) (all’ p (rest xs)))))

(define (all p xs) (andmap p xs))

(check-true (and (all number? '(5 5 5)) (all’ number? '(5 5 5))))

;; Okay next up. Let's define build.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define -build
  `(define (build g) (g cons '())))
(eval -build ns)
;; ^^ Essentially: partial application of g with cons and '().!

;; Let's do the from example.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; from
(define (from a b)
  (if (> a b)
    '()
    (cons a (from (+ a 1) b))))

;; from’
(define -from’
  `(define (from’ a b)
    (λ (c n)
       (if (> a b)
         n
         (c a ((from’ (+ a 1) b) c n))))))
(eval -from’ ns)

;; Verify from’ is spiritually equal to from.
(check-true (eval `(andmap equal? (from 0 5) (build (from’ 0 5))) ns))

;; Nice! We can see that these things work, just like the paper said. (Whodathunk.)
;; Let's build the (build) stdlib.
(define -map’
  `(define (map’ f xs)
    (build (λ (c n) (foldr (λ (a b) (c (f a) b)) n xs)))))
(eval -map’ ns)

(define -filter’
  `(define (filter’ f xs)
    (build (λ (c n) (foldr (λ (a b) (if (f a) (c a b) b)) n xs)))))
(eval -filter’ ns)

(define -++’
  `(define (++’ xs ys)
    (build (λ (c n) (foldr c (foldr c n ys) xs)))))
(eval -++’ ns)

(define -concat’
  `(define (concat’ xs)
    (build (λ (c n) (foldr (λ (x y) (foldr c y x)) n xs)))))
(eval -concat’ ns)

;; Seems non-trivial to create an infinite list in Racket to mimic repeat.
;; Would I need Streams here? Not that important.
#| (define (repeat’ x)      |#
#|   (build (λ (c n) ...))) |#

#| (repeat’ 5)              |#

(define -zip’
  `(define (zip’ xs ys)
    (build (λ (c n) (if (and (not (empty? xs)) (not (empty? ys)))
                        (c `(,(first xs) ,(first ys)) (zip’ (rest xs) (rest ys)))
                        n)))))
(eval -zip’ ns)

(define -nil’
  `(define nil’ (build (λ (c n) n))))
(eval -nil’ ns)

(define -cons’
  `(define (cons’ x xs) (build (λ (c n) (c x (foldr c n xs))))))
(eval -cons’ ns)

(define -cons’-nil’
  `(define (cons’ x nil’) (build (λ (c n) (c x n)))))

;; Verify loosely/informally that these behave more-or-less as expected.
(check-equal? (eval `(map’ - '(1 2 3)) ns)
              '(-1 -2 -3))

(check-equal? (eval `(filter’ number? '(1 2 "a" "b" 4 "c")) ns )
              '(1 2 4))

(check-equal? (eval `(++’ '(1 2) '(3 4)) ns)
              '(1 2 3 4))

(check-equal? (eval `(concat’ '((1) (2 3) (4 5 6))) ns)
              '(1 2 3 4 5 6))

(check-equal? (eval `(zip’ '(1 2 3) '("a" "b" "c")) ns)
              '((1 "a") (2 "b") (3 "c")))

(check-equal? (eval `nil’ ns)
              '())

(check-equal? (eval `(cons’ 5 '(4 3 2 1)) ns)
              '(5 4 3 2 1))

;; Now let's do some kind of actual work.
;; Convert unlines to use build-based library functions.
;; In Haskell, strings are lists, which means racket don't do that... So we fake it.
(define (unlines ls) (flatten (map (λ (l) (append l '("\n"))) ls)))

(define ls '(("t" "h" "i" "s") () ("s" "u" "c" "K" "s")))
(check-equal? (unlines ls)
              '("t" "h" "i" "s" "\n" "\n" "s" "u" "c" "K" "s" "\n"))

;; flatten -> concat’
;; append  -> append’

(define (libfn->buildfn exp)
  (match exp
    [`(flatten ,xs) `(concat’ ,(libfn->buildfn xs))]
    [`(append ,xs ,ys) `(++’ ,(libfn->buildfn xs) ,(libfn->buildfn ys))]
    [`(map ,f ,xs) `(map’ ,(libfn->buildfn f) ,(libfn->buildfn xs))]
    [`'(,elt) `(cons’ ,elt nil’)]
    [`(λ ,args ,body) `(λ ,args ,(libfn->buildfn body))]
    [e e]))

(check-equal? (libfn->buildfn `(append l '("\n")))
              '(++’ l (cons’ "\n" nil’)))

;; Let's actually try to run it!
(letrec ([bexp (libfn->buildfn `(flatten (map (λ (l) (append l '("\n"))) ',ls)))])
  (check-equal? (apply string-append (eval bexp ns)) "this\n\nsucKs\n"))
;; But hey, the transformation is (roughly) working on the body of unlines.

;; Let's try expanding buildfns using their bodies.

;; Oh man this is awful. What have I done.
(define -body third)

;; This must be incomplete. But anyway.
;; This needs to be capture-avoiding substitution. (See whiteboard photo.)
;; Application with variable, occurrence where variable doesn't occur free in λ body.
;; Add more of their examples and make sure they work.
;; Replace all free occurrences of exp with val in body.
(define (symbol-add-suffix s1 s2)
  (string->symbol (string-append (symbol->string s1) (~a s2))))

(check-equal? (symbol-add-suffix 'a 5) 'a5)

(define (not-in exp var)
  (match exp
    [(? list?) (andmap (λ (e) (not-in e var)) exp)]
    [e (not (equal? e var))]))

(check-false (not-in 'x 'x))
(check-false (not-in '(λ (x) (+ x 17)) 'x))
(check-true (not-in 'y 'x))
(check-true (not-in '(λ (x) (λ (y) (+ x y))) 'z))

(define (unused-suffix-in e x)
  (for/first ([suf (stream-cons "" (in-naturals))]
             #:when (not-in e (symbol-add-suffix x suf)))
    (symbol-add-suffix x suf)))

(check-equal? (unused-suffix-in `(x x0 x1 x2 x3) 'x) 'x4)
(check-equal? (unused-suffix-in `(x0 x1 x2 x3) 'x) 'x)
(check-equal? (unused-suffix-in `(x0 x1 x2 x3) 'x0) 'x00)

(define (replace-exp exp val body)
  (match body
    [`(λ (,arg) ,lbody) #:when (equal? arg exp)
      ;; exp == argument; do no substitution.
      `(λ (,arg) ,lbody)]
    ;; This is the interesting case:
    [`(λ (,arg) ,lbody) #:when (symbol? exp)
      (let ([new-arg (unused-suffix-in `(,exp ,val) arg)])
        `(λ (,new-arg) ,(replace-exp exp val (replace-exp arg new-arg lbody))))]
    [e #:when (equal? e exp) val]
    [(? list?) (map (curry replace-exp exp val) body)]
    ;; Right now, the only case activated by expand-buildfn is this last one. The dumb one.
    [e e]))
    ;[e (error 'replace-broke)]))

(check-equal?
  (replace-exp 'x 5 `((λ (x) (+ x x)) x))
  `((λ (x) (+ x x)) 5))
(check-equal?
  (replace-exp 'x '3 `(λ (y) (λ (y) (+ y x))))
  `(λ (y) (λ (y) (+ y 3))))
(check-equal?
  (replace-exp 'a 7 `(λ (b0) ((λ (b) (+ b0 b a)) b0)))
  `(λ (b0) ((λ (b) (+ b0 b 7)) b0)))
(check-equal?
  (replace-exp 'x 4 `(λ (x0) (+ x x1)))
  `(λ (x0) (+ 4 x1)))
;; I feel like I am missing an edge case. (Or two. Or three.) But I don't know which.

;; Who needs efficiency?!
(define (expand-buildfn exp)
  (match exp
    [`(concat’ ,xs) (replace-exp 'xs
                                 (expand-buildfn xs)
                                 (-body -concat’))]
    [`(++’ ,xs ,ys) (replace-exp 'ys
                                 (expand-buildfn ys)
                                 (replace-exp 'xs (expand-buildfn xs) (-body -++’)))]
    [`(map’ ,f ,xs) (replace-exp 'f
                                 (expand-buildfn f)
                                 (replace-exp 'xs (expand-buildfn xs) (-body -map’)))]
    ;; God this is sickening.
    [`(cons’ ,x nil’) (replace-exp 'x x (-body -cons’-nil’))]
    [`(λ ,args ,body) `(λ ,(expand-buildfn args) ,(expand-buildfn body))]
    [e e]))

(check-equal? (eval (expand-buildfn `(concat’ '((a b c) (d e f)))) ns) '(a b c d e f))
(check-equal? (eval (expand-buildfn `(++’ '(a b c) '(d e f))) ns) '(a b c d e f))

;; Completely expand our implementation of `unlines`.
(pretty-print (expand-buildfn (libfn->buildfn `(flatten (map (λ (l) (append l '("\n"))) ls)))))

;; Use this macro to define one rule at a time.
(define-syntax-rule (define-rule rule-name lhs rhs)
  (define (rule-name exp)
    (match exp
        [lhs rhs]
        [(? list?) (map rule-name exp)]
        [e e])))

(define-rule collapse-fold-build `(((foldr ,k) ,z) (build ,g)) `((,g ,k) ,z))
(check-equal? (collapse-fold-build `(((foldr +) 0) (build ,(-body -map’))))
              `((,(-body -map’) +) 0))

(define-rule collapse-fold-nil `(foldr cons '() ',xs) xs)
(check-equal? (collapse-fold-nil `(foldr cons '() '(a b c)))
              '(a b c))

(define-rule β-reduction/constant
             `((λ (,a) ,b) ,(or (? number? x) (? symbol? x)))
             (replace-exp a x b))
(check-equal? (β-reduction/constant `((λ (y) (+ y y)) 5)) '(+ 5 5))
(check-equal? (β-reduction/constant `((λ (y) (+ y y)) x)) '(+ x x))
(check-equal? (β-reduction/constant `((λ (x) ((λ (y) (+ y y)) x)) 5)) '((λ (y) (+ y y)) 5))
(check-equal? (β-reduction/constant
                (β-reduction/constant `((λ (x) ((λ (y) (+ y y)) x)) 5)))
              '(+ 5 5))

;; Keep running until a fixed point using a list of rules iterating over them.
;; ^ May not always terminate. (Depends on β-reductions.)

