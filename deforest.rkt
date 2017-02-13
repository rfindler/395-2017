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
(define -:
  `(define :
     (λ (x)
       (λ (xs)
          (cons x xs)))))
(eval -: ns)
(define -foldr’
  `(define foldr’
     (λ (f)
        (λ (z)
           (λ (xs)
              (foldr
                (λ (a b) ((f a) b))
                z
                xs))))))
(eval -foldr’ ns)
(define -build
  `(define (build g) ((g :) '())))
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
  `(define from’
     (λ (a)
        (λ (b)
           (λ (c)
              (λ (n)
                 (if (> a b)
                   n
                   ((c a) ((((from’ (+ a 1)) b) c) n)))))))))
(eval -from’ ns)

;; Verify from’ is spiritually equal to from.
(check-true (eval `(andmap equal? (from 0 5) (build ((from’ 0) 5))) ns))

;; Nice! We can see that these things work, just like the paper said. (Whodathunk.)
;; Let's build the (build) stdlib.
(define -map’
  `(define map’
     (λ (f)
        (λ (xs)
           (build
             (λ (c)
                (λ (n)
                   (((foldr’ (λ (a) (λ (b) ((c (f a)) b)))) n) xs))))))))
(eval -map’ ns)

(check-equal? (eval `((map’ (λ (a) (+ a 1))) '(1 2 3)) ns) '(2 3 4))

(define -filter’
  `(define filter’
     (λ (f)
        (λ (xs)
           (build
             (λ (c)
                (λ (n)
                    (((foldr’
                        (λ (a)
                           (λ (b)
                              (if (f a)
                                ((c a) b)
                                b))))
                      n)
                     xs))))))))
(eval -filter’ ns)

(check-equal? (eval `((filter’ number?) '(1 "a" 3)) ns) '(1 3))

(define -++’
  `(define ++’
     (λ (xs)
        (λ (ys)
           (build
             (λ (c)
                (λ (n)
                   (((foldr’ c) (((foldr’ c) n) ys)) xs))))))))
(eval -++’ ns)

(check-equal? (eval `((++’ '(1 2)) '(3 4)) ns) '(1 2 3 4))

(define -concat’
  `(define concat’
     (λ (xs)
        (build
          (λ (c)
             (λ (n)
                (((foldr’
                    (λ (x)
                       (λ (y) (((foldr’ c) y) x))))
                    n)
                  xs)))))))
(eval -concat’ ns)

(check-equal? (eval `(concat’ '((1 2 3) (4 5 6))) ns) '(1 2 3 4 5 6))

;; Seems non-trivial to create an infinite list in Racket to mimic repeat.
;; Would I need Streams here? Not that important.
#| (define (repeat’ x)      |#
#|   (build (λ (c n) ...))) |#

#| (repeat’ 5)              |#

(define -zip’
  `(define zip’
     (λ (xs)
        (λ (ys)
           (build
             (λ (c)
                (λ (n)
                    (if (and (not (empty? xs)) (not (empty? ys)))
                      ((c (list (first xs) (first ys))) ((zip’ (rest xs)) (rest ys)))
                      n))))))))
(eval -zip’ ns)

(check-equal? (eval `((zip’ '(1 2 3)) '("a" "b" "c")) ns) '((1 "a") (2 "b") (3 "c")))

(define -nil’
  `(define nil’ (build (λ (c) (λ (n) n)))))
(eval -nil’ ns)

(define -cons’
  `(define cons’
     (λ (x)
        (λ (xs)
           (build
             (λ (c)
                (λ (n)
                   ((c x) (((foldr’ c) n) xs)))))))))
(eval -cons’ ns)

(check-equal? (eval `((cons’ 5) nil’) ns) '(5))

(define -cons’-nil’
  `(define (cons’ x nil’) (build (λ (c n) (c x n)))))

;; Verify loosely/informally that these behave more-or-less as expected.
(check-equal? (eval `((map’ -) '(1 2 3)) ns)
              '(-1 -2 -3))

(check-equal? (eval `((filter’ number?) '(1 2 "a" "b" 4 "c")) ns )
              '(1 2 4))

(check-equal? (eval `((++’ '(1 2)) '(3 4)) ns)
              '(1 2 3 4))

(check-equal? (eval `(concat’ '((1) (2 3) (4 5 6))) ns)
              '(1 2 3 4 5 6))

(check-equal? (eval `((zip’ '(1 2 3)) '("a" "b" "c")) ns)
              '((1 "a") (2 "b") (3 "c")))

(check-equal? (eval `nil’ ns)
              '())

(check-equal? (eval `((cons’ 5) '(4 3 2 1)) ns)
              '(5 4 3 2 1))

;; Now let's do some kind of actual work.
;; Convert unlines to use build-based library functions.
;; In Haskell, strings are lists, which means racket don't do that... So we fake it.
(define (unlines ls) (flatten (map (λ (l) (append l '("\n"))) ls)))
(define unlines-expr `(λ (ls) (flatten (map (λ (l) (append l '("\n"))) ls))))

(define ls '(("t" "h" "i" "s") () ("s" "u" "c" "K" "s")))
(check-equal? (unlines ls)
              '("t" "h" "i" "s" "\n" "\n" "s" "u" "c" "K" "s" "\n"))

;; flatten -> concat’
;; append  -> append’

(define (libfn->buildfn exp)
  (match exp
    [`(flatten ,xs) `(concat’ ,(libfn->buildfn xs))]
    [`(append ,xs ,ys) `((++’ ,(libfn->buildfn xs)) ,(libfn->buildfn ys))]
    [`(map ,f ,xs) `((map’ ,(libfn->buildfn f)) ,(libfn->buildfn xs))]
    ;; Missing cases for cons, zip, ...
    [`'(,elt) `((cons’ ,elt) nil’)]
    ;; Only works if original lambdas are at most 1 arg (so... potentially often not?).
    [`(λ (,arg) ,body) `(λ (,arg) ,(libfn->buildfn body))]
    [e e]))

(check-equal? (libfn->buildfn `(append l '("\n")))
              '((++’ l) ((cons’ "\n") nil’)))

;; Let's actually try to run it!
(letrec ([bexp (libfn->buildfn `(,unlines-expr ',ls))])
  (check-equal? (apply string-append (eval bexp ns)) "this\n\nsucKs\n"))
;; But hey, the transformation is (roughly) working on the body of unlines.

;; Let's try expanding buildfns using their bodies.

;; Oh man this is awful. What have I done.
(define -body third)

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
(check-equal? (unused-suffix-in `(λ (x) (+ x0 x)) 'x) 'x1)

(define (replace-exp exp val body)
  (match body
    [`(λ (,arg) ,lbody) #:when (equal? arg exp)
      ;; exp == argument; do no substitution.
      `(λ (,arg) ,lbody)]
    ;; This is the interesting case:
    [`(λ (,arg) ,lbody) #:when (and (symbol? exp) (not (not-in val arg)))
      (let ([new-arg (unused-suffix-in body arg)])
        `(λ (,new-arg) ,(replace-exp exp val (replace-exp arg new-arg lbody))))]
    [e #:when (equal? e exp) val]
    [(? list?) (map (curry replace-exp exp val) body)]
    ;; Right now, the only case activated by expand-buildfn is this last one. The dumb one.
    [e e]))
    ;[e (error 'replace-broke)]))

;; Don't do any renaming if you don't need to I.
(check-equal?
  (replace-exp 'x 5 `((λ (x) (+ x x)) x))
  `((λ (x) (+ x x)) 5))
;; Don't do any renaming if you don't need to II.
(check-equal?
  (replace-exp 'x '3 `(λ (y) (λ (y) (+ y x))))
  `(λ (y) (λ (y) (+ y 3))))
;; Don't do any renaming if you don't need to III.
(check-equal?
  (replace-exp 'a 7 `(λ (b0) ((λ (b) (+ b0 b a)) b0)))
  `(λ (b0) ((λ (b) (+ b0 b 7)) b0)))
;; Replace a free variable with a value.
(check-equal?
  (replace-exp 'x 4 `(λ (x0) (+ x x1)))
  `(λ (x0) (+ 4 x1)))
;; Do NOT replace a bound variable.
(check-equal?
  (replace-exp 'x 4 `(λ (x) (+ x x1)))
  `(λ (x) (+ x x1)))
;; We MIGHT replace y with x but x is bound, so rename.
(check-equal?
  (replace-exp 'y 'x `(λ (x) (+ x0 x)))
  `(λ (x1) (+ x0 x1)))
;; We WILL replace x with y but y is bound.
(check-equal?
  (replace-exp 'x 'y `(λ (y) (+ x y)))
  `(λ (y0) (+ y y0)))

;; Use this macro to define one rule at a time.
(define-syntax-rule (define-rule rule-name lhs rhs)
  (define (rule-name exp)
    (match exp
        [lhs rhs]
        [(? list?) (map rule-name exp)]
        [e e])))

(define-rule collapse-fold-build `(((foldr’ ,k) ,z) (build ,g)) `((,g ,k) ,z))
(check-equal? (collapse-fold-build `(((foldr’ +) 0) (build ,(-body -map’))))
              `((,(-body -map’) +) 0))

(define-rule collapse-fold-nil `(((foldr’ cons) '()) ',xs) xs)
(check-equal? (collapse-fold-nil `(((foldr’ cons) '()) '(a b c)))
              '(a b c))

;; Application with variable, occurrence where variable doesn't occur free in λ body.
;; Couldn't figure out how to pass the "variable free in λ body" test case with macro definition.
(define (β-reduction/constant exp)
  (match exp
        [`((λ (,a) ,b) ,x) #:when
                           (or (number? x)
                               (string? x)
                               (and (list? x) (equal? (first x) 'build))
                               (and (symbol? x) (not-in b x)))
          (replace-exp a x b)]
        [(? list?) (map β-reduction/constant exp)]
        [e e]))
;; Substitute simple argument with number.
(check-equal? (β-reduction/constant `((λ (y) (+ y y)) 5)) '(+ 5 5))
;; Substitute simple argument with free variable that isn't free in body.
(check-equal? (β-reduction/constant `((λ (y) (+ y y)) x)) '(+ x x))
;; Reduce outer lambda.
(check-equal? (β-reduction/constant `((λ (x) ((λ (y) (+ y y)) x)) 5)) '((λ (y) (+ y y)) 5))
;; Reduce outer and then inner lambda.
(check-equal? (β-reduction/constant
                (β-reduction/constant `((λ (x) ((λ (y) (+ y y)) x)) 5)))
              '(+ 5 5))
;; DON'T substitute if variable already occurs free in body.
(check-equal? (β-reduction/constant `((λ (a) (+ a b)) b))
              `((λ (a) (+ a b)) b))

(define-rule β-reduction/unsafe `((λ (,a) ,b) ,x) (replace-exp a x b))

;; Who needs efficiency?!
(define (expand-buildfn exp)
  (match exp
    [`(concat’ ,xs) `(,(-body -concat’) ,(expand-buildfn xs))]
    [`((++’ ,xs) ,ys) `((,(-body -++’) ,(expand-buildfn xs)) ,(expand-buildfn ys))]
    ;; Should expand-buildfn of f?
    [`((map’ ,f) ,xs) `((,(-body -map’) ,(expand-buildfn f)) ,(expand-buildfn xs))]
    [`((filter’ ,f) ,xs) `((,(-body -filter’) ,(expand-buildfn f))  ,(expand-buildfn xs))]
    [`((zip’ ,xs) ,ys) `((,(-body -zip’) ,(expand-buildfn xs))  ,(expand-buildfn ys))]
    [`((cons’ ,x) nil’) (-body -cons’-nil’)]
    [`((cons’ ,hd) ,tl) `((,(-body -cons’) ,(expand-buildfn hd)) ,(expand-buildfn tl))]
    [`(sum’ ,xs) `(,(-body -sum’) ,(expand-buildfn xs))]
    [`((from2 ,a) ,b) `((,(-body -from2) ,(expand-buildfn a)) ,(expand-buildfn b))]
    [e e]))

(check-equal? (expand-buildfn `(concat’ '((a b c) (d e f))))
              `((λ (xs)
                   (build (λ (c) (λ (n) (((foldr’ (λ (x) (λ (y) (((foldr’ c) y) x)))) n) xs)))))
                '((a b c) (d e f))))
(check-equal? (expand-buildfn `((++’ '(a b c)) '(d e f)))
              `(((λ
                   (xs)
                   (λ
                     (ys)
                     (build (λ (c) (λ (n) (((foldr’ c) (((foldr’ c) n) ys)) xs))))))
                 '(a b c))
                '(d e f)))
(check-equal? (expand-buildfn `((map’ (λ (x) (+ x 1))) '(1 2 3)))
              `(((λ
                   (f)
                   (λ
                     (xs)
                     (build (λ (c) (λ (n) (((foldr’ (λ (a) (λ (b) ((c (f a)) b)))) n) xs))))))
                 (λ (x) (+ x 1)))
                '(1 2 3)))
(check-equal? (expand-buildfn `((filter’ number?) '(1 "a" 2 "b" 3 "c")))
              `(((λ
                   (f)
                   (λ
                     (xs)
                     (build
                       (λ (c) (λ (n) (((foldr’ (λ (a) (λ (b) (if (f a) ((c a) b) b)))) n) xs))))))
                 number?)
                '(1 "a" 2 "b" 3 "c")))
(check-equal? (expand-buildfn `((zip’ '(a b c)) '(d e f)))
              `(((λ (xs)
                    (λ (ys)
                       (build
                         (λ (c)
                            (λ (n)
                               (if (and (not (empty? xs)) (not (empty? ys)))
                                 ((c (list (first xs) (first ys))) ((zip’ (rest xs)) (rest ys)))
                                 n))))))
                 '(a b c))
                '(d e f)))
(check-equal? (expand-buildfn `((cons’ a) '(b c)))
              `(((λ (x) (λ (xs) (build (λ (c) (λ (n) ((c x) (((foldr’ c) n) xs))))))) a) '(b c)))
(check-equal? (expand-buildfn `((cons’ a) nil’))
              `(build (λ (c n) (c x n))))
(check-equal? (expand-buildfn `(λ (a) (cons’ a '(1 2 3))))
              `(λ (a) (cons’ a '(1 2 3))))
(check-equal? (expand-buildfn `(+ x 1))
              `(+ x 1))
(check-equal? (eval (expand-buildfn `(concat’ '((a b c) (d e f)))) ns) '(a b c d e f))
(check-equal? (eval (expand-buildfn `((++’ '(a b c)) '(d e f))) ns) '(a b c d e f))
;; Same tests as above, but with (expand-buildfn) called on the exp first.
(check-equal? (eval (expand-buildfn `((map’ -) '(1 2 3))) ns) '(-1 -2 -3))
(check-equal? (eval (expand-buildfn `((filter’ number?) '(1 2 "a" "b" 4 "c"))) ns) '(1 2 4))
(check-equal? (eval (expand-buildfn `((++’ '(1 2)) '(3 4))) ns) '(1 2 3 4))
(check-equal? (eval (expand-buildfn `(concat’ '((1) (2 3) (4 5 6)))) ns) '(1 2 3 4 5 6))
(check-equal? (eval (expand-buildfn `((zip’ '(1 2 3)) '("a" "b" "c"))) ns)
              '((1 "a") (2 "b") (3 "c")))
(check-equal? (eval (expand-buildfn `nil’) ns) '())
(check-equal? (eval (expand-buildfn `((cons’ 5) '(4 3 2 1))) ns) '(5 4 3 2 1))

;; Make a command that does:
;; raco docs %: (word under cursor)

;; Keep running until a fixed point using a list of rules iterating over them.
;; ^ May not always terminate. (Depends on β-reductions.)

;; Loop: 1. expand 2. fold/build 3. fold/nil 4. β-reduction/constant
(define -from2
  `(define from2
     (λ (a)
        (λ (b)
           (build ((from’ a) b))))))
(eval -from2 ns)

(define -from3
  `(define from3
     (λ (a)
        (λ (b)
           (if (> a b)
             '()
             ((cons’ a) ((from3 (+ a 1)) b)))))))
(eval -from3 ns)

(check-equal? (eval `((from3 0) 5) ns) '(0 1 2 3 4 5))

(define -sum’
  `(define sum’
     (λ (ns)
        (((foldr’ (λ (a) (λ (b) (+ a b)))) 0) ns))))
(eval -sum’ ns)

(check-equal? (eval `((from2 0) 5) ns) '(0 1 2 3 4 5))
(check-equal? (eval `(sum’ ((from2 0) 5)) ns) 15)

(collapse-fold-build
  (β-reduction/constant
    (β-reduction/constant
      (β-reduction/constant
        (expand-buildfn `(sum’ ((from2 0) 5)))))))

(check-equal?
  (eval
    (collapse-fold-build
      (β-reduction/constant
        (β-reduction/constant
          (β-reduction/constant
            (expand-buildfn `(sum’ ((from2 0) 5)))))))
    ns)
  15)

;; Let's loop it.
(define (deforest-maybe exp)
  (collapse-fold-build
    (collapse-fold-nil
      (β-reduction/constant
        (expand-buildfn
          exp)))))

(define (deforest-fxpt exp)
  (let ([new-exp (deforest-maybe exp)])
    (if (equal? new-exp exp)
      exp
      (deforest-fxpt new-exp))))

(deforest-fxpt `(sum’ ((from2 0) 5)))

