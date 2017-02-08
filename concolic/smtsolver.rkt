#lang racket

(provide
 current-solver-path
 solve)

(define (decompile-smt v)
  (define (do-decompile cxt v)
    (match v
      [(? (λ (k) (and (symbol? k) (dict-has-key? cxt k))))
       (dict-ref cxt v)]
      [(? number? v) v]
      [`(- ,(? number? v)) (- v)]
      ['true #t]
      ['false #f]
      [`(let ([,vars ,exprs] ...) ,v)
       (do-decompile
        (for/fold ([cxt cxt]) ([var vars] [expr exprs])
          (dict-set cxt var (do-decompile cxt expr)))
        v)]))
  (do-decompile (make-immutable-hash) v))

(define current-solver-path (make-parameter (find-executable-path "/usr/local/bin/z3")))

(define (solve vars smts0)
  (define smts
    `(,@(dict-map vars (λ (var type)
                         `(declare-const ,var (,type))))
      ,@(map (λ (smt) `(assert ,smt)) smts0)))
  (define-values (proc solverout solverin no-err-pipe)
    (subprocess #f #f 'stdout (current-solver-path) "-in"))
  (define (cleanup)
    (when solverout (close-input-port solverout))
    (when solverin (close-output-port solverin))
    (subprocess-kill proc #t))
  (define (sendsolver dat)
    (displayln dat solverin)
    (flush-output solverin))
  (define (recvsolver)
    (read solverout))
  (with-handlers ([exn:fail? (λ (e)
                               (cleanup)
                               (raise e))])
    (for-each sendsolver smts)
    (sendsolver '(check-sat))
    (define has-sol (recvsolver))
    (cond
      [(equal? has-sol 'sat)
       (sendsolver '(get-model))
       (define model (recvsolver))
       (cleanup)
       (match model
         [`(model
            (define-fun ,var () ,typ ,val)
            ...)
          (values (make-immutable-hash (map cons var (map decompile-smt val)))
                  (make-immutable-hash (map cons var typ)))]
         [else (error 'solve-smt (format "cannot parse model ~a; input: ~a" model smts))])]
      [(equal? has-sol 'unsat)
       (cleanup)
       (values #f #f)]
      [else
       (error 'solve-smt (format "unknown result '~a'; input: ~a" has-sol smts))])))

(module+ test
  (require rackunit)

  (define-syntax values->list
    (syntax-rules ()
      [(_ e) (call-with-values (thunk e) list)]))

  (check-equal?
   (values->list
    (solve
     '((x . Int) (y . Int))
     '((> y x)
       (> x y))))
   '(#f #f))

  (check-equal?
   (values->list
    (solve
     '((x . Int) (y . Int))
     '((= x (* 2 y))
       (= x (+ 10 y)))))
   (list (hash 'x 20 'y 10) (hash 'x 'Int 'y 'Int))))
