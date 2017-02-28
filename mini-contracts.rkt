#lang racket/base
(require (for-syntax racket/base))


(define-values (prop:contract prop:contract? get-contract)
  (make-struct-type-property 'prop:contract))

(struct exn:fail:contract exn:fail ())

(struct ctc ())

(struct wrapper (fn number)
  #:property prop:procedure 0)

(define (wrapper-count x)
  (if (wrapper? x) (wrapper-number x) 0))

(struct flat ctc (pred?)
  #:property prop:contract
  (lambda (ctc val pos neg)
    (if ((flat-pred? ctc) val)
        val
        (raise (exn:fail:contract (format "flat contract error blaming: ~a" pos)
                             (current-continuation-marks))))))

(struct cons/c ctc (car cdr)
  #:property prop:contract
  (λ (ctc val pos neg)
    (unless (pair? val)
      (raise (exn:fail:contract (format "cons/c contract error: not a pair, blaming: ~a" pos)
                                (current-continuation-marks))))
    (define car-ctc (cons/c-car ctc))
    (define cdr-ctc (cons/c-cdr ctc))
    (cons (contract car-ctc (car val) pos neg)
          (contract cdr-ctc (cdr val) pos neg))))
    
(struct -> ctc (dom rng)
  #:property prop:contract
  (lambda (ctc val pos neg)
    (define dom-ctc (->-dom ctc))
    (define rng-ctc (->-rng ctc))
    (define count (add1 (if (wrapper? val) (wrapper-number val) 0)))
    (wrapper
     (lambda (x)
       (let ([x (contract dom-ctc x neg pos)])
         (contract rng-ctc (val x) pos neg)))
     count)))
    

(define (contract ctc val pos neg)
  ((get-contract ctc) ctc val pos neg))


;; Tests for flat and function contracts
(module+ test
  (require rackunit)
  (define int->int-fn
    (contract (-> (flat integer?) (flat integer?))
              (lambda (n) n)
              'pos
              'neg))
  (define int->int-fn-bad
    (contract (-> (flat integer?) (flat integer?))
              (lambda (n) "bad")
              'pos
              'neg))
  
  (check-exn (regexp "flat contract error blaming: neg")
             (λ () (int->int-fn "bad")))
  
  (check-exn (regexp "flat contract error blaming: pos")
             (λ () (int->int-fn-bad 0)))
  
  (check-equal? (int->int-fn 0) 0)
  
  (check-equal? (contract (flat integer?) 5 'pos 'neg) 5)
  
  (check-exn (regexp "flat contract error blaming: pos")
             (λ () (contract (flat string?) 5 'pos 'neg))))


;; Adding recursive contracts
;; This is basically just a simplified version
;; of the implementation in racket/contract 
;; This is needed for the exponential wrapping example

(define (force-recursive-contract ctc)
  (define current (recursive-contract-ctc ctc))
  (cond
    [(not current)
     (define thunk (recursive-contract-thunk ctc))
     (define forced-ctc (thunk))
     (set-recursive-contract-ctc! ctc forced-ctc)
     forced-ctc]
    [else current]))

(struct recursive-contract ctc ([thunk #:mutable] [ctc #:mutable])
  #:transparent
  #:constructor-name make-recursive-contract
  #:omit-define-syntaxes
  #:property prop:contract
  (lambda (ctc val pos neg) 
    (define r-ctc (force-recursive-contract ctc))
    (contract r-ctc val pos neg)))

;; macro wrapper for defining useful recursive contracts
(define-syntax (recursive-contract stx)
  (syntax-case stx ()
    [(_ arg)
     #'(make-recursive-contract (lambda () arg) #f)]))

(define bubble/c
  (recursive-contract
   (-> bubble/c bubble/c)))

(define (id x) x)

(define id-with-ctc 
  (contract bubble/c id 'pos 'neg))

(define (go n)
  (define start-time (current-milliseconds))
  (let loop ([id id-with-ctc] [n n])
    (cond
      [(= n 0)
       (printf "time since start: ~a\n" (- (current-milliseconds) start-time))
       (printf "Number of wrappers: ~a\n" (wrapper-count id))
       (printf "End\n")]
      [else
       (printf "time since start: ~a\n" (- (current-milliseconds) start-time))
       (printf "Number of wrappers: ~a\n" (wrapper-count id))
       (loop (id id)
             (sub1 n))])))

;(go 5)

;; Sample output:
;; > (go 5)
;; time since start: 0
;; Number of wrappers: 1
;; time since start: 0
;; Number of wrappers: 3
;; time since start: 0
;; Number of wrappers: 9
;; time since start: 0
;; Number of wrappers: 27
;; time since start: 0
;; Number of wrappers: 81
;; time since start: 1
;; Number of wrappers: 243
;; End

#;
(define f1
  (contract (-> (flat integer?) (flat integer?))
            (let ()
              (define (f x)
                (x "bad"))
              f)
            'p
            'n))
#;
(define f
  (contract
   (-> (flat integer?) (flat integer?))
   (λ (x) (f "bad"))
   'p
   'n))

(require (for-syntax racket/base))
(define-for-syntax enable-contracts #f)
(define-syntax (define/contract stx)
  (if enable-contracts 
      (syntax-case stx ()
        [(_ (fun arg) ctc blame-p blame-n exprs ...)
         #'(define fun
             (contract ctc
                       (let ()
                         (define (fun arg) exprs ...)
                         fun)
                       blame-p
                       blame-n))]
        [(_ ((fun arg) arg2) ctc blame-p blame-n exprs ...)
         #'(define fun
             (contract ctc
                       (let ()
                         (define ((fun arg) arg2) exprs ...)
                         fun)
                       blame-p
                       blame-n))])
      (syntax-case stx ()
        [(_ thing ctc blame-p blame-n exprs ...)
         #'(define thing exprs ...)])))

(provide (all-defined-out))



