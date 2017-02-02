#lang racket
;; the language

;; string string (any ... -> any)
(struct task (name text callback) #:transparent)

(define tasks empty)

;; string string (any ... -> any) -> void
(define (add-task! name text)
  (define callback (λ () (printf "resolving task: ~a\n for patient: ~a\n\n" text name)))
  (set! tasks
        (cons (task name text callback) tasks)))

;; task any ... -> void
(define (resolve-task! task . values)
  (set! tasks (remove task tasks))
  (apply (task-callback task) values))

;; the library

(define (urgent-hypertension! patient when-creat when-neuro)
  (add-task! patient (~a "order check creatinine " when-creat))
  (add-task! patient "check blood pressure")
  (add-task! patient "order check K+")
  (add-task! patient "current hypertension drug check")
  (add-task! patient (~a "neurology check for hypertension " when-neuro))
  ;;and more
  )

;; the patient

(urgent-hypertension! "jones" "tomorrow morning" "Stat")
(add-task! "jones" "order CT angiogram of kidneys")
(add-task! "jones" "review current literature on diagnosing hyperaldosteronism")
(add-task! "jones" "order 2g sodium diet")
(add-task! "jones" "consult nephrology")

;; A second patiend
(urgent-hypertension! "smith" "tomorrow morning" "Stat")
(add-task! "smith" "order CT angiogram of kidneys")
(add-task! "smith" "review current literature on diagnosing hyperaldosteronism")
(add-task! "smith" "order 2g sodium diet")
(add-task! "smith" "consult nephrology")

(urgent-hypertension! "foo" "tomorrow morning" "Stat")
(add-task! "foo" "order CT angiogram of kidneys")
(add-task! "foo" "review current literature on diagnosing hyperaldosteronism")
(add-task! "foo" "order 2g sodium diet")
(add-task! "foo" "consult nephrology")


(urgent-hypertension! "bar" "tomorrow morning" "Stat")
(add-task! "bar" "order CT angiogram of kidneys")
(add-task! "bar" "review current literature on diagnosing hyperaldosteronism")
(add-task! "bar" "order 2g sodium diet")
(add-task! "bar" "consult nephrology")

(urgent-hypertension! "baz" "tomorrow morning" "Stat")
(add-task! "baz" "order CT angiogram of kidneys")
(add-task! "baz" "review current literature on diagnosing hyperaldosteronism")
(add-task! "baz" "order 2g sodium diet")
(add-task! "baz" "consult nephrology")

;; the plan

;; before next class: add a gui thing
;; during next class: how to conceptualize and write loops and conditionals
(require "task-gui.rkt")

;; setup gui-builder
(define run-task-manager
  (make-task-gui-builder task-name task-text resolve-task!))

(run-task-manager tasks)

;; Maybe there should be a separate list of patients to show patients with no available tasks
