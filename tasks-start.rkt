#lang racket
;; the language

;; string string (any ... -> any)
(struct task (name text callback) #:transparent)

(define tasks empty)

;; string string (any ... -> any) -> void
(define (add-task! name text)
  (set! tasks
        (cons (task name text void) tasks)))

;; task (listof any) -> void
(define (resolve-task! task values)
  (set! tasks (remove task tasks))
  (apply (task-callback task) values))

;; the library

(define (urgent-hypertension! patient when-creat when-neuro)
  (add-task! patient (~a "order check creatinine" when-creat))
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

;; the plan

;; before next class: add a gui thing
;; during next class: how to conceptualize and write loops and conditionals

tasks