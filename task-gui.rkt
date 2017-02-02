#lang racket

(provide make-task-gui-builder)

(require racket/gui)

;; build-task-gui : get-task-name get-task-text resolve-task! -> ((listof task?) -> void?)
(define ((make-task-gui-builder task-name task-text resolve-task)
         tasks
         #:width [width 500]
         #:height [height 500]
         #:label [label "Task Manager"])
  (define the-frame
    (new frame% [label label] [width width] [height height]))

  (define the-panel
    (new vertical-panel% [parent the-frame] [style '(auto-vscroll auto-hscroll)]))

  (define processed-tasks (process-task-list tasks task-name task-text))
  (for ([(name task-details) (in-hash processed-tasks)])
    (define group-box
      (new group-box-panel%
           [label name]
           [parent the-panel]
           [alignment (list 'left 'top)]))
    (for ([task-detail (in-list task-details)])
      (match-define (cons text task) task-detail)
      (new button%
           [label text]
           [parent group-box]
           [callback (λ (btn evt)
                       ;; call the callback then remove the button
                       ;; ASSUME no additional args to resolve-task for now
                       (resolve-task task)
                       (send group-box delete-child btn))])))
  (send the-frame show #t))


;; process-task-list : (listof task?) get-task-name get-task-text get-task-callback
;;                     -> (hash/c string? (cons/c string? procedure?))
(define (process-task-list tasks task-name task-text)
  (for/fold ([task-hash (hash)])
            ([task (in-list tasks)])
    (define name (task-name task))
    (define text (task-text task))
    (hash-update task-hash
                 name
                 (λ (lst) (cons (cons text task) lst))
                 null)))
  