#lang racket

(provide make-task-gui-builder)

(require racket/gui)

;; build-task-gui : get-task-name get-task-text get-task-callback -> ((listof task?) -> void?)
(define ((make-task-gui-builder task-name task-text task-callback)
         tasks
         #:width [width 500]
         #:height [height 500]
         #:label [label "Task Manager"])
  (define the-frame
    (new frame% [label label] [width width] [height height]))

  (define processed-tasks (process-task-list tasks task-name task-text task-callback))
  (for ([(name task-details) (in-hash processed-tasks)])
    (define group-box
      (new group-box-panel%
           [label name]
           [parent the-frame]
           [alignment (list 'left 'top)]))
    (for ([task-detail (in-list task-details)])
      (match-define (cons text callback) task-detail)
      (new button%
           [label text]
           [parent group-box]
           [callback (λ (btn evt)
                       ;; call the callback then remove the button
                       ;; ASSUME no args to callback for now
                       (callback)
                       (send group-box delete-child btn))])))
  (send the-frame show #t))


;; process-task-list : (listof task?) get-task-name get-task-text get-task-callback
;;                     -> (hash/c string? (cons/c string? procedure?))
(define (process-task-list tasks task-name task-text task-callback)
  (for/fold ([task-hash (hash)])
            ([task (in-list tasks)])
    (define name (task-name task))
    (define text (task-text task))
    (define callback (task-callback task))
    (hash-update task-hash
                 name
                 (λ (lst) (cons (cons text callback) lst))
                 null)))
  