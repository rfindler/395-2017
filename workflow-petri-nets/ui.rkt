#lang at-exp racket/gui
(provide ui)
(require "model.rkt" redex/reduction-semantics pict)
(define (ui S #:draw [draw "circo"])
  (define current-state (box S))
  (define init-pict (state->pict S draw))
  (define W (exact-ceiling (pict-width init-pict)))
  (define H (exact-ceiling (pict-height init-pict)))
  (define-values (W** H**) (get-display-size))
  (define W* (exact-ceiling (* 0.8 W**)))
  (define H* (exact-ceiling (* 0.8 H**)))
  (define (scale* s)
    (define minrat
      (min
       (if (> W W*)
           (/ W* W)
           1)
       (if (> H H*)
           (/ H* H)
           1)))
    (scale s minrat))
  (define current-pict (box (scale* init-pict)))
  (define es
    (let ([es (make-eventspace)])
      (define base
        (new frame%
             [width (min W W**)]
             [height (min H H**)]
             [label "net"]))
      (define vert
        (new vertical-panel%
             [parent base]
             [style '(auto-vscroll auto-hscroll)]))
      (define pict-container
        (new canvas%
             [parent vert]
             [min-width (min W W*)]
             [min-height (min H H*)]
             [paint-callback
              (lambda (p dc)
                (send dc erase)
                (send dc draw-bitmap
                      (pict->bitmap (unbox current-pict))
                      0
                      0))]))
      (define button-container
        (new horizontal-panel%[parent vert]))
      (define (new-state! S)
        (set-box! current-state S)
        (set-box! current-pict (scale* (state->pict S draw)))
        (send pict-container refresh)
        (send button-container change-children (const empty))
        (buttons))
      (define (buttons)
        (define S (unbox current-state))
        (for/list ([t (in-list (all-enabled S))])
          (new button%
               [label (trim-label t)]
               [parent button-container]
               [callback
                (lambda (b c)
                  (new-state!
                   (first (judgment-holds (-->t ,S ,t S)
                                          S))))])))
      (new-state! S)
      (send base reflow-container)
      (send base show #t)
      es))
  (sync es))


(define (all-enabled S)
  (match-define (list (list P T F) M) S)
  (for/list ([t (in-list T)]
             #:when (term (enabled? ,S ,t)))
    t))

(define (state->pict S draw)
  (define graph (PN->graph (first S) (second S)))
  (define dot (graph->dot graph))
  (render-dot dot draw))

(struct graph (nodes edges) #:transparent)
(struct node (label shape dots enabled?) #:transparent)
(struct edge (from to) #:transparent)

(define (PN->graph pn m)
  (define PS
    (for/list ([p (first pn)])
      (node (trim-label p)
            'circle
            (second (or (assoc p m) (list '_ 0)))
            #f)))
  (define TS
    (for/list ([t (second pn)])
      (node (trim-label t)
            'square
            0
            (term (enabled? (,pn ,m) ,t)))))
  (define ES
    (for/list ([e (third pn)])
      (edge (trim-label (first e))
            (trim-label (second e)))))
  (graph (append PS TS) ES))

(define (trim-label s)
  (apply string
         (for/list ([c (in-string (substring (~a s) 2))])
           (case c
             [(#\-) #\_]
             [(#\*) #\K]
             [(#\/) #\_]
             [else c]))))

(define (graph->dot g)
  (apply string-append
         @list{strict digraph petrie {{ @(nodes->dot (graph-nodes g))}
                              @(edges->dot (graph-edges g))}}))

(define (nodes->dot nodes)
  (apply string-append (map node->dot nodes)))
(define (node->dot n)
  (match-define (node label shape dots enabled?) n)
  (define color
    (cond [enabled? "color=lightblue;style=\"filled\""]
          [(not (zero? dots))
            "color=pink;style=\"filled\""]
          [else ""]))
  @~a{@~a["\"" label "\""] [ "shape"=@shape @";" "label"=@~a[label "_" dots] @";"@color] @";"})
(define (edges->dot edges)
  (apply string-append (map edge->dot edges)))
(define (edge->dot e)
  (match-define (edge from to) e)
  @string-append{@from -> @to @"; "})


(define (render-dot prog draw)
  (define prog.dot (make-temporary-file))

  (with-output-to-file prog.dot #:exists 'replace
    (thunk
     (display prog)))
  (define img.png (make-temporary-file))
  (define cmd (list "/usr/local/bin/dot" "-Tpng" "-o" (path->string img.png) "-K" draw (path->string prog.dot)))
  (match-define (list input output id err w) (apply process* cmd))

  (define (cleanup)
    #|
    (displayln "output")
    (displayln (port->string input))
    |#
    (displayln "err")
    (displayln (port->string err))
    (close-input-port input)
    (close-input-port err)
    (close-output-port output))

  (with-handlers ([void (lambda (e) (cleanup) (raise e))])
    (w 'wait)
    (cleanup)
    (bitmap img.png)))



#|
list input-port?
      output-port?
      exact-nonnegative-integer?
      input-port?
      ((or/c 'status 'wait 'interrupt 'kill) . -> . any))

|#
