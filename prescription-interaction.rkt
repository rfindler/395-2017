#lang racket

(struct prescription (patient rx sig indication signed))
(struct Rx (medication dose count))

(define patient/c
  (object/c
   [get-name (->m string?)]
   [give-stuff (-> any/c void?)]))

(define nat/c exact-nonnegative-integer?)

(define Rx/c (struct/c Rx string?
                       (and/c real? positive?)
                       nat/c))

(define prescriber/c
  (object/c [ack (->i ([this any/c]
                       [patient patient/c]
                       [prescription () prescription/c])
                      [result void?])]))

(define prescription/c
  (struct/c prescription
            any/c ; patient/c
            Rx/c
            any/c
            string?
            (or/c any/c #;prescriber/c #f)))

(define pharmacy/c
  (object/c [please-dispense (->m prescription/c void?)]
            [fill (->m patient/c void?)]))

(define/contract pharmacy
  pharmacy/c
  (new (class object%
         (define shelf (make-hash))
         (define/public (fill a-patient)
           (define the-prescription (hash-ref shelf a-patient #f))
           (cond
             [the-prescription
              (send a-patient give-stuff ...)
              (send prescriber ack ...)]
             [else
              (error 'fill "please come back later")]))
         (define/public (please-dispense a-prescription)
           (when (prescription-signed a-prescription)
             (hash-set! shelf
                        (prescription-patient a-prescription)
                        ;; actually put pills in a bottle and stick
                        ;; a sticker on it
                        a-prescription)))
         (super-new))))

#;
(define/contract prescriber
  prescriber/c
  
  (send pharmacy please-dispense ...))

(define bill-gates
  (new (class object%
         (define/public (get-name) "Bill Gates")
         (super-new))))

(module+ main
  (send pharmacy please-dispense)
  (send pharmacy fill bill-gates))
