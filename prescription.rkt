#lang racket

(require rackunit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Start with the script. Let's model that first, and see how it goes.
;;
;; NOTE: These were defined in reverse order, but I had to rearrange them to satisfy Racket. It may
;;   be more helpful to read the definitions from the bottom up. (They make more sense to me that
;;   way.)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Patient.
(struct patient (first-name last-name))
(define patient/c
  (struct/c patient
            string?   ; first-name
            string?)) ; last-name

;; Rx-drug.
(struct drug (name))
(define drug/c
  (struct/c drug
            string?))

;; Rx-dosage.
(struct dosage (amount units))
(define dosage/c
  (struct/c dosage
            (and/c real? positive?) ; exact amount
            string?))               ; units (eg "mg")

;; Rx-doses.
(struct doses (count))
(define doses/c
  (struct/c doses
            exact-nonnegative-integer?))

;; Rx.
(struct rx (drug dosage doses))
(define rx/c
  (struct/c rx
            drug/c     ; drug
            dosage/c   ; dosage
            doses/c))  ; doses

;; Sig-redundant-count.
;; TODO: is there a way using contracts to enforce that number of •s must be the same as the given
;;       integer value? Then we have an invariant that the compiler can yell at us about if we get
;;       it wrong. Maybe. Hopefully.
(struct redundant-count (dots numerals))
(define redundant-count/c
  (struct/c redundant-count
            string?                      ; dots
            exact-nonnegative-integer?)) ; numerals

;; Sig-addtl.
(struct addtl (instructions))
(define addtl/c
  (struct/c addtl
            (listof string?))) ; addtl-instructions

;; Sig.
(struct sig (times addtl))
(define sig/c
  (struct/c sig
            redundant-count/c ; redundant count
            addtl/c))         ; additional description

;; Script-reason.
(define reason/c (or/c string? null?))

;; Script.
(struct script (patient rx sig desc))
(define script/c
  (struct/c script
            patient/c  ; patient
            rx/c       ; rx
            sig/c      ; sig
            reason/c)) ; reason

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Script checkpoint.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define example-script
  (script
   (patient "Bill" "Gates")
   (rx (drug "Zestril") (dosage 10 "mg") (doses 90))
   ;; NOTE: the unicode codepoint for • is 2022.
   (sig (redundant-count "•" 1) (addtl '("po" "qhs")))
   "for hypertension"))

;;
;; Possibly interesting ideas for extension?
;;
;; • More sophisticated patient object. (Medical history, current medications, etc.)
;;   ➟ Use mdb (described below) to check for medications that may interact!
;;     e.g.; (mdb/in-category-of "Zestril"
;;              #:that-will 'treat-hypertension
;;              #:that-does-not-interact-with-any-of (patient-current-medications patient))
;; • Alternate ways of specifying doses. e.g.: (days 20)
;; • Write medication as a database query. e.g.:
;;     (mdb/in-category-of "Zestril" #:that-will 'treat-hypertension)
;;     or, (mdb/any #:that-will 'treat-headache)
;;     or, (mdb/any #:that-does-not-cause 'sleepiness #:that-will 'treat-depression)
;;     Then the pharmacist can execute the given query on their own database of available pills.
;; • sig-addtl-instructions interpreter that translates "po" to "by mouth," "qhs" to "before
;      bed," etc.
;; • generate-label: take a script and generate a reasonable label for the medication bottle.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Script tests.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ... We don't have any functions yet. Hard to write any tests that way.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; What happens once we have the script?
;;
;; Pharmacist can do a number of things, in particular substitution.
;; More generally, the Pharmacist's job is to translate the script into physical medication.
;;
;; translate-script :: script/c -> script/c
;;
;; roughly:
;; (letrec ((transformations (valid-fill-options-for script))
;;          (transformation (magically-choose-one transformations)))
;;    (when (not (equal? (transformation script) script))
;;      (alert-doctor 'substitution-made transformation))
;;    (fill (transformation script)))
;;
;; magically-choose-one and valid-fill-options-for have signatures something like...
;; fill-options :: script/c -> (listof script-translation/c)
;; magically-choose-one :: (listof script-translation/c) -> script-translation/c
;;
;; script-translation/c is some record of a transformation from script/c -> script/c.
;;
;; We don't want to just make the substitution because then it becomes harder to tell exactly what
;; steps were taken in making the subtitution in the first place. If we record the intended
;; subsitution, we can defer making the substitution at this time. And it gives us something to
;; *tell* the doctor, instead of "here's what I filled" and it just doesn't look at all the same.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A record of how a pharmacist is filling the script.
(define script-translation/c
  (list/c
   (-> script/c script/c)
   script/c
   rx/c))

;; The function that translates the original script to its possibly substituted fulfilment.
(define/contract (substitute script)
  (-> script/c script/c)
  ;; TODO
  script)

;; Find all the rx from a list of available rx that can be used in place of a given rx.
(define/contract (possible-rx-for intended-rx available-rx)
  (-> rx/c (listof rx/c) (listof rx/c))
  ;; TODO
  (letrec ([intended-drug (rx-drug intended-rx)]
           [intended-dosage (rx-dosage intended-rx)]
           [intended-doses (rx-doses intended-rx)])
    (filter
      (λ (an-rx)
         (and
           (equal? (rx-drug an-rx) intended-drug)
           (equal? (rx-dosage an-rx) intended-dosage)
           (equal? (rx-doses an-rx) intended-doses)))
      available-rx)))

;; Find all valid script-translation/c for a given script using only available-rx.
(define/contract (valid-fill-options-for script available-rx)
  (-> script/c (listof rx/c) (listof script-translation/c))
  ;; NOTE: this is where the mdb idea becomes really appealing again. Even just using
  ;;   a SQL query would be cleaner, more flexible, than something bespoke like this.
  (let ([possible-rx (possible-rx-for (script-rx script) available-rx)])
   (for/list ([new-rx possible-rx])
       (list substitute script new-rx))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Pharmacist tests.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-equal?
 (valid-fill-options-for example-script (list (script-rx example-script)))
 (list (list substitute example-script (script-rx example-script))))

