#lang racket

(require rackunit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Start with the script. Let's model that first, and see how it goes.
;;
;; NOTE: These were defined in reverse order, but I had to rearrange them to satisfy Racket. It may
;;   be more helpful to read the definitions from the bottom up. (They make more sense to me that
;;   way.)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Script checkpoint.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;;     bed," etc.
;; • generate-label: take a script and generate a reasonable label for the medication bottle.
;; • better script validation - it should not be possible for instance to have
;;     (redundant-count "••" 4).
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Script tests.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ... We don't have any functions yet. Hard to write any tests that way.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;; We don't want to just make the substitution because this way gives us something to *tell*
;; the doctor, instead of "here's what I filled" and it just doesn't look at all the same. It seems
;; better for record-keeping this way. Plus, a reason could hypothetically be attached to a
;; substitution with just a little bit of work, and then the pharmacist could explain.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A record of how a pharmacist is filling the script.
(define script-translation/c
  (list/c
   (-> script/c rx/c script/c)
   script/c
   rx/c))

;; The function that translates the original script to its possibly substituted fulfillment.
;; TODO: adjust dosage, doses, ifneedbe. Change sig if necessary (maybe the substituted drug must be
;;       taken 2 at a time instead of 1 at a time, for instance, if it is half-dose size.)
(define/contract (substitute-rx script rx)
  (-> script/c script/c rx/c)
  ;; TODO: Should not be the same as exact-rx.
  script)

;; Make no change to rx. The "id" translation.
(define/contract (exact-rx script rx)
  (-> script/c script/c rx/c)
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
           (equal? (drug-name (rx-drug an-rx)) (drug-name intended-drug))
           (equal? (dosage-amount (rx-dosage an-rx)) (dosage-amount intended-dosage))
           (equal? (dosage-units (rx-dosage an-rx)) (dosage-units intended-dosage))
           (>= (doses-count (rx-doses an-rx)) (doses-count intended-doses))))
      available-rx)))

;; Find all valid script-translation/c for a given script using only available-rx.
(define/contract (valid-fill-options-for script available-rx)
  (-> script/c (listof rx/c) (listof script-translation/c))
  ;; NOTE: this is where the mdb idea becomes really appealing again. Even just using
  ;;   a SQL query would be cleaner, more flexible, than something bespoke like this.
  (let ([possible-rx (possible-rx-for (script-rx script) available-rx)])
   (for/list ([new-rx possible-rx])
     (if (equal? (drug-name (rx-drug new-rx))
                 (drug-name (rx-drug (script-rx script))))
       `(,exact-rx ,script ,new-rx)
       `(,substitute-rx ,script ,new-rx)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Pharmacist translation tests.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-equal?
 (valid-fill-options-for example-script (list (script-rx example-script)))
 `((,exact-rx ,example-script ,(script-rx example-script))))

(define prilosec
  ;; "doses" here means "stock" in the pharmacy
  (rx (drug "Omeprazole") (dosage 20 "mg") (doses 1000)))
(define minocycline
  (rx (drug "Minocycline") (dosage 100 "mg") (doses 1000)))
(define prescription-strength-tylenol
  (rx (drug "Acetominophen") (dosage 325 "mg") (doses 10000)))
(define probably-deadly-tylenol
  (rx (drug "Acetominophen") (dosage 500 "mg") (doses 10000)))
(define not-quite-same-zestril
  (rx (drug "Zestril") (dosage 5 "mg") (doses 180)))

(define some-rx
  (list prilosec minocycline prescription-strength-tylenol probably-deadly-tylenol))

(check-equal?
  (valid-fill-options-for example-script some-rx)
  '())

(check-equal?
  (valid-fill-options-for example-script (reverse (cons (script-rx example-script) some-rx)))
  `((,exact-rx ,example-script ,(script-rx example-script))))

;; FIXME: this will currently fail.
(check-equal?
  (valid-fill-options-for example-script (cons not-quite-same-zestril some-rx))
  `((,substitute-rx ,example-script ,not-quite-same-zestril)))

(define tylenol-script
  (script
   (patient "Mark" "The Zuck")
   (rx (drug "Acetominophen") (dosage 500 "mg") (doses 1))
   ;; NOTE: the unicode codepoint for • is 2022.
   (sig (redundant-count "•" 1) (addtl '()))
   "for your neighbor's annoying dog"))

(check-equal?
  (valid-fill-options-for tylenol-script some-rx)
  `((,exact-rx ,tylenol-script ,probably-deadly-tylenol)))

(define unfillably-large-script
  (script
    (patient "Steve" "Jobs")
    (rx (drug "Omeprazole") (dosage 20 "mg") (doses 1500))
    (sig (redundant-count "•" 1) (addtl '("po" "qhs")))
    "because tacos don't like you but you really like them"))

;; TODO: partial fill? With some notification to the doctor. Add a "come back Tuesday" to the label.
;;   Stuff like that. We should probably have a struct we can modify as-we-go that builds up these
;;   sorts of things as they occur.
(check-equal?
  (valid-fill-options-for unfillably-large-script some-rx)
  '())

