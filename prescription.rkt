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
(struct patient (first-name last-name)) ;; currently-taking))
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
(define optional-string/c (or/c string? null?))

;; Script.
(struct script (patient rx sig reason))
(define script/c
  (struct/c script
            patient/c           ; patient
            rx/c                ; rx
            sig/c               ; sig
            optional-string/c)) ; reason

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
;;     --- Note that labels actually say things like "Omeprazole... Substituted For: Prilosec."
;; • better script validation - it should not be possible for instance to have
;;     (redundant-count "••" 4), or to write an Rx for a nonexistent drug, etc.
;; • drug medium - capsule, tablet, liquid, etc. This is also something that usually shows up on a
;;     label, but which we don't really know if/when a doctor specifies. A pharmacist however has to
;;     know and/or decide.
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
;; (letrec ((transformations (valid-fill-methods-for script))
;;          (transformation (magically-choose-one transformations)))
;;    (when (not (equal? (transformation script) script))
;;      (alert-doctor 'substitution-made transformation))
;;    (fill (transformation script)))
;;
;; magically-choose-one and valid-fill-methods-for have signatures something like...
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
(define/contract (valid-fill-methods-for script available-rx)
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
;; Translation really feels like it should be... more of a process that looks like "here's an ORM
;; query from the doctor that describes the drug he wants prescribed, run the query against your
;; available Rx."
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-equal?
 (valid-fill-methods-for example-script (list (script-rx example-script)))
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
  (valid-fill-methods-for example-script some-rx)
  '())

(check-equal?
  (valid-fill-methods-for example-script (cons (script-rx example-script) some-rx))
  `((,exact-rx ,example-script ,(script-rx example-script))))

;; FIXME: this will currently fail.
(check-equal?
  (valid-fill-methods-for example-script (cons not-quite-same-zestril some-rx))
  `((,substitute-rx ,example-script ,not-quite-same-zestril)))

(define tylenol-script
  (script
   (patient "Mark" "The Zuck")
   (rx (drug "Acetominophen") (dosage 500 "mg") (doses 1))
   ;; NOTE the unicode codepoint for • is 2022.
   (sig (redundant-count "•" 1) (addtl '()))
   "for your neighbor's annoying dog"))

(check-equal?
  (valid-fill-methods-for tylenol-script some-rx)
  ;; BUG probably-deadly-tylenol will have the wrong "doses" because it's stock, not dispensed.
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
  (valid-fill-methods-for unfillably-large-script some-rx)
  ;; possibly better answer?
  ;; `((,partial-fill ,1000 (,exact-rx ,unfillably-large-script ,prilosec)))
  '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Pharmacists and doctors need to be modeled, and they need to be able to converse.
;; There should also be an actual patient model, instead of just a pair of names.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Doctor writes a script.
(define (doctor/write-script patient)
  ;; TODO
  (script
    patient
    (rx (drug "Omeprazole") (dosage 20 "mg") (doses 10))
    (sig (redundant-count "••" 2) (addtl '("po")))
    "for gastroesophageal reflux disorder"))

;; Script signing.
(struct signed (script signature))
(define signed/c
  (struct/c signed
            script/c            ; script
            optional-string/c)) ; signature
  #| (struct/c signed |#
  #|           ;; FIXME do I really have to copy this? |#
  #|           patient/c           ; patient |#
  #|           rx/c                ; rx |#
  #|           sig/c               ; sig |#
  #|           optional-string/c   ; reason |#
  #|           ; |#
  #|           optional-string/c)) ; signature |#

;; Doctor signs the script.
(define (doctor/sign-script script name)
  (signed script name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Pharmacist uses the query to dispense, confirms.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Verify query is signed.
(define (pharmacist/verify some-script)
  (if (not (null? (signed-signature some-script)))
    (signed-script some-script)
    ;; QUESTION should this be an error?
    (error 'pharmacist/verify "The script is not signed.")))

;; script -> adjusted, substituted, disambiguated (if necessary) script.
(define (pharmacist/adjust original-script actual-fill-method)
  ;; TODO actually adjust the script, based on results of pharmacist/find.
  original-script)

;; adjusted-script -> actual-medication
;; TODO
;; stopgap: script -> rx-with-label
;; Need a quick struct for adding a label to rx.
(struct rx-with-label (rx label))
(define rx-with-label/c
  (struct/c rx-with-label
            rx/c      ; actual rx
            string?)) ; label
;; Now actually take a script and give an rx-with-label back.
(define (pharmacist/fill script stock)
  ;; TODO better sig-to-string.
  (let ([sig-to-string
          (lambda (sig)
            "#<sig>")])
    (let ([fill-options (valid-fill-methods-for script stock)])
      (when (empty? fill-options)
        (error 'pharmacist/fill "Cannot fill script. Please come again later.")))
    (letrec ([fill-method (first (valid-fill-methods-for script stock))]
             [new-script (pharmacist/adjust script fill-method)])
      ; TODO
      ;(alert-doctor 'script-filled script fill-method)
      ; and something like:
      ;(save-prescription-record script new-script fill-date) ; gives an rxid
      ;(add-to-shelf (rx-with-label ...))
      (rx-with-label (script-rx new-script)
                     (string-append
                       (sig-to-string (script-sig new-script))
                       "\n"
                       (script-reason new-script))))))

;; pharmacist -> patient -> patient (now with new medications).
(define (pharmacist/dispense pharmacist patient)
  ;; TODO something like...
  ;(get-from-shelf (rx-for patient))
  ;(struct-update patient [currently-taking (append (patient-currently-taking) new-stuff)])
  ;(alert-doctor 'medication-dispensed patient new-stuff)
  null)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Tests. Any tests. Does anything work?
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Signing and verification.
(check-equal?
  (pharmacist/verify (doctor/sign-script example-script "John Hancock"))
  example-script)

(check-exn
  exn:fail?
  (lambda ()
    (pharmacist/verify (signed example-script null))))

;; Pharmacist fill.
(check-exn
  exn:fail?
  (lambda ()
    (pharmacist/fill example-script some-rx)))

(check-equal?
  (rx-with-label-rx (pharmacist/fill example-script (cons (script-rx example-script) some-rx)))
  (script-rx example-script))

(check-equal?
  (rx-with-label-rx (pharmacist/fill tylenol-script some-rx))
  (script-rx tylenol-script))

(check-equal?
  (rx-with-label-label (pharmacist/fill tylenol-script some-rx))
  "#<sig>\nfor your neighbor's annoying dog")

;; FIXME this will currently be an exn. Need to make valid-fill-methods-for return a valid fill
;;   method when there's a way to fill the rx inexactly.
(check-equal?
  (rx-with-label-rx (pharmacist/fill example-script (cons not-quite-same-zestril some-rx)))
  not-quite-same-zestril)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Stuff that isn't figured out yet.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (script->query script)
  ;; TODO make script more executable.
  script)

;; TODO find a conversion from script->actual-medication, finalize this model.
(struct actual-medication rx (medium label rxid refills-allowed?))
(define actual-medication/c
  (struct/c actual-medication
            ; FIXME do I really have to copy this?
            drug/c     ; inherited...
            dosage/c   ; inherited...
            doses/c    ; inherited...
            ;
            any/c ;medium/c   ; medium - eg capsule
            string?    ; label text
            number?    ; prescription id (for looking up at the pharmacy)
            boolean?)) ; refills allowed

;;
;; Pharmacist needs to be able to confirm translation and dispensal with doctor.
;; Also needs to be able to return a new patient who has new drugs.
;;
;; And, at any point, the doctor should be able to (alert-pharmacist 'stop-dispense) or 'alter-script
;; or what-have-you in case things change... These communications channels are undefined now.
;;

