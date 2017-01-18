#lang racket

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
            exact-nonnegative-integer? ; times
            string?)) ; additional description

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
   (rx "Zestril" (dosage 10 "mg") (doses 90))
   ;; NOTE: the unicode codepoint for • is 2022.
   (sig (redundant-count "•" 1) '("po" "qhs"))
   "for hypertension")) ;; --> creates a script object.

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

;; ... We don't have any functions yet.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; What happens once we have the script?
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Cool, it works. Now let's make it do something.

;; Pharmacist can do a number of things, mostly by substitution.
;; Generally, the Pharmacist's job is to translate the script into physical medication.
