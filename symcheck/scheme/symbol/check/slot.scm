(define-module (symbol check slot)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (symbol gettext)
  #:use-module (symbol blame)
  #:use-module (symbol check attrib)
  #:use-module (symbol check duplicate)
  #:use-module (geda attrib)
  #:use-module (geda object)

  #:export (check-slots))

(define-record-type <slot>
  (make-slot object number pins)
  slot?
  (object slot-object set-slot-object!)
  (number slot-number set-slot-number!)
  (pins slot-pins set-slot-pins!))

(define-syntax blame-error
  (syntax-rules ()
    ((_ object msg arg ...)
     (begin (blame-object object 'error (format #f msg arg ...))
            #f))))

(define-syntax blame-info
  (syntax-rules ()
    ((_ object msg arg ...)
     (begin (blame-object object 'info (format #f msg arg ...))))))

(define (blame-attrib-info object)
  (blame-info object
              (_ "Found ~A=~A attribute")
              (attrib-name object)
              (attrib-value object)))

(define (blame-invalid-slotdef object)
  (blame-error object
               (_ "Invalid slotdef=~A attribute (the format is #:#,#,#,...)")
               (attrib-value object)))

(define (string->integer value)
  (let ((num (string->number value)))
    (and (integer? num)
         num)))

(define (check-numslots page numslots)
  (let* ((value (and=> numslots attrib-value))
         (num (and=> value string->integer)))
    (and num
         (cond
          ((negative? num)
           (blame-error numslots
                        (_ "Negative attribute: numslots=~A")
                        num))
          ((zero? num)
           (blame-info numslots (_ "numslots set to 0, symbol does not have slots"))
           #f)
          ((positive? num) num)))))


(define (check-slotdef-pins s object)
  (define (check-slotdef-pin pin object)
    (when (string=? pin "0")
      (blame-error object
                   (_ "Zero pin number in slotdef=~A")
                   (attrib-value object))))

  (let ((pin-string-list (string-split s #\,)))
    (for-each (cut check-slotdef-pin <> object)
              pin-string-list)
    pin-string-list))

(define (check-slotdef-number object slotdef number-string numslots)
  (define (greater-than-numslots? num)
    (> num numslots))

  (match (string->integer number-string)
    (#f (blame-invalid-slotdef object))
    ((? negative? num) (blame-invalid-slotdef object))
    ((? zero? num)
     (blame-error object
                  (_ "Found a zero slot in slotdef=~A")
                  (attrib-value object)))
    ((? greater-than-numslots? num)
     (blame-error object
                  (_ "Slot number ~A (slotdef=~A) is greater than the maximum slot number (~A)")
                  num
                  (attrib-value object)
                  numslots))
    ((? positive? num) num)
    (_ (blame-invalid-slotdef object))))

;;; Takes slotdef attribute object and translates it into a pair
;;; (SLOTNUM . (PIN-LIST)).
(define (check-slotdef numslots object)
  (let* ((slotdef (attrib-value object))
         (colon-position (string-index slotdef #\:)))
    (blame-attrib-info object)
    (if colon-position
        (let* ((slot-number-string (string-take slotdef colon-position))
               (pinlist-string (string-drop slotdef (1+ colon-position)))
               (slotnum (check-slotdef-number object
                                              slotdef
                                              slot-number-string
                                              numslots)))
          (and slotnum
               (make-slot object
                          slotnum
                          (check-slotdef-pins pinlist-string
                                              object))))
        (blame-invalid-slotdef object))))

;;; Helper procedures.
(define (slot-number<? a b)
  (< (slot-number a) (slot-number b)))

(define (slot-number=? a b)
  (= (slot-number a) (slot-number b)))

(define (make-slot-list numslots slotdef-list)
  (if slotdef-list
      (sort (filter-map (cut check-slotdef numslots <>)
                        slotdef-list)
            slot-number<?)
      '()))


(define (check-slot-pin-duplicates slot)
  "Checks for duplicated pin numbers in SLOT."
  (define (blame-if-list ls)
    (when (list? ls)
      (let ((object (slot-object slot))
            (pin (car ls)))
        (blame-error object
                     (_ "Duplicate pin number ~A in slotdef=~A")
                     pin
                     (attrib-value object)))))

  (for-each blame-if-list
            (list->duplicate-list (slot-pins slot)
                                  string<?
                                  string=?)))


(define (check-slot-pin-in-slot pin slot other-slot)
  (define (blame-slot-list-pin-duplicate slot)
    (let ((object (slot-object slot)))
      (blame-error object
                   (_ "Duplicate pin number ~A in slotdef=~A and other slotdef")
                   pin
                   (attrib-value object))))

  (when (member pin (slot-pins other-slot))
    (blame-slot-list-pin-duplicate slot)
    (blame-slot-list-pin-duplicate other-slot)))


(define (check-slot-list-pins ls)
  (unless (null? ls)
    (let ((slot (car ls)))
      (check-slot-pin-duplicates slot)
      (for-each (lambda (other-slot)
                  (for-each (lambda (pin) (check-slot-pin-in-slot pin slot other-slot))
                            (slot-pins slot)))
                (cdr ls))
      (check-slot-list-pins (cdr ls)))))


(define (check-slot-numbers page numslots slot-list)
  (define (slot-number-in-list? slot ls)
    (and (memq (slot-number slot) ls)
         slot))

  (define (blame-superfluous-slot-number slot)
    (blame-error (slot-object slot)
                 (_ "Superfluous slotdef=~A:... (there should be ~A slotdef= attributes)")
                 (slot-number slot)
                 numslots))

  (let* ((required-numbers (iota numslots 1))
         (real-numbers (map slot-number slot-list))
         (missing-numbers (lset-difference = required-numbers real-numbers))
         (superfluous-numbers (lset-difference = real-numbers required-numbers))
         (superfluous-slots (filter-map (cut slot-number-in-list? <> superfluous-numbers)
                                        slot-list)))
    (for-each (cut blame-error page
                   (_ "Missing slotdef=~A:... (there should be ~A slotdef= attributes)")
                   <>
                   numslots)
              missing-numbers)
    (for-each (cut blame-superfluous-slot-number <>)
              superfluous-slots)))


(define (check-slotdef-pin-number numpins slot-list)
  (define (check-pin-number slot)
    (let ((real-pin-number (length (slot-pins slot)))
          (object (slot-object slot)))
      (cond
       ((< real-pin-number numpins)
        (blame-error object
                     (_ "Not enough pins in slotdef=~A (must be ~A)")
                     (attrib-value object)
                     numpins))
       ((> real-pin-number numpins)
        (blame-error object
                     (_ "Too many pins in slotdef=~A (must be ~A)")
                     (attrib-value object)
                     numpins)))))

  (for-each check-pin-number slot-list))


(define (check-slot-number-duplicates slot-list)
  "Checks for duplicated slot numbers in SLOT-LIST."
  (define (blame-duplicate-slot-number slot)
    (let ((object (slot-object slot)))
      (blame-error object
                   (_ "Duplicate slot number ~A in slotdef=~A")
                   (slot-number slot)
                   (attrib-value object))))

  (define (blame-if-list ls)
    (if (list? ls)
        (begin
          (for-each blame-duplicate-slot-number ls)
          (car ls))
        ls))

  (map blame-if-list
       (list->duplicate-list slot-list
                             slot-number<?
                             slot-number=?)))


(define (check-slots page pins numslots-ls slotdef-ls)
  (define (count-all-slot-pins page ls)
    (blame-info page
                (_ "Found ~A distinct pins in slots")
                (length (list->duplicate-list (append-map slot-pins ls)
                                              string<?
                                              string=?))))

  ;; Look for numslots to see if this symbol has slotting info.
  (let ((numslots (check-numslots page numslots-ls)))
    ;; If there's no numslots= attribute, don't check slotting at all.
    (and numslots
         (let ((slot-list (make-slot-list numslots slotdef-ls)))
           (check-slot-numbers page numslots slot-list)
           (check-slot-list-pins (check-slot-number-duplicates slot-list))
           (check-slotdef-pin-number (length pins) slot-list)
           (count-all-slot-pins page slot-list)
           slot-list))))
