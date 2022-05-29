;;; Lepton EDA Symbol Checker
;;; Scheme API
;;; Copyright (C) 2017-2022 Lepton EDA Contributors
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

(define-module (symbol check slot)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)

  #:use-module (lepton attrib)
  #:use-module (lepton gettext)
  #:use-module (lepton object)
  #:use-module (symbol blame)
  #:use-module (symbol check attrib)
  #:use-module (symbol check duplicate)

  #:export-syntax (make-slot slot?
                   slot-object set-slot-object!
                   slot-number set-slot-number!
                   slot-pins set-slot-pins!)

  #:export (check-slots
            set-slot-printer!))

(define-record-type <slot>
  (make-slot object number pins)
  slot?
  (object slot-object set-slot-object!)
  (number slot-number set-slot-number!)
  (pins slot-pins set-slot-pins!))

;;; Sets default printer for <slot>
(set-record-type-printer!
 <slot>
 (lambda (record port) (format port "#<lepton-slot ~A>" (slot-number record))))

(define (set-slot-printer! format-string . args)
  "Adjust pretty-printing of <slot> records.
FORMAT-STRING must be in the form required by the procedure
`format'. The following ARGS may be used:
  'object
  'number
  'pins
Any other unrecognized argument will lead to yielding '?' in the
corresponding place.
Example usage:
  (set-slot-printer! \"<slot-~A ~A>\" 'number 'pins)"
  (set-record-type-printer!
   <slot>
   (lambda (record port)
     (apply format port format-string
            (map
             (lambda (arg)
               (match arg
                 ('object (slot-object record))
                 ('number (slot-number record))
                 ('slot-pins (slot-pins record))
                 (_ #\?)))
             args)))))

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
              (G_ "Found ~A=~A attribute")
              (attrib-name object)
              (attrib-value object)))

(define (blame-invalid-slotdef object)
  (blame-error object
               (G_ "Invalid slotdef=~A attribute (the format is #:#,#,#,...)")
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
                        (G_ "Negative attribute: numslots=~A")
                        num))
          ((zero? num)
           (blame-info numslots (G_ "numslots set to 0, symbol does not have slots"))
           #f)
          ((positive? num) num)))))


(define (check-slotdef-pins s object)
  (define (check-slotdef-pin pin object)
    (when (string=? pin "0")
      (blame-error object
                   (G_ "Zero pin number in slotdef=~A")
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
                  (G_ "Found a zero slot in slotdef=~A")
                  (attrib-value object)))
    ((? greater-than-numslots? num)
     (blame-error object
                  (G_ "Slot number ~A (slotdef=~A) is greater than the maximum slot number (~A)")
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


(define (check-slot-numbers page numslots slot-list)
  (define (slot-number-in-list? slot ls)
    (and (memq (slot-number slot) ls)
         slot))

  (define (blame-superfluous-slot-number slot)
    (blame-error (slot-object slot)
                 (G_ "Superfluous slotdef=~A:... (there should be ~A slotdef= attributes)")
                 (slot-number slot)
                 numslots))

  (let* ((required-numbers (iota numslots 1))
         (real-numbers (map slot-number slot-list))
         (missing-numbers (lset-difference = required-numbers real-numbers))
         (superfluous-numbers (lset-difference = real-numbers required-numbers))
         (superfluous-slots (filter-map (cut slot-number-in-list? <> superfluous-numbers)
                                        slot-list)))
    (for-each (cut blame-error page
                   (G_ "Missing slotdef=~A:... (there should be ~A slotdef= attributes)")
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
                     (G_ "Not enough pins in slotdef=~A (must be ~A)")
                     (attrib-value object)
                     numpins)
        #f)
       ((> real-pin-number numpins)
        (blame-error object
                     (G_ "Too many pins in slotdef=~A (must be ~A)")
                     (attrib-value object)
                     numpins)
        #f)
       (else slot))))

  (filter-map check-pin-number slot-list))


(define (check-slot-number-duplicates slot-list)
  "Checks for duplicated slot numbers in SLOT-LIST."
  (define (blame-duplicate-slot-number slot)
    (let ((object (slot-object slot)))
      (blame-error object
                   (G_ "Duplicate slot number ~A: ~A")
                   (slot-number slot)
                   (text-string object))))

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
                (G_ "Found ~A distinct pins in slots")
                (length (list->duplicate-list (append-map slot-pins ls)
                                              string<?
                                              string=?))))

  ;; Look for numslots to see if this symbol has slotting info.
  (let ((numslots (check-numslots page numslots-ls)))
    ;; If there's no numslots= attribute, don't check slotting at all.
    (and numslots
         (let ((slot-list (make-slot-list numslots slotdef-ls)))
           (check-slot-numbers page numslots slot-list)
           (count-all-slot-pins page slot-list)
           (check-slotdef-pin-number (length pins)
                                     (check-slot-number-duplicates slot-list))))))
