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

(define-module (symbol check pin)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)

  #:use-module (lepton attrib)
  #:use-module (lepton gettext)
  #:use-module (lepton object)
  #:use-module (symbol blame)
  #:use-module (symbol check alignment)
  #:use-module (symbol check attrib)
  #:use-module (symbol check connection)
  #:use-module (symbol check line)
  #:use-module (symbol check obsolete)

  #:export-syntax (make-symbol-pin symbol-pin?
                   symbol-pin-object set-symbol-pin-object!
                   symbol-pin-seq set-symbol-pin-seq!
                   symbol-pin-type set-symbol-pin-type!
                   symbol-pin-number set-symbol-pin-number!
                   symbol-pin-label set-symbol-pin-label!
                   symbol-pin-attribs set-symbol-pin-attribs!)

  #:export (check-pin
            set-symbol-pin-printer!))


(define-record-type <symbol-pin>
  (make-symbol-pin object seq type number label attribs)
  symbol-pin?
  (object symbol-pin-object set-symbol-pin-object!)
  (seq symbol-pin-seq set-symbol-pin-seq!)
  (type symbol-pin-type set-symbol-pin-type!)
  (number symbol-pin-number set-symbol-pin-number!)
  (label symbol-pin-label set-symbol-pin-label!)
  (attribs symbol-pin-attribs set-symbol-pin-attribs!))

;;; Sets default printer for <symbol-pin>
(set-record-type-printer!
 <symbol-pin>
 (lambda (record port) (format port "#<lepton-symbol-pin ~A>" (symbol-pin-seq record))))

(define (set-symbol-pin-printer! format-string . args)
  "Adjust pretty-printing of <symbol-pin> records.
FORMAT-STRING must be in the form required by the procedure
`format'. The following ARGS may be used:
  'object
  'seq
  'type
  'number
  'label
  'attribs
Any other unrecognized argument will lead to yielding '?' in the
corresponding place.
Example usage:
  (set-symbol-pin-printer! \"<symbol-pin-~A (~A)>\" 'number 'seq)"
  (set-record-type-printer!
   <symbol-pin>
   (lambda (record port)
     (apply format port format-string
            (map
             (lambda (arg)
               (match arg
                 ('object (symbol-pin-object record))
                 ('seq (symbol-pin-seq record))
                 ('type (symbol-pin-type record))
                 ('number (symbol-pin-number record))
                 ('label (symbol-pin-label record))
                 ('attribs (symbol-pin-attribs record))
                 (_ #\?)))
             args)))))

(define %valid-pintype-values
  '(in out io oc oe pas tp tri clk pwr))

(define (valid-pintype? value)
  (and value
       (memq (string->symbol value)
             %valid-pintype-values)))

(define (invalid-pintype? value)
  (not (valid-pintype? value)))

(define (check-pin-attrib-duplicates ls)
  "Checks for duplicated attributes in object list LS."
  (define (blame-duplicate object)
    (blame-object object
                  'error
                  (format #f
                          (G_ "Duplicate pin attribute on one pin: ~A")
                          (attrib-name object))))
  (unless (null? (cdr ls))
    (for-each blame-duplicate ls))
  (car ls))

(define (check-attrib entry)
  (match entry
    ((name . objects)
     `(,name . ,(check-obsolete-pin-attrib
                 (check-pin-attrib-duplicates objects))))
    (_ (error "Invalid pin attribute list."))))


(define-syntax-rule (blame-zero-value object name)
  (blame-object object
                'error
                (format #f
                        (G_ "Prohibited zero value pin attribute: ~A=0")
                        name)))

(define-syntax-rule (blame-invalid-pintype object value)
  (blame-object object
                'error
                (format #f
                        (G_ "Invalid pin attribute value: pintype=~A")
                        value)))

(define (check-pin-attrib-value object name value)
  (match `(,name . ,value)
    (((or 'pinseq 'pinnumber) . "0") (blame-zero-value object name) #f)
    (('pintype . (? invalid-pintype? v)) (blame-invalid-pintype object v) #f)
    (_ value)))

(define-syntax-rule (blame-missing-or-wrong pin severity name attrib-alist)
  (let ((attrib (assq-ref attrib-alist name)))
    (unless attrib
      (blame-object pin
                    severity
                    (format #f (G_ "Missing pin attribute: ~A") name)))
    (check-pin-attrib-value attrib
                            name
                            (and=> attrib attrib-value))))

(define (pin->symbol-pin object)
  (let ((attrib-alist (map check-attrib
                           (attribs->attrib-alist (object-attribs object)))))
    (make-symbol-pin object
                     (blame-missing-or-wrong object 'error 'pinseq attrib-alist)
                     (blame-missing-or-wrong object 'warning 'pintype attrib-alist)
                     (blame-missing-or-wrong object 'error 'pinnumber attrib-alist)
                     (blame-missing-or-wrong object 'warning 'pinlabel attrib-alist)
                     attrib-alist)))

(define (check-pin object)
  "Checks pin OBJECT.
  * Checks that it has non-zero size.
  * Checks if it has forbidden connections.
  * Checks that it is properly aligned by grid.
Returns record <symbol-pin>."
  (check-line-size object)
  (check-connections object)
  (check-pin-alignment object)
  (pin->symbol-pin object))
