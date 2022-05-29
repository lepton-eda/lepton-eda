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

(define-module (symbol check attrib)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)

  #:use-module (lepton attrib)
  #:use-module (lepton gettext)
  #:use-module (lepton object)
  #:use-module (lepton page)
  #:use-module (symbol blame)
  #:use-module (symbol check duplicate)
  #:use-module (symbol check obsolete)

  #:export (floating-attrib?
            check-attribute
            attribs->attrib-alist
            attribs->symbol-attribs))

(define required-attribs '(refdes footprint device))

(define (floating-attrib? object)
  "Returns #t if OBJECT is attribute and is floating, otherwise
returns #f."
  (and (attribute? object)
       (not (attrib-attachment object))))

;;; Returns #t if OBJECT is correct graphical= attribute,
;;; otherwise returns #f and blames it.
(define (check-special/graphical object)
  (let ((value (attrib-value object)))
    (or (string=? value "1")
        (and (blame-object object
                           'warning
                           (format #f
                                   (G_ "Set 'graphical=1' if you want the symbol to be graphical, current value: ~A")
                                   value))
             #f))))


;;; Checks device= attribute. If schematic symbol is graphical,
;;; also checks for device= value which should be 'none'.  This is
;;; a special check required by some netlister backends
;;; (e.g. spice-sdb).
(define (check-special/device is-graphical device-list)
  (and is-graphical
       device-list
       (let ((device (car device-list)))
         (unless (string=? (attrib-value device) "none")
           ;; Check for "device=none" for graphical symbols.
           (blame-object device
                         'warning
                         (format #f (G_"Graphical symbols should have device=none")))))))

(define (check-attribute object)
  "Checks attribute OBJECT."
  (and (attribute? object)
       (let ((aname (string->symbol (attrib-name object)))
             (s (text-string object)))

         (case aname
           ((type name)
            (blame-object object
                          'error
                          (format #f (G_ "Forbidden attribute: ~A") s)))

           ((uref label email)
            (blame-object object
                          'warning
                          (format #f (G_ "Obsolete attribute: ~A") s)))

           ;; Valid pin attributes.
           ((pinlabel pintype pinseq pinnumber)
            (if (or (floating-attrib? object)
                    (not (pin? (attrib-attachment object))))
                (blame-object object
                              'error
                              (format #f (G_ "Misplaced pin attribute: ~A") s))))

           ;; Valid attributes.
           ((device graphical description author
                    comment numslots slotdef footprint
                    documentation refdes slot net
                    value symversion dist-license use-license)

            ;; Check if they are floating (not attached to anything).
            (unless (floating-attrib? object)
              (blame-object object
                            'error
                            (format #f (G_ "Wrongly attached attribute: ~A") s))))

           ;; All other attributes are unknown.
           (else (blame-object object
                               'warning
                               (format #f (G_ "Unknown attribute: ~A") s)))))))

(define (check-floating-attrib-duplicates ls)
  "Checks for duplicated attributes in object list LS."
  (define (blame-duplicate object)
    (blame-object object
                  'error
                  (format #f
                          (G_ "Duplicate floating attribute: ~A")
                          (attrib-name object))))
  (unless (null? (cdr ls))
    (for-each blame-duplicate ls))
  (car ls))

(define (check-attrib entry)
  (match entry
    (('graphical . objects)
     `(graphical . ,(check-special/graphical (check-floating-attrib-duplicates objects))))
    (('slotdef . objects)
     `(slotdef . ,objects))
    (('net . objects)
     `(net . ,objects))
    ((name . objects)
     `(,name . ,(check-obsolete-floating-attrib
                 (check-floating-attrib-duplicates objects))))
    (_ (error "Invalid attribute list."))))

(define (attribs->attrib-alist objects)
  (fold
   (lambda (object alist)
     (let* ((name (string->symbol (attrib-name object)))
            (same (assq-ref alist name)))
       (if same
           (assq-set! alist name (cons object same))
           (assq-set! alist name (list object)))))
   ;; Initial empty alist.
   '()
   objects))

(define (check-missing-attribs page alist)
  (define (blame-missing name)
    (unless (assq-ref alist name)
      (blame-object page
                    'warning
                    (format #f
                            (G_ "Missing required attribute: ~A")
                            name))))

  (for-each blame-missing required-attribs))

(define (attribs->symbol-attribs page floating-attribs)
  "Forms symbol attribute list from objects in the list
FLOATING-ATTRIBS."
  (let ((attrib-alist (attribs->attrib-alist floating-attribs)))
    (check-missing-attribs page attrib-alist)
    (let ((output-alist (map check-attrib attrib-alist)))
      (check-special/device (assq-ref output-alist 'graphical)
                            (assq-ref attrib-alist 'device))
      output-alist)))
