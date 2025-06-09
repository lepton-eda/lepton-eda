;;; Lepton EDA netlister
;;; Copyright (C) 2016-2017 gEDA Contributors
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

(define-module (netlist attrib refdes)
  #:use-module (ice-9 match)
  #:use-module (lepton gettext)
  #:use-module (lepton log)
  #:use-module (lepton object)
  #:use-module (lepton page)
  #:use-module (netlist config)
  #:use-module (netlist error)
  #:use-module (netlist mode)

  #:export (make-refdes
            make-mock-refdes
            hierarchical-refdes->string))


;;; Refdes value used in netlists for plain symbols without
;;; refdes.
(define mock-refdes "U?")


(define (make-hierarchical-refdes basename hierarchy-tag)
  (define (error-invalid-hierarchy-tag tag)
    (netlist-error 1 (G_ "Invalid hierarchy tag: ~S") tag))

  (and basename
       (match hierarchy-tag
         ((? list? tag) `(,basename . ,tag))
         (#f basename)
         (_ (error-invalid-hierarchy-tag hierarchy-tag)))))


;;; Create refdes from object ATTRIBS depending on the current
;;; netlist mode.  In "geda" mode it is the plain value of the
;;; refdes attribute.  In "spice" mode it is a string of the form
;;; "refdes-attrib-value.slot-attrib-value".
(define (netlist-mode-refdes attribs)
  (define (error-netlist-mode-not-supported mode)
    (netlist-error 1 (G_ "Netlist mode ~S is not supported.") mode))

  (let ((refdes (and=> (assq-ref attribs 'refdes) car)))
    (case (netlist-mode)
      ((spice)
       (let ((slot (and=> (assq-ref attribs 'slot) car)))
         (if slot
             (string-append refdes "." slot)
             refdes)))
      ((geda) refdes)
      (else (error-netlist-mode-not-supported (netlist-mode))))))


(define (make-mock-refdes object hierarchy-tag)
  "Makes up an artificial refdes for OBJECT. It may be used when
the object has neither attached \"refdes\" attribute, nor any
special other one. If HIERARCHY-TAG is not #F, forms a
hierarchical refdes in the form of a list. Logs a warning with the
object info."
  (log! 'critical
        (G_ "\nNon-graphical symbol ~S\nat ~A on page ~S\nhas neither refdes= nor net=.")
        (component-basename object)
        (component-position object)
        (page-filename (object-page object)))
  (make-hierarchical-refdes mock-refdes hierarchy-tag))


(define (make-refdes attribs hierarchy-tag)
  "Makes up a plain symbol refdes from symbol ATTRIBS. If
HIERARCHY-TAG is not #F, forms a hierarchical refdes in the form
of a list."
  (make-hierarchical-refdes (netlist-mode-refdes attribs)
                            hierarchy-tag))


(define (hierarchical-refdes->string refdes)
  "Transforms hierarchical REFDES, being a list, into string form
accounting for netlister settings."
  (define reverse-refdes-order?
    (netlist-config-ref 'reverse-refdes-order))

  (define refdes-separator
    (netlist-config-ref 'refdes-separator))

  (match refdes
    ;; Return #f for graphical, hierarchical, etc. symbols.
    ((#f a ...) #f)
    ((? list? refdes)
     (string-join
      ;; Refdes list is already reversed, so we have to reverse
      ;; it again if we do not need the reverse order.
      (if reverse-refdes-order? refdes (reverse refdes))
      refdes-separator))
    (refdes refdes)))
