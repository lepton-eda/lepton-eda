;;; Lepton EDA netlister
;;; Copyright (C) 2016-2017 gEDA Contributors
;;; Copyright (C) 2017-2019 Lepton EDA Contributors
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
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;;; MA 02111-1301 USA.

(define-module (netlist attrib refdes)

  ;; Import C procedures and variables.
  #:use-module (netlist core gettext)

  #:use-module (ice-9 match)
  #:use-module (geda object)
  #:use-module (geda log)
  #:use-module (lepton page)
  #:use-module (netlist config)
  #:use-module (netlist error)
  #:use-module (netlist hierarchy)
  #:use-module (netlist mode)
  #:use-module (netlist schematic-component)
  #:use-module (netlist subschematic)

  #:export (schematic-component-refdes*
            schematic-component-refdes->string))


(define (hierarchy-create-refdes basename hierarchy-tag)
  (define (error-invalid-hierarchy-tag tag)
    (netlist-error 1 (_ "Invalid hierarchy tag: ~S") tag))

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
    (netlist-error 1 (_ "Netlist mode ~S is not supported.") mode))

  (let ((refdes (and=> (assq-ref attribs 'refdes) car)))
    (case (netlist-mode)
      ((spice)
       (let ((slot (and=> (assq-ref attribs 'slot) car)))
         (if slot
             (string-append refdes "." slot)
             refdes)))
      ((geda) refdes)
      (else (error-netlist-mode-not-supported (netlist-mode))))))

;;; Make up an artificial refdes for OBJECT with HIERARCHY-TAG, if
;;; needed, and warn the user about this.
(define (mock-refdes object hierarchy-tag)
  (log! 'critical
        (_ "\nNon-graphical symbol ~S\nat ~A on page ~S\nhas neither refdes= nor net=.")
        (component-basename object)
        (component-position object)
        (page-filename (object-page object)))
  (hierarchy-create-refdes "U?" hierarchy-tag))

;;; Make up a plain symbol refdes from symbol ATTRIBS with
;;; HIERARCHY-TAG.
(define (make-refdes attribs hierarchy-tag)
  (hierarchy-create-refdes (netlist-mode-refdes attribs) hierarchy-tag))


(define* (schematic-component-refdes* component #:optional hierarchical?)
  (define object (schematic-component-object component))
  (define attribs (schematic-component-attribs component))
  (define has-net? (not (null? (schematic-component-net-maps component))))
  (define graphical? (or (schematic-component-graphical? component)
                         (schematic-component-nc? component)))
  (define plain-symbol? (and (not has-net?)
                             (not graphical?)))
  (define hierarchy-tag
    (and hierarchical?
         (subschematic-name (schematic-component-parent component))))

  ;; First try to get refdes from attribs.
  (or (make-refdes attribs hierarchy-tag)
      ;; If no refdes found, make a mock one for non-special
      ;; symbols.  For graphical symbols, or for symbols having
      ;; the "net=" attribute, which are considered to be power
      ;; or some other special symbols, it is #f.
      (and plain-symbol? (mock-refdes object hierarchy-tag))))


(define (schematic-component-refdes->string refdes)
  (define reverse-refdes-order?
    (gnetlist-config-ref 'reverse-refdes-order))

  (define refdes-separator
    (gnetlist-config-ref 'refdes-separator))

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
