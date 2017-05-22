;;; Lepton EDA Symbol Checker
;;; Scheme API
;;; Copyright (C) 2017 Lepton EDA Contributors
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
;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA

(define-module (symbol check)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-1)
  #:use-module (geda page)
  #:use-module (symbol blame)
  #:use-module (symbol check attrib)
  #:use-module (symbol check pin)
  #:use-module (symbol check pin-attrib)
  #:use-module (symbol check primitive)
  #:use-module (symbol check slot)

  #:export (check-symbol))

(define (check-symbol page)
  (let ((objects (page-contents page)))

    ;; First reset all blaming info collected previously.
    (for-each acquit-object `(,page . ,(page-contents page)))

    (let ((rest (filter-map check-primitive objects)))
      (receive (pins attribs)
          (partition symbol-pin? rest)

        ;; Check symbol attributes; common checks.
        (for-each check-attribute attribs)

        (receive (floating-attribs attached-attribs)
            (partition floating-attrib? attribs)

          ;; Create preliminary symbol structure.
          (let ((symbol-attribs (attribs->symbol-attribs page floating-attribs)))

            ;; Check pinseq attributes.
            (check-duplicates/pinseq pins)
            (check-duplicates/pinnumber pins)

            ;; Check symbol pinnumber attribute
            (let ((nets (sort (net-numbers objects) string<?))
                  (pinnumber-values (sort (filter-map symbol-pin-number pins) string<?)))

              (check-duplicate-net-pinnumbers page nets)
              (check-duplicate-net-pinnumber-numbers page pinnumber-values nets))

            ;; Check symbol slotting attributes.
            (let ((slotting-info
                   (check-slots page
                                pins
                                (assq-ref symbol-attribs 'numslots)
                                (assq-ref symbol-attribs 'slotdef))))

              `(lepton-symbol (@ ,@symbol-attribs (slotting-info ,@slotting-info))
                              ,@pins))))))))
