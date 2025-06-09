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

(define-module (symbol check)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (lepton page)
  #:use-module (symbol blame)
  #:use-module (symbol check attrib)
  #:use-module (symbol check entity-pin)
  #:use-module (symbol check net-attrib)
  #:use-module (symbol check pin)
  #:use-module (symbol check pin-attrib)
  #:use-module (symbol check primitive)
  #:use-module (symbol check slot)

  #:export (check-symbol))

(define (symbol-pins->entity-pins pins slotting-info)
  (define (make-slot-pins slot)
    (map (cut symbol-pin->entity-pin <> slot) pins))

  (check-duplicates/slot (append-map make-slot-pins
                                     (or slotting-info '(#f)))))

(define (assign-nets! entity-pin-list net-map-list)
  (define pinnumber=? string=?)

  (define (make-virtual-entity-pin net-map)
    (make-entity-pin
     #f                                ; object
     #f                                ; slot
     #f                                ; pin-seq
     'pwr                              ; pin-type
     (net-map-pinnumber net-map)       ; pin-number
     #f                                ; pin-label
     (net-map-netname net-map)         ; net-name
     ;; attribs
     #f))

  (define (check-pin-and-assign-net! net-map ls)
    (and (not (null? ls))
         (let ((pin (car ls)))
           (if (pinnumber=? (net-map-pinnumber net-map)
                            (entity-pin-number pin))
               (set-entity-pin-netname! pin (net-map-netname net-map))
               (check-pin-and-assign-net! net-map (cdr ls))))))

  (let loop ((nmls net-map-list)
             (epls entity-pin-list))
    (if (null? nmls)
        epls
        (loop (cdr nmls)
              (if (check-pin-and-assign-net! (car nmls) epls)
                  epls
                  (cons (make-virtual-entity-pin (car nmls))
                        epls))))))


;;; The function is also called from C GUI code in the same named
;;; procedure in the module (schematic symbol check).  To guard
;;; <object>s from garbage collecting, as only FFI pointers to
;;; them are returned to C, they are returned to that module and
;;; directly used there.
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

          (check-duplicates/pinseq pins)
          (check-duplicates/pinnumber pins)
          ;; Create preliminary symbol structure.
          (let* ((symbol-attribs (attribs->symbol-attribs page floating-attribs))
                 (slotting-info
                  ;; Check symbol slotting attributes.
                  (check-slots page
                               pins
                               (assq-ref symbol-attribs 'numslots)
                               (assq-ref symbol-attribs 'slotdef)))
                 (net-mapping (make-net-maps (or (assq-ref symbol-attribs 'net) '()))))
            (assign-nets! (symbol-pins->entity-pins pins slotting-info)
                          net-mapping)))
        ;; Return objects to guard them from garbage collection.
        objects))))
