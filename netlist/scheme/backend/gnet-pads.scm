;;; Lepton EDA netlister
;;; Copyright (C) 1998-2010 Ales Hvezda
;;; Copyright (C) 1998-2017 gEDA Contributors
;;; Copyright (C) 2018 Lepton EDA Contributors
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

;; PADS netlist format

(use-modules (netlist schematic)
             (netlist schematic toplevel))

;; This procedure takes a net name as determined by gnetlist and
;; modifies it to be a valid pads net name.
;;
(define pads:map-net-names
  (lambda (net-name)
    (let ((net-alias net-name)
          )
      ;; Convert to all upper case because Pads seems
      ;; to do that internally anyway and we'd rather do
      ;; it here to catch shorts created by not preserving
      ;; case.  Plus we can eliminate lots of ECO changes
      ;; that will show up during backannotation.
      (string-upcase net-alias)
      )
    )
  )

;; This procedure takes a refdes as determined by gnetlist and
;; modifies it to be a valid pads refdes.
;;
(define pads:map-refdes
  (lambda (refdes)
    (let ((refdes-alias refdes)
          )
      ;; Convert to all upper case because Pads seems
      ;; to do that internally anyway and we'd rather do
      ;; it here to catch name clashes created by not preserving
      ;; case.
      (string-upcase refdes-alias)
      )
    )
  )

(define pads:components
   (lambda (packages)
      (if (not (null? packages))
         (begin
            (let ((pattern (gnetlist:get-package-attribute (car packages)
                                                           "pattern"))
            ;; The above pattern should stay as "pattern" and not "footprint"
                  (package (car packages)))
               (if (not (string=? pattern "unknown"))
                  (display pattern))

               ;; print out the refdes with aliasing
               (display (gnetlist:alias-refdes package))

               (write-char #\tab)
               (display (gnetlist:get-package-attribute package "footprint"))
               (display "\r\n"))
            (pads:components (cdr packages))))))

(define (connections->string connections)
  (define package car)
  (define pinnumber cdr)
  (define (connection->string connection)
    (format #f
            "~A.~A"
            (gnetlist:alias-refdes (package connection))
            (pinnumber connection)))
  (string-join (map connection->string connections) " " 'prefix))


(define (pads:write-net netnames)
  (if (not (null? netnames))
      (let ((netname (car netnames)))
        (format #t "*SIGNAL* ~A\r\n" (gnetlist:alias-net netname))
        (display (gnetlist:wrap
                  (string-append
                   (connections->string
                    (get-all-connections netname))
                   "\r\n")
                  78
                  ""))
        (pads:write-net (cdr netnames)))))

(define (pads output-filename)
  (let ((nets (schematic-nets (toplevel-schematic)))
        (packages (schematic-package-names (toplevel-schematic))))
    ;; initialize the net-name aliasing
    (gnetlist:build-net-aliases pads:map-net-names nets)

    ;; initialize the refdes aliasing
    (gnetlist:build-refdes-aliases pads:map-refdes packages)

    ;; print out the header
    (display "!PADS-POWERPCB-V3.0-MILS!\r\n")
    (display "\r\n*PART*\r\n")

    ;; print out the parts
    (pads:components packages)

    ;; print out the net information
    (display "\r\n*NET*\r\n")
    (pads:write-net nets)

    ;; print out the footer
    (display "\r\n*END*\r\n")))
