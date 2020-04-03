;;; Lepton EDA netlister
;;; Copyright (C) 2004-2010 Braddock Gaskill (braddock@braddock.com,
;;;                                           adapted PCB code to Eagle)
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

;; EAGLE netlist format

(use-modules (srfi srfi-1)
             (netlist schematic)
             (netlist schematic toplevel))

;; This procedure takes a net name as determined by gnetlist and
;; modifies it to be a valid eagle net name.
(define (eagle:map-net-names net-name)
  (let ((net-alias net-name))
    ;; Convert to all upper case because Eagle seems
    ;; to do that internally anyway and we'd rather do
    ;; it here to catch shorts created by not preserving
    ;; case.  Plus we can eliminate lots of ECO changes
    ;; that will show up during backannotation.
    (string-upcase net-alias)))


(define (eagle:components packages)
  (for-each
   (lambda (package)
     (let ((pattern (gnetlist:get-package-attribute package "pattern"))
           ;; The above pattern should stay as "pattern" and not "footprint"
           (lib (gnetlist:get-package-attribute package "lib"))
           (value (gnetlist:get-package-attribute package "value"))
           (device (gnetlist:get-package-attribute package "device"))
           (footprint (gnetlist:get-package-attribute package "footprint")))

       (and (known? pattern)
            (display pattern))
       (format #t "ADD '~A' ~A@~A (1 1);\n"
               package
               footprint
               (if (known? lib) lib "smd-ipc"))

       (let ((val (or (and (known? value) value)
                      (and (known? device) device))))
         (when val
           (format #t "VALUE '~A' '~A';\n" package val)))))
   packages))

(define (eagle:display-connections connections)
  (define package car)
  (define pinnumber cdr)
  (string-join
   (map (lambda (connection)
          (format #f "   '~A' '~A'"
                  (package connection)
                  (pinnumber connection)))
        connections)
   "\r\n"
   'suffix))


(define (eagle:write-net netnames)
  (define (write-netname netname)
    (format #t "SIGNAL '~A'\n~A;\n"
            (gnetlist:alias-net netname)
            (eagle:display-connections (get-all-connections netname))))

  (for-each write-netname netnames))


(define (eagle output-filename)
  (let ((nets (schematic-nets (toplevel-schematic)))
        (packages (schematic-package-names (toplevel-schematic))))
    ;; initialize the net-name aliasing
    (gnetlist:build-net-aliases eagle:map-net-names nets)

    ;; print out the header
    (display "   ;\n")

    ;; print out the parts
    (eagle:components packages)

    ;; print out the net information
    (eagle:write-net nets)))
