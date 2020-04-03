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

;; --------------------------------------------------------------------------
;;
;; TANGO netlist backend written by Nuno Sucena starts here
;;

(use-modules (srfi srfi-1)
             (netlist schematic)
             (netlist schematic toplevel))

;;
;; Given a uref, returns the device attribute value (for tango-netlist)
;;
(define (tango:get-device package)
  (gnetlist:get-package-attribute package "device"))

;;
;; Given a uref, returns the footprint attribute value (PATTERN if not defined)
;;
(define (tango:get-pattern package)
  (let ((pattern (gnetlist:get-package-attribute package "footprint")))
    (if (unknown? pattern) "PATTERN" pattern)))

;;
;; Given a uref, returns the value attribute (empty if not defined)
;;
(define (tango:get-value package)
  (let ((value (gnetlist:get-package-attribute package "value")))
    (if (unknown? value) "" value)))

;;
;; Top level component writing
;;
(define (tango:components ls)
  (for-each
   (lambda (package)
     (format #t "[\n~A\n~A\n~A\n~A\n\n]\n"
             package
             (tango:get-pattern package)
             (tango:get-device package)
             (tango:get-value package)))
   ls))

;;
;; Display the individual net connections
;;
(define (tango:display-connections nets)
  (define package car)
  (define pinnumber cdr)
  (string-join
   (map
    (lambda (net)
      (format #f
              "~A-~A\n"
              (package net)
              (pinnumber net)))
    nets)
   ""))


;;
;; Write out a net associated with a particular package and pin
;;
(define (tango:write-net netnames)
  (for-each
   (lambda (netname)
     (format #t "(\n~A\n~A)\n"
             netname
             (tango:display-connections (get-all-connections netname))))
   netnames))


;;
;; Top level function to write out nets associated with a particular component
;;
(define (tango:nets nets)
  (tango:write-net nets))

;;; Highest level function
;;; Write tango netlist format
;;;
(define (tango output-filename)
  (let ((nets (schematic-nets (toplevel-schematic)))
        (packages (schematic-package-names (toplevel-schematic))))
    (tango:components packages)
    (tango:nets nets)))

;;
;; TANGO netlist backend written by Nuno Sucena ends here
;;
;; --------------------------------------------------------------------------
