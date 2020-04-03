;;; Lepton EDA netlister
;;; tEDAx plug-in for lepton-netlist
;;; Copyright (C) 2018 Bdale Garbee
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
;; The tEDAx format is documented at http://repo.hu/projects/tedax/
;; --------------------------------------------------------------------------

(use-modules (netlist schematic)
             (netlist schematic toplevel))

;;
;; return device attribute
;;
(define (tEDAx:get-device package)
  (gnetlist:get-package-attribute package "device"))

;;
;; return footprint attribute (UNKNOWN if not defined)
;;
(define (tEDAx:get-pattern package)
  (let ((pattern (gnetlist:get-package-attribute package "footprint")))
    (if (unknown? pattern) "UNKNOWN" pattern)))

;;
;; returns value attribute (empty if not defined)
;;
(define (tEDAx:get-value package)
  (let ((value (gnetlist:get-package-attribute package "value")))
    (if (unknown? value) "" value)))

;;
;; emit header
;;
(define (tEDAx:header)
  (format #t "tEDAx v1\nbegin netlist v1 netlist\n\n"))

;;
;; emit trailer
;;
(define (tEDAx:trailer)
  (format #t "end netlist\n"))

;;
;; emit component related lines
;;
(define (tEDAx:components ls)
  (for-each
   (lambda (package)
     (format #t "\tfootprint ~A ~A\n\tdevice ~A ~A\n\tvalue ~A ~A\n\n"
       package
       (tEDAx:get-pattern package)
       package
       (tEDAx:get-device package)
       package
       (tEDAx:get-value package)))
   ls))

;;
;; emit network related lines for current net
;;
(define (tEDAx:display-connections netname nets)
  (define package car)
  (define pinnumber cdr)
  (string-join
   (map
    (lambda (net)
      (format #f "\tconn ~A ~A ~A\n"
	netname
        (package net)
        (pinnumber net)))
    nets)
   ""))

;;
;; iterate over all nets
;;
(define (tEDAx:nets netnames)
  (for-each
   (lambda (netname)
     (format #t "~A\n"
       (tEDAx:display-connections netname (get-all-connections netname))))
   netnames))

;;;
;;; emit netlist in tEDAx interchange format
;;;
(define (tEDAx output-filename)
  (let ((nets (schematic-nets (toplevel-schematic)))
        (packages (schematic-package-names (toplevel-schematic))))
    (tEDAx:header)
    (tEDAx:components packages)
    (tEDAx:nets nets)
    (tEDAx:trailer)))

;; --------------------------------------------------------------------------
