;;; Lepton EDA netlister
;;; Copyright (C) 1998-2014 Ales Hvezda
;;; Copyright (C) 1998-2016 gEDA Contributors
;;; Copyright (C) 2018-2020 Lepton EDA Contributors
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
;;
;; --------------------------------------------------------------------------
;; Common functions for the SPICE netlist backends `spice' and `spice-sdb'.
;; By S. Gieltjes and others.
;; --------------------------------------------------------------------------

(define-module (spice common)
  #:use-module (srfi srfi-1)
  #:use-module (netlist)
  #:use-module (netlist deprecated)
  #:use-module (netlist schematic)
  #:use-module (netlist schematic toplevel)
  #:export (spice:write-net-names-on-component)
  #:export (spice:write-ccvs)
  #:export (spice:write-cccs)
  #:export (spice:write-vccs)
  #:export (spice:write-vcvs)
  #:export (spice:write-nullor)
  #:export (spice:format-attrib-list)
  #:export (spice:component-value)
  #:export (spice:get-net)
)


;;---------------------------------------------------------------------
;; write netnames connected to pin-a and pin-b
;;   (currently used by the controlled sources (e, g, f and h)
;;---------------------------------------------------------------------
(define spice:write-two-pin-names
  (lambda (package pin-a pin-b)
    (display (string-append
              (spice:get-net package (gnetlist:get-attribute-by-pinseq package pin-a "pinnumber")) " "))
    (display (string-append
              (spice:get-net package (gnetlist:get-attribute-by-pinseq package pin-b "pinnumber")) " "))))


;;--------------------------------------------------------------------
;; Given a refdes returns the device associated nets(s) ordered by
;; their pin number,
;; what when not defined?
;;      problem is slotted components e.g. ../examples/singlenet_1.sch
;;--------------------------------------------------------------------
(define (spice:write-net-names-on-component refdes)
  (do ((i 1 (1+ i)))
      ((> i  (length (get-pins refdes))))
    (let ((pin-name (number->string i)))
      (display (spice:get-net refdes (gnetlist:get-attribute-by-pinseq refdes pin-name "pinnumber")))
      (write-char #\space))))


;;----------------------------------------------------------------
;; write a current controlled voltage source and implement the necessary
;;   current measuring voltage source
;;----------------------------------------------------------------
(define spice:write-ccvs
  (lambda (package)
    ( begin
      (display "* begin ccvs expansion, h<name>\n")
          ;; implement the controlled current source
          ;; the user should create the refdes label begining with a h
      (display (string-append package " "))
      (spice:write-two-pin-names package "1" "2")
      (display (string-append "Vsense_" package  " " (spice:component-value package) "\n" ))
          ;; implement the current measuring voltage source
      (display (string-append "Vsense_" package " "))
      (spice:write-two-pin-names package "3" "4")
      (display "dc 0\n")
          ;; now it is possible to leave the output voltage source unconnected
          ;; i.e. spice won't complain about unconnected nodes
      (display (string-append "IOut_" package " "))
      (spice:write-two-pin-names package "1" "2")
      (display "dc 0\n")
      (display "* end ccvs expansion\n"))))


;;-----------------------------------------------------------------------
;; write a current controlled current source and implement the necessary
;;   current measuring voltage source
;;-----------------------------------------------------------------------
(define spice:write-cccs
  (lambda (package)
    ( begin
      (display "* begin cccs expansion, f<name>\n")
          ;; implement the controlled current source
          ;; the user should create the refdes label begining with a f
      (display (string-append package " "))
      (spice:write-two-pin-names package "1" "2")
      (display (string-append "Vsense_" package " " (gnetlist:get-package-attribute package "value") "\n" ))
          ;; implement the current measuring voltage source
      (display (string-append "Vsense_" package " "))
      (spice:write-two-pin-names package "3" "4")
      (display "dc 0\n")
      (display "* end cccs expansion\n"))))


;;-------------------------------------------------------------------------
;; write a voltage controlled current source and implement the necessary
;;   voltage measuring current source
;;-------------------------------------------------------------------------
(define spice:write-vccs
  (lambda (package)
    ( begin
      (display "* begin vccs expansion, g<name>\n")
          ;; implement the controlled current source
          ;; the user should create a refdes label beginning with a g
      (display (string-append package " "))
      (spice:write-net-names-on-component package)
      (display (string-append (spice:component-value package) "\n"))
          ;; implement the voltage measuring current source
          ;; imagine yourself copying the voltage of a voltage source with an internal
          ;; impedance, spice starts complaining about unconnected nets if this current
          ;; source is not here.
      (display (string-append "IMeasure_" package " "))
      (spice:write-two-pin-names package "3" "4")
      (display "dc 0\n")
      (display "* end vccs expansion\n"))))


;;------------------------------------------------------------------------
;; write a voltage controlled voltage source and implement the necessary
;;   voltage measuring current source
;;------------------------------------------------------------------------
(define spice:write-vcvs
  (lambda (package)
    ( begin
      (display "* begin vcvs expansion, e<name>\n")
          ;; implement the controlled voltage source
          ;; the user should create a refdes label beginning with an e
      (display (string-append package " "))
      (spice:write-net-names-on-component package)
      (display (string-append (gnetlist:get-package-attribute package "value") "\n" ))
          ;; implement the voltage measuring current source
          ;; imagine yourself copying the voltage of a voltage source with an internal
          ;; impedance, spice starts complaining about unconnected nets if this current
          ;; source is not here.
      (display (string-append "Isense_" package " "))
      (spice:write-two-pin-names package "3" "4")
      (display "dc 0\n")
          ;; with an output current source it is possible to leave the output voltage source
          ;; unconnected i.e. spice won't complain about unconnected nodes
      (display (string-append "IOut_" package " "))
      (spice:write-two-pin-names package "1" "2")
      (display "dc 0\n")
      (display "* end vcvs expansion\n"))))


;;--------------------------------------------------------------------------
;; Create a nullor, make sure it consists of a voltage controlled source
;;--------------------------------------------------------------------------
(define spice:write-nullor
  (lambda (package)
    (let ((value (gnetlist:get-package-attribute package "value")))
      (display "* begin nullor expansion, e<name>\n")
          ;; implement the controlled voltage source
      (display (string-append "E_" package " "))
      (spice:write-net-names-on-component package)
      (display (string-append (if (unknown? value) "1000Meg" value) "\n"))
          ;; implement the voltage measuring current source
          ;; imagine yourself copying the voltage of a voltage source with an internal
          ;; impedance, spice starts complaining about unconnected nets if this current
          ;; source is not here.
      (display (string-append "IMeasure_" package " "))
      (spice:write-two-pin-names package "3" "4")
      (display "dc 0\n")
          ;; with an output current source it is possible to leave the output voltage source
          ;; unconnected i.e. spice won't complain about unconnected nodes
      (display (string-append "IOut_" package " "))
      (spice:write-two-pin-names package "1" "2")
      (display "dc 0\n")
      (display "* end of nullor expansion\n"))))


;;; Given the list of attribute names ATTRIB-LIST returns
;;; attributes of PACKAGE as the list of strings in the form
;;; "<attrib-name>=<attrib-value>". Unknown attributes are
;;; filtered out.
(define (spice:format-attrib-list package attrib-list)
  (filter-map
   (lambda (attrib)
     (let ((attrib-value (gnetlist:get-package-attribute package attrib)))
       ;; Is it possible to make no differentiation between upper and lower case?
       ;; That relieves you of mixed case forms e.g. As, AS, as..., they are the
       ;; same attributes, spice3f5 is case insensitive.  And other spice versions?
       (and (not (unknown? attrib-value))
            (string-append attrib "=" attrib-value))))
   attrib-list))


;;-----------------------------------------------------------
;; Given a refdes, returns the device attribute "value" as string
;; Used when "value" is a mandatory attribute.
;; Returns "<no valid attribute . . .>" if not available.
;;-----------------------------------------------------------
(define spice:component-value
  (lambda (package)
    (let ((value (gnetlist:get-package-attribute package "value")))
      (if (not (unknown? value))
        value
        "<No valid value attribute found>"))))

;;-----------------------------------------------------------
;; gnet-spice replacement of get-nets, a net labeled "GND" becomes 0
;;-----------------------------------------------------------
(define (spice:get-net package pin-name)
  (let ((net-name (pin-netname package pin-name)))
    (if (string=? net-name "GND")
        "0"
        net-name)))
