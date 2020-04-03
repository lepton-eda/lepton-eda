;;; Lepton EDA netlister
;;; Copyright (C) 1998-2010 Ales Hvezda
;;; Copyright (C) 1998-2017 gEDA Contributors
;;; Copyright (C) 2019 Lepton EDA Contributors
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
;; SPICE netlist backend written by S. Gieltjes starts here
;;

(use-modules (netlist schematic)
             (netlist schematic toplevel)
             (spice common))


;;  write mos transistor
;;
(define spice:write-mos-transistor
  (lambda (package)
    (spice:write-one-component package)
            ;; create list of attributes which can be attached to a mosfet
    (let ((attrib-list (list "l" "w" "as" "ad" "pd" "ps" "nrd" "nrs" "temp" "ic")))
      (display (string-join (spice:format-attrib-list package attrib-list)
                            " " 'prefix)))
            ;; write the off attribute separately
    (let ((off-value (gnetlist:get-package-attribute package "off")))
      (cond ((string=? off-value "#t") (display " off"))
            ((string=? off-value "1" ) (display " off"))))
    (newline)))


;;
;; Include a file
;;
(define spice:write-include
  (lambda (package)
    (display (string-append package " " (spice:component-value package) "\n"))))


;;
;; write the refdes, the net name connected to pin# and the component value. No extra attributes.
;;
(define spice:write-one-component
  (lambda (package)
    (display (string-append package " "))
        ;; write net names, slotted components not implemented
    (spice:write-net-names-on-component package)
        ;; write component value, if components have a label "value=#"
        ;; what if a component has no value label, currently unknown is written
    (display (spice:component-value package))))


;;
;; write the refdes, to the pin# connected net and component value and optional extra attributes
;; check if the component is a special spice component
;;
(define spice:write-netlist
  (lambda (ls)
     (if (not (null? ls))
      (let ((package (car ls)))                           ;; search for specific device labels
        (cond
          ( (string=? (get-device package) "SPICE-ccvs")
              (spice:write-ccvs package))
          ( (string=? (get-device package) "SPICE-cccs")
              (spice:write-cccs package))
          ( (string=? (get-device package) "SPICE-vcvs")
              (spice:write-vcvs package))
          ( (string=? (get-device package) "SPICE-vccs")
              (spice:write-vccs package))
          ( (string=? (get-device package) "SPICE-nullor")
              (spice:write-nullor package))
          ( (string=? (get-device package) "PMOS_TRANSISTOR")
              (spice:write-mos-transistor package))
          ( (string=? (get-device package) "NMOS_TRANSISTOR")
              (spice:write-mos-transistor package))
          ( (string=? (get-device package) "include")
              (spice:write-include package))
          ( else (spice:write-one-component package)
               (newline)))
        (spice:write-netlist (cdr ls)) ))))


;;
;; Spice netlist header
;;
(define (spice:write-top-header)
  (display "* SPICE Lepton EDA netlister backend\n"))


;;
;; Write the .END line
;;
(define (spice:write-bottom-footer)
  (display ".END")
  (newline))


; public:
; Instruct the netlister to use 'spice mode
;
( define ( request-netlist-mode )
  ; return:
  'spice
)


;;
;; Spice netlist generation
;;
(define (spice output-filename)
  (spice:write-top-header)
  (spice:write-netlist (schematic-package-names (toplevel-schematic)))
  (spice:write-bottom-footer))


;; SPICE netlist backend written by S. Gieltjes ends here
;;
;; --------------------------------------------------------------------------
