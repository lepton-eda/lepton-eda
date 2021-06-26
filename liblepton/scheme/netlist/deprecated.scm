;;; Lepton EDA Netlister
;;; Copyright (C) 2018-2021 Lepton EDA Contributors
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

;;; Deprecated *gnetlist* procedures and variables, and a
;;; lepton-netlist setter for them.

(define-module (netlist deprecated)
  #:use-module (srfi srfi-1)
  #:use-module (netlist attrib compare)
  #:use-module (netlist config)
  #:use-module (netlist duplicate)
  #:use-module (netlist option)
  #:use-module (netlist package-pin)
  #:use-module (netlist schematic-component)
  #:use-module (netlist schematic)
  #:use-module (netlist schematic toplevel)

  #:export (;; deprecated procedures
            get-pins
            gnetlist:get-pins
            gnetlist-option-ref
            ;; deprecated variables
            non-unique-packages
            packages
            all-unique-nets
            all-nets
            all-pins
            ;; deprecated variables setter
            set-deprecated-schematic-variables!))

;;; Backward compatibility procedures.
(define gnetlist-option-ref netlist-option-ref)
(define gnetlist-config-ref netlist-config-ref)
(define print-gnetlist-config print-netlist-config)

(define (get-pins refdes)
  (define (found? x)
    (and x
         (string=? x refdes)))

  (sort-remove-duplicates
   (append-map
    (lambda (package)
      (if (found? (schematic-component-refdes package))
          (filter-map package-pin-number (schematic-component-pins package))
          '()))
    (schematic-components (toplevel-schematic)))
   refdes<?))

;;; Alias for get-pins().
(define gnetlist:get-pins get-pins)


;;; Backward compatibility variables.
(define non-unique-packages #f)
(define packages #f)
(define all-unique-nets #f)
(define all-nets #f)
(define all-pins #f)


;;; Setter for backward compatibility variables.
(define (set-deprecated-schematic-variables! schematic)
  "Set deprecated variables 'non-unique-packages', 'packages',
'all-unique-nets', 'all-nets', 'all-pins' for given
SCHEMATIC. Returns SCHEMATIC."
  (set! non-unique-packages (schematic-non-unique-package-names schematic))
  (set! packages (schematic-package-names schematic))
  (set! all-unique-nets (schematic-nets schematic))
  (set! all-nets (schematic-non-unique-nets schematic))
  (set! all-pins (map gnetlist:get-pins packages))
  schematic)
