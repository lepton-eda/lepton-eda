;;; Lepton EDA netlister
;;; Copyright (C) 2016 gEDA Contributors
;;; Copyright (C) 2018-2025 Lepton EDA Contributors
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

(define-module (backend partslist2)
  #:use-module (netlist attrib compare)
  #:use-module (netlist partlist common)
  #:use-module (netlist partlist)
  #:use-module (netlist schematic toplevel)
  #:use-module (netlist schematic)

  #:export (partslist2))

(define (partslist2)
  (display
   (partlist->string
    (make-partlist (schematic-package-names (toplevel-schematic))
                   '(device value footprint refdes))
    #:sort-order `((device . ,string-ci<?)
                   (value . ,value<?)
                   (footprint . ,string-ci<?)
                   (refdes . ,refdes<?))
    #:output-order '(refdes device value footprint #{}#)
    #:header ".START\n.."
    #:footer "\n.END\n"
    #:prepend-names? #t
    #:remove '((device . "include"))
    )))
