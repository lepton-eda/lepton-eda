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
;; DRC backend written by Matt Ettus starts here
;;

;; DRC rules format:  (list (part rules) (net rules) (pin rules))
;; Part rules:  List of predicates of one variable, uref
;; Net rules:  List of predicates of one variable, net name
;; Pin Rules:  List of predicates of 2 variables, uref and pin number

(use-modules (srfi srfi-26)
             (netlist error)
             (netlist schematic)
             (netlist schematic toplevel))

(define drc:parseconfig
  (lambda (port)
    (let ((read-from-file (read port)))
      (if (not (eof-object? read-from-file))
          (cons (symbol->string read-from-file) (drc:parseconfig port))
          '()))))

(define drc:attriblist
  ((lambda (filename)
     (if (file-exists? filename)
       (drc:parseconfig
         (open-input-file filename))
       (netlist-error 1 "ERROR: Attribute file ~S not found.\n" filename)))
   "attribs"))


;;; Checks connections of NETS.
(define (drc:net-rules nets)
  (for-each
   (lambda (net)
     (case (length (get-all-connections net))
       ((0) (format #t "Net ~A has no connected pins\n" net))
       ((1) (format #t "Net ~A has only 1 connected pin\n" net))))
   nets))


(define (drc:device-rules attribs packages)
  (define (drc:has-attributes? attribs package)
    (for-each
     (lambda (attrib)
       (if (unknown? (gnetlist:get-package-attribute package attrib))
           (format #t "~A Does not have attribute: ~A\n" package attrib)))
     attribs))

  (for-each (cut drc:has-attributes? attribs <>) packages))


(define (drc output-filename)
  (drc:device-rules drc:attriblist (schematic-package-names (toplevel-schematic)))
  (drc:net-rules (schematic-nets (toplevel-schematic))))

;;
;; DRC backend written by Matt Ettus ends here
;;
;; --------------------------------------------------------------------------
