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

;; MAXASCII netlist format

(use-modules (netlist schematic)
             (netlist schematic toplevel))

(define maxascii:components
   (lambda (packages)
      (if (not (null? packages))
         (begin
            (let ((pattern (gnetlist:get-package-attribute (car packages)
                                                           "footprint"))
                  (package (car packages)))
;               (if (not (string=? pattern "unknown"))
;                  (display pattern))
               (display "*COMP ")
               (display package)
               (write-char #\tab)
               (display "\"")
               (display (gnetlist:get-package-attribute package "footprint"))
               (display "\"")
               (newline))
            (maxascii:components (cdr packages))))))

(define (connections->string connections)
  (define package car)
  (define pinnumber cdr)
  (define (connection->string connection)
    (format #f "~A.\"~A\"" (package connection) (pinnumber connection)))
  (string-join (map connection->string connections) " "))


;;
;; Wrap a string into lines no longer than wrap-length
;; (from Stefan Petersen)
(define (maxascii:wrap string-to-wrap wrap-length netname)
  (if (> wrap-length (string-length string-to-wrap))
      string-to-wrap ; Last snippet of string
      (let ((pos (string-rindex string-to-wrap #\space 0 wrap-length)))
        (cond ((not pos)
               (message "Couldn't wrap string  at requested position\n")
               " Wrap error!")
              (else
               (string-append
                (substring string-to-wrap 0 pos)
                " \n*NET \"" netname "\" "
                (maxascii:wrap (substring string-to-wrap (+ pos 1)) wrap-length netname)))))))



(define (maxascii:write-net netnames)
  (for-each
   (lambda (netname)
     (format #t "*NET \"~A\"\n" netname)
     (format #t "*NET \"~A\" ~A\n"
             netname
             (maxascii:wrap
              (connections->string
               (get-all-connections netname))
              490 netname)))
   netnames))

(define (maxascii output-filename)
  (display "*OrCAD\n*START\n")

  (maxascii:components (schematic-package-names (toplevel-schematic)))

  (maxascii:write-net (schematic-nets (toplevel-schematic)))
  (display "\n*END\n"))
