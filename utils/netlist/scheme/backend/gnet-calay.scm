;;; Lepton EDA netlister
;;; Copyright (C) 1998-2010 Ales Hvezda
;;; Copyright (C) 2006-2010 John P. Doty
;;; Copyright (C) 2006-2017 gEDA Contributors
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

;;  Calay format (modified from Ales's gnet-PCB.scm by jpd)
;;  Netname translation cleaned up at Dan McMahill'suggestion -jpd
(use-modules (netlist schematic)
             (netlist schematic toplevel))

(define (connections->string connections)
  (define package car)
  (define pinnumber cdr)
  (define (connection->string connection)
    (format #f "~A(~A)" (package connection) (pinnumber connection)))
  (string-join (map connection->string connections)))

;;
;; Wrap a string into lines no longer than wrap-length
;; (from Stefan Petersen)
;; (Modified for Calay format by jpd)
(define (calay:wrap string-to-wrap wrap-length)
  (if (> wrap-length (string-length string-to-wrap))
      string-to-wrap ; Last snippet of string
      (let ((pos (string-rindex string-to-wrap #\space 0 wrap-length)))
        (cond ((not pos)
               (display "Couldn't wrap string  at requested position\n")
               " Wrap error!")
              (else
               (string-append
                (substring string-to-wrap 0 pos)
                ",\n          "
                (calay:wrap (substring string-to-wrap (+ pos 1)) wrap-length)))))))

;; Translate netnames
;; For the nonce, this just turns "_" into "-"
(define (calay:translate s)
  (string-join (string-split s #\_) "-"))


(define (net->string netname)
  (let ((connections (get-all-connections netname)))
   (format #f "/~A\t~A;\n"
           (gnetlist:alias-net netname)
           (calay:wrap (connections->string connections) 66))))

(define (nets->calay-netlist nets)
  (map net->string nets))

(define (calay output-filename)
  (let ((nets (schematic-nets (toplevel-schematic))))
    (gnetlist:build-net-aliases calay:translate nets)
    (for-each display (nets->calay-netlist nets))))
