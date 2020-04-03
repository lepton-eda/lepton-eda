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
;; Netlister for GOSSIP system simulation system, based on GUILE
;;  For more info see http://gossip.sourceforge.net
;;

(use-modules (netlist schematic)
             (netlist schematic toplevel))

(define (gossip:write-top-header)
  (display ";; Gossip Netlist Created by gNetlist

;; Created By Matt Ettus <matt@ettus.com>
;; Libraries:

"))

(define (gossip:get-libraries components done)
  (if (not (null? components))
      (let ((lib (gnetlist:get-package-attribute (car components) "library")))
        (if (string=? "unknown" lib)
            (message (format #f "Component ~A does not have a library attribute\n"
                             (car components))))
        (if (member lib done)
            (gossip:get-libraries (cdr components) done)
            (begin
              (format #t "(use-library ~A *)\n" lib)
              (gossip:get-libraries (cdr components) (cons lib done)))))))

(define (gossip:list-pins allnets uref pin)
  (let ((pinname (gnetlist:get-attribute-by-pinnumber uref (number->string pin) "label")))
    (if (string=? "unknown" pinname)
        (display ")\n")
        (begin
          (display " :")
          (display pinname)
          (write-char #\space)
          (display (or (gossip:find-net uref pin allnets)
                       "Not Connected"))
          (gossip:list-pins allnets uref (+ 1 pin))))))

(define (gossip:find-net uref pin allnets)
  (and (not (null? allnets))
       (if (gossip:finder uref pin (get-all-connections (car allnets)))
           (car allnets)
           (gossip:find-net uref pin (cdr allnets)))))

(define (gossip:finder uref pin connections)
  (define package car)
  (define pinnumber cdr)
  (and (not (null? connections))
       (let ((connection (car connections)))
         (or (and (string=? uref (package connection))
                  (string=? (number->string pin) (pinnumber connection)))
             (gossip:finder uref pin (cdr connections))))))

(define (gossip:display-connections nets)
  (if (not (null? nets))
      (begin
        (display (car (car nets)))
        (write-char #\space)
        (display (car (cdr (car nets))))
        (if (not (null? (cdr nets)))
            (begin
              (write-char #\,)
              (write-char #\space)))
        (gossip:display-connections (cdr nets)))))

(define (gossip:display-name-nets nets)
  (gossip:display-connections nets)
  (write-char #\space)
  (newline))

(define (gossip:blocks ls allnets)
  (if (not (null? ls))
      (let ((package (car ls)))
        (display "   (")
        (display package)
        (gossip:list-pins allnets package 1)
        (gossip:blocks (cdr ls) allnets))))

(define (gossip:signals nets)
  (format #t "(signals ~A)\n" nets))

(define (gossip:write-block-header blockname)
  (format #t "(define-block (~A (\n" blockname))

(define (gossip output-filename)
  (let ((nets (schematic-nets (toplevel-schematic)))
        (packages (schematic-package-names (toplevel-schematic)))
        (blockname (or (schematic-toplevel-attrib (toplevel-schematic) 'blockname)
                       "not found")))
    (gossip:write-top-header)
    (gossip:get-libraries packages '())
    (gossip:write-block-header blockname)
    (gossip:signals nets)
    (gossip:blocks packages nets)))
