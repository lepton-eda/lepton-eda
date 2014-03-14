;;; gEDA - GPL Electronic Design Automation
;;; gnetlist - gEDA Netlist
;;; Copyright (C) 1998-2010 Ales Hvezda
;;; Copyright (C) 1998-2010 gEDA Contributors (see ChangeLog for details)
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
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;;; MA 02111-1301 USA.

;; --------------------------------------------------------------------------
;;
;; Bartels Format
;; Layout board;
;; PARTS
;;   part : footprint;
;; CONNECT
;;   /net1/ uref.pin=uref.pin=uref.pin=...uref.pin;
;;   /net2/ PRIORITY(1..100) MINDIST(mm) ROUTWIDTH(mm) uref.pin(width_mm)=...;
;; END.
;;

;;
;; Top level header
;;
(define (bae:write-top-header)
  (display "LAYOUT board;")
  (newline))

;;
;; header for components section
;;
(define (bae:start-components)
  (display "PARTS")
  (newline))
;; no header for components

;;
;; header for nets section
;;
(define (bae:start-nets)
  (display "CONNECT")
  (newline))

;;
;; footer for net section
;;
(define (bae:end-nets)
  (display "END.")
  (newline))

;;
;; Top level component writing
;;
(define (bae:components ls)
  (if (not (null? ls))
     (let ((package (car ls)))
        (begin
           (display "    ")
           (display package)
           (display " : ")
           (display (gnetlist:get-package-attribute package  "footprint"))
           (display ";")
           (newline)
           (bae:components (cdr ls))))))

;;
;; Display the individual net connections
;;
(define (bae:display-connections nets)
  (if (not (null? nets))
     (begin
        (let ((package (car (car nets))))
           (display package)
           (write-char #\.)
           (display (car (cdr (car nets)))))
        (if (not (null? (cdr nets)))
           (begin
              (display #\=)))
        (bae:display-connections (cdr nets)))))

;;
;; Display all nets
;;
(define (bae:display-name-nets nets)
  (begin
     (bae:display-connections nets)
     (write-char #\;)))

;;
;; Write netname : uref pin, uref pin, ...
;;
(define (bae:write-net netnames)
  (if (not (null? netnames))
    (let ((netname (car netnames)))
      (begin
        (display "    ")
        (display "/'")
        (display netname)
        (display "'/ ")
        (bae:display-name-nets (gnetlist:get-all-connections netname))
        (newline)
        (bae:write-net (cdr netnames))))))

;;
;; Write the net part of the gEDA format
;;
(define (bae:nets)
  (let ((all-uniq-nets (gnetlist:get-all-unique-nets "dummy")))
     (bae:write-net all-uniq-nets)))

;;; Highest level function
;;; Write my special testing netlist format
;;;
(define (bae output-filename)
  (set-current-output-port (gnetlist:output-port output-filename))
    (begin
      (bae:write-top-header)
      (bae:start-components)
      (bae:components packages)
      (bae:start-nets)
      (bae:nets)
      (bae:end-nets))
    (close-output-port (current-output-port)))

;;
;; gEDA's native test netlist format specific functions ends
;;
;; --------------------------------------------------------------------------
