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
;; gEDA's native test netlist format specific functions go here
;;

(use-modules (srfi srfi-1)
             (gnetlist attrib compare))

;;
;; Top level header
;;
(define (geda:write-top-header)
  (format #t "START header

gEDA's netlist format
Created specifically for testing of gnetlist

END header

"))

;;
;; header for components section
;;
(define (geda:start-components)
  (display "START components\n\n"))

;;
;; footer for components section
;;
(define (geda:end-components)
  (display "\nEND components\n\n"))

;;
;; header for renamed section
;;
(define (geda:start-renamed-nets)
  (display "START renamed-nets\n\n"))

;;
;; footer for renamed section
;;
(define (geda:end-renamed-nets)
  (display "\nEND renamed-nets\n\n"))

;;
;; header for nets section
;;
(define (geda:start-nets)
  (display "START nets\n\n"))

;;
;; footer for net section
;;
(define (geda:end-nets)
  (display "\nEND nets\n\n"))

;;
;; Top level component writing
;;
(define (geda:components ls)
  (for-each
   (lambda (package)
     (format #t "~A device=~A\n" package (get-device package)))
   ls))

;;
;; renamed nets writing
;;
(define (geda:renamed-nets ls)
  (for-each
   (lambda (renamed-pair)
     (format #t "~A -> ~A\n" (first renamed-pair) (second renamed-pair)))
   ls))

;;
;; Display the individual net connections
;;
(define (geda:display-connections nets)
  (string-join
   (map
    (lambda (net)
      (format #f "~A ~A" (first net) (second net)))
    nets) ", "))

;;
;; Display all nets
;;
(define (geda:display-name-nets nets)
  (format #f "~A \n" (geda:display-connections nets)))

;;
;; Write netname : uref pin, uref pin, ...
;;
(define (geda:write-net netnames)
  (for-each
   (lambda (netname)
     (format #t "~A : ~A"
             netname
             (geda:display-name-nets (gnetlist:get-all-connections netname))))
   netnames))

;;
;; Write the net part of the gEDA format
;;
(define (geda:nets)
  (geda:write-net (gnetlist:get-all-unique-nets "dummy")))

;;; Highest level function
;;; Write my special testing netlist format
;;;
(define (geda output-filename)
  (set-current-output-port (gnetlist:output-port output-filename))
  (begin
    (geda:write-top-header)
    (geda:start-components)
    (geda:components (sort packages refdes<?))
    (geda:end-components)
    (geda:start-renamed-nets)
    (geda:renamed-nets (gnetlist:get-renamed-nets "dummy"))
    (geda:end-renamed-nets)
    (geda:start-nets)
    (geda:nets)
    (geda:end-nets))
  (close-output-port (current-output-port)))

;;
;; gEDA's native test netlist format specific functions ends
;;
;; --------------------------------------------------------------------------
