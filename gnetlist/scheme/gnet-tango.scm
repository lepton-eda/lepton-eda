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
;; TANGO netlist backend written by Nuno Sucena starts here
;;

(use-modules (gnetlist attrib compare))

;;
;; Given a uref, returns the device attribute value (for tango-netlist)
;;
(define tango:get-device
   (lambda (package)
      (gnetlist:get-package-attribute package "device")))

;;
;; Given a uref, returns the footprint attribute value (PATTERN if not defined)
;;
(define tango:get-pattern
   (lambda (package)
      (define pattern (gnetlist:get-package-attribute package "footprint"))
      (if (string=? "unknown" pattern)
         "PATTERN"
         pattern)))
; how do i return "PATTERN" if not defined? humm... need to read some
; guile stuff... i did, and see the result :)

;;
;; Given a uref, returns the value attribute (empty if not defined)
;;
(define tango:get-value
   (lambda (package)
      (define value (gnetlist:get-package-attribute package "value"))
      (if (string=? "unknown" value)
         ""
         value)))

;;
;; Top level component writing
;;
(define tango:components
   (lambda (ls)
      (if (not (null? ls))
         (let ((package (car ls)))
            (begin
               (display "[")
               (newline)
               (display package)
               (newline)
               (display (tango:get-pattern package))
               (newline)
               (display (tango:get-device package))
               (newline)
               (display (tango:get-value package))
               (newline)
               (newline)
               (display "]")
               (newline)
               (tango:components (cdr ls)))))))

;;
;; Display the individual net connections
;;
(define tango:display-connections
   (lambda (nets)
      (if (not (null? nets))
         (begin
            (display (car (car nets)))
            (display "-")
            (display (car (cdr (car nets))))
            (if (not (null? (cdr nets)))
                (begin
                  (newline)))
                  (tango:display-connections (cdr nets))))))


;;
;; Properly format the name of the net and the actual net connections
;;
(define tango:display-name-nets
   (lambda (nets)
      (begin
         (tango:display-connections nets))))

;;
;; Write out a net associated with a particular package and pin
;;
(define tango:write-net
   (lambda (netnames)
      (if (not (null? netnames))
         (let ((netname (car netnames)))
            (begin
               (display "(")
               (newline)
               (display netname)
               (newline)

               (tango:display-name-nets (gnetlist:get-all-connections netname))
               (newline)
               (display ")")
               (newline)
               (tango:write-net (cdr netnames)))))))


;;
;; Top level function to write out nets associated with a particular component
;;
(define (tango:nets)
  (let ((all-uniq-nets (gnetlist:get-all-unique-nets "dummy")))
     (tango:write-net all-uniq-nets)))

;;; Highest level function
;;; Write tango netlist format
;;;
(define (tango output-filename)
  (with-output-to-port (gnetlist:output-port output-filename)
    (lambda ()
      (tango:components (sort packages refdes<?))
      (tango:nets))))

;;
;; TANGO netlist backend written by Nuno Sucena ends here
;;
;; --------------------------------------------------------------------------
