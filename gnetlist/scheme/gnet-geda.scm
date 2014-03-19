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

;;
;; Top level header
;;
(define geda:write-top-header
   (lambda ()
      (display "START header")
      (newline)
      (newline)
      (display "gEDA's netlist format")
      (newline)
      (display "Created specifically for testing of gnetlist")
      (newline)
      (newline)
      (display "END header")
      (newline)
      (newline)))

;;
;; header for components section
;;
(define geda:start-components
   (lambda ()
      (display "START components")
      (newline)
      (newline)))

;;
;; footer for components section
;;
(define geda:end-components
   (lambda ()
      (newline)
      (display "END components")
      (newline)
      (newline)))

;;
;; header for renamed section
;;
(define geda:start-renamed-nets
   (lambda ()
      (display "START renamed-nets")
      (newline)
      (newline)))

;;
;; footer for renamed section
;;
(define geda:end-renamed-nets
   (lambda ()
      (newline)
      (display "END renamed-nets")
      (newline)
      (newline)))

;;
;; header for nets section
;;
(define geda:start-nets
   (lambda ()
      (display "START nets")
      (newline)
      (newline)))

;;
;; footer for net section
;;
(define geda:end-nets
   (lambda ()
      (newline)
      (display "END nets")
      (newline)
      (newline)))

;;
;; Top level component writing
;;
(define geda:components
   (lambda (ls)
      (if (not (null? ls))
         (let ((package (car ls)))
            (begin
               (display package)
               (write-char #\space)
               (display "device=")
               (display (get-device package))
               (newline)
               (geda:components (cdr ls)))))))

;;
;; renamed nets writing
;;
(define geda:renamed-nets
   (lambda (ls)
      (if (not (null? ls))
         (let ((renamed-pair (car ls)))
            (begin
;;;            (display renamed-pair) (newline)
               (display (car renamed-pair))
               (display " -> ")
               (display (car (cdr renamed-pair)))
               (newline)
               (geda:renamed-nets (cdr ls)))))))

;;
;; Display the individual net connections
;;
(define geda:display-connections
   (lambda (nets)
      (if (not (null? nets))
         (begin
            (display (car (car nets)))
            (write-char #\space)
            (display (car (cdr (car nets))))
            (if (not (null? (cdr nets)))
               (begin
                  (write-char #\,)
                  (write-char #\space)))
               (geda:display-connections (cdr nets))))))

;;
;; Display all nets
;;
(define geda:display-name-nets
   (lambda (nets)
      (begin
         (geda:display-connections nets)
         (write-char #\space)
         (newline))))

;;
;; Write netname : uref pin, uref pin, ...
;;
(define geda:write-net
   (lambda (netnames)
      (if (not (null? netnames))
         (let ((netname (car netnames)))
            (begin
               (display netname)
               (display " : ")
               (geda:display-name-nets (gnetlist:get-all-connections netname))
               (geda:write-net (cdr netnames)))))))

;;
;; Write the net part of the gEDA format
;;
(define geda:nets
   (lambda ()
      (let ((all-uniq-nets (gnetlist:get-all-unique-nets "dummy")))
         (geda:write-net all-uniq-nets))))

;;; Highest level function
;;; Write my special testing netlist format
;;;
(define (geda output-filename)
  (set-current-output-port (gnetlist:output-port output-filename))
  (begin
    (geda:write-top-header)
    (geda:start-components)
    (geda:components packages)
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
