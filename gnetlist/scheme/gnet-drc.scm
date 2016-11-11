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
;; DRC backend written by Matt Ettus starts here
;;

;; DRC rules format:  (list (part rules) (net rules) (pin rules))
;; Part rules:  List of predicates of one variable, uref
;; Net rules:  List of predicates of one variable, net name
;; Pin Rules:  List of predicates of 2 variables, uref and pin number

(use-modules (gnetlist attrib compare))

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
       ((message (string-append "ERROR: Attribute file '" filename "' not found.\n"))
        (primitive-exit 1))))
   "attribs"))

(define (drc output-filename)
  (with-output-to-port (gnetlist:output-port output-filename)
    (lambda ()
      (let ((packages (sort packages refdes<?)))
        (drc:device-rules drc:attriblist packages)
        (drc:net-rules (gnetlist:get-all-unique-nets "dummy"))
        (drc:pin-rules packages)))))


(define drc:net-rules
  (lambda(nets)
    (cond
      ((null? nets) #t)
      ((null? (gnetlist:get-all-connections (car nets)))
          (begin
            (display "Net ")
            (display (car nets))
            (display " has no connected pins\n")
            (drc:net-rules (cdr nets))
            #f))
      ((null? (cdr (gnetlist:get-all-connections (car nets))))
          (begin
            (display "Net ")
            (display (car nets))
            (display " has only 1 connected pin\n")
            (drc:net-rules (cdr nets))
            #f))
      (#t (drc:net-rules (cdr nets))))))

(define drc:pin-rules
  (lambda(packages)
    #t))

(define drc:device-rules
  (lambda (attriblist packages)
    (if (not (null? packages))
      (begin
        (drc:has-attributes? attriblist (car packages))
        (drc:device-rules attriblist (cdr packages))))))

(define drc:has-attributes?
  (lambda (attriblist uref)
    (if (not (null? attriblist))
      (begin
        (if (string=? "unknown" (gnetlist:get-package-attribute uref (car attriblist)))
          (begin
            (display uref)
            (display " Does not have attribute: ")
            (display (car attriblist))
            (newline)))
        (drc:has-attributes? (cdr attriblist) uref)))))


;;
;; DRC backend written by Matt Ettus ends here
;;
;; --------------------------------------------------------------------------
