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
;; Netlister for GOSSIP system simulation system, based on GUILE
;;  For more info see http://gossip.sourceforge.net
;;

(define gossip:write-top-header
   (lambda ()
      (display ";; Gossip Netlist Created by gNetlist")
      (newline)
      (newline)
      (display ";; Created By Matt Ettus <matt@ettus.com>")
      (newline)
      (display ";; Libraries:")
      (newline)
      (newline)))

(define gossip:get-libraries
  (lambda (components done)
    (if (not (null? components))
      (let ((lib (gnetlist:get-package-attribute (car components) "library")))
        (if (string=? "unknown" lib)
          (begin
            (message "Component ")
            (message (car components))
            (message " does not have a library attribute\n")))
        (if (contains? done lib)
          (gossip:get-libraries (cdr components) done)
          (begin
            (display "(use-library ")
            (display lib)
            (display " *)")
            (newline)
            (gossip:get-libraries (cdr components) (cons lib done))))))))

(define gossip:list-pins
   (lambda (allnets uref pin)
      (let ((pinname (gnetlist:get-attribute-by-pinnumber uref (number->string pin) "label")))
         (if (string=? "unknown" pinname)
            (display ")\n")
            (begin
               (display " :")
               (display pinname)
               (write-char #\space)
               (display (gossip:find-net uref pin allnets))
               (gossip:list-pins allnets uref (+ 1 pin)))))))

;(define gossip:reverse-netlist
;   (lambda (allnets)
;      (if (null? allnets)
;         '()
;         (let ((connections (gnetlist:get-all-connections (car allnets))))
;            (cons (gossip:connectionlist connections)
;                  (gossip:reverse-netlist (cdr allnets))))))

(define gossip:find-net
   (lambda (uref pin allnets)
      (cond
         ((null? allnets) "Not Connected" )
         ((gossip:finder uref pin (gnetlist:get-all-connections (car allnets)))(car allnets))
         (#t (gossip:find-net uref pin (cdr allnets))))))

(define gossip:finder
   (lambda (uref pin list)
      (cond
         ((null? list)#f)
         ((and (string=? uref (caar list)) (string=? (number->string pin) (cadar list))) #t)
         (#t (gossip:finder uref pin (cdr list))))))

(define gossip:display-connections
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
               (gossip:display-connections (cdr nets))))))

(define gossip:display-name-nets
   (lambda (nets)
      (begin
         (gossip:display-connections nets)
         (write-char #\space)
         (newline))))

(define gossip:blocks
   (lambda (ls allnets)
      (if (not (null? ls))
         (let ((package (car ls)))
            (display "   (")
            (display package)
            (gossip:list-pins allnets package 1)
            (gossip:blocks (cdr ls) allnets)))))

(define gossip:signals
   (lambda ()
      (display "(signals ")
      (display (gnetlist:get-all-unique-nets "dummy"))
      (display ")\n")))

(define gossip:write-block-header
   (lambda ()
      (let ((blockname (gnetlist:get-toplevel-attribute "blockname")))
         (display "(define-block (")
         (display blockname)
         (display " (")
         (newline))))

(define (gossip output-filename)
  (set-current-output-port (gnetlist:output-port output-filename))
  (begin
     (gossip:write-top-header)
     (gossip:get-libraries packages '())
     (gossip:write-block-header)
     (gossip:signals)
     (gossip:blocks packages (gnetlist:get-all-unique-nets "dummy")))
  (close-output-port (current-output-port)))
