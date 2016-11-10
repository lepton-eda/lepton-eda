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

(use-modules (gnetlist attrib compare))
;;
;; Top level component writing
;;
(define (bae:components ls)
  (define (output-package package)
    (format #t "    ~A : ~A;\n"
            package
            (gnetlist:get-package-attribute package "footprint")))
  (for-each output-package ls))

;;
;; Display the individual net connections
;;
(define (bae:format-connections nets)
  (string-join
   (map
    (lambda (net) (let ((package (car net))
                   (connection (cadr net)))
               (format #f "~A.~A" package connection)))
    nets)
   "="))

;;
;; Write netname : uref pin, uref pin, ...
;;
(define (bae:write-net netname)
  (format #t "    /'~A'/ ~A;\n" netname
          (bae:format-connections (gnetlist:get-all-connections netname))))

;;
;; Write the net part of the gEDA format
;;
(define (bae:nets)
  (for-each bae:write-net (gnetlist:get-all-unique-nets "dummy")))

;;; Highest level function
;;; Write my special testing netlist format
;;;
(define (bae output-filename)
  (with-output-to-port
      (gnetlist:output-port output-filename)
    (lambda ()
      (display "LAYOUT board;\n")
      (display "PARTS\n")
      (bae:components (sort packages refdes<?))
      (display "CONNECT\n")
      (bae:nets)
      (display "END.\n"))))
