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

;; RACAL-REDAC / Cadstar netlist format by Wojciech Kazubski 2003

;;
;; Display the individual net connections
;;
(define redac:display-connections
   (lambda (nets k)
      (if (not (null? nets))
         (let ((item (string-append (caar nets) " " (cadar nets))))
            (display item)
            (if (not (null? (cdr nets)))
               (begin
               (if (> k 0)
                  (begin
                    (display " ")
                    (redac:display-connections (cdr nets) (- k 1)))
                  (begin
                    (display (string-append "\r\n"  item " "))
                    (redac:display-connections (cdr nets) (+ k 6))))))))))


(define redac:write-net
   (lambda (netnames)
      (if (not (null? netnames))
         (let ((netname (car netnames)))
            (display ".REM ")
            (display netname)
            (display "\r\n")
            (redac:display-connections
                       (gnetlist:get-all-connections netname) 7)
            (display "\r\n")
            (redac:write-net (cdr netnames))
            ))))

(define (redac output-filename)
  (with-output-to-port (gnetlist:output-port output-filename)
    (lambda ()
      (display ".PCB\r\n")
      (display ".REM CREATED BY gEDA GNETLIST\r\n")
      (display ".CON\r\n")
      (display ".COD 2\r\n\r\n")
      (redac:write-net (gnetlist:get-all-unique-nets "dummy"))
      (display ".EOD\r\n"))))
