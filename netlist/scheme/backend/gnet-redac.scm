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

;; RACAL-REDAC / Cadstar netlist format by Wojciech Kazubski 2003

(use-modules (srfi srfi-1)
             (ice-9 receive)
             (netlist schematic)
             (netlist schematic toplevel))

;;; Transforms LS into list of groups where each group is a list
;;; containig NUM elements.
(define (group-elements ls num)
  (receive
      (head rest)
      (if (> (length ls) num)
          (split-at ls num)
          (values ls '()))
    (if (null? rest)
        (list head)
        (cons head (group-elements rest num)))))

;;
;; Display the individual net connections
;;
(define (redac:display-connections groups)
  (define package car)
  (define pinnumber cdr)
  (define (connection->string connection)
    (format #f "~A ~A" (package connection) (pinnumber connection)))
  (define (group->string group)
    (string-join (map connection->string group) " "))
  (string-join (map group->string groups) "\r\n"))


(define (redac:write-net netnames)
  (define (write-net-info netname)
    (format #t
            ".REM ~A\r\n~A\r\n"
            netname
            (redac:display-connections
             (group-elements (get-all-connections netname) 8))))
  (for-each write-net-info netnames))


(define (redac output-filename)
  (display ".PCB\r\n")
  (display ".REM CREATED BY Lepton EDA netlister\r\n")
  (display ".CON\r\n")
  (display ".COD 2\r\n\r\n")
  (redac:write-net (schematic-nets (toplevel-schematic)))
  (display ".EOD\r\n"))
