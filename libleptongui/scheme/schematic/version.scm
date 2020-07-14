;;; Lepton EDA attribute editor
;;; Copyright (C) 2020 Lepton EDA Contributors
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
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA


(define-module (schematic version)
  #:use-module (ice-9 match)

  #:use-module (lepton log)
  #:use-module (lepton version)
  #:use-module (schematic core gettext)

  #:export (print-lepton-schematic-version))


(define* (print-lepton-schematic-version #:optional stdout)
  "Print lepton-schematic version and copyright/warranty notices.
If STDOUT is true, output the info to standard output and exit
with exit status 0.  Otherwise, just print the message to log."

  (define (version-msg . args)
    (apply format #f "Lepton EDA/lepton-schematic ~A~A.~A (git: ~A)\n" args))

  (match (lepton-version)
    ((prepend dotted date commit bugs url copyright)
     (let ((version-message (version-msg prepend dotted date (string-take commit 7))))
       (if stdout
           (begin
             (display version-message)
             (display copyright)
             (primitive-exit 0))
           (log! 'message version-message))))))
