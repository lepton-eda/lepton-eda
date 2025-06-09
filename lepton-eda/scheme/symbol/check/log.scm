;;; Lepton EDA Symbol Checker
;;; Scheme API
;;; Copyright (C) 2017-2022 Lepton EDA Contributors
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

(define-module (symbol check log)
  #:use-module (lepton gettext)
  #:use-module (lepton log)

  #:export (set-check-log-destination!
            current-check-log-destination
            check-log!))

(define %check-log-destination 'log)

(define (set-check-log-destination! value)
  "Sets check-log! destination to VALUE which can be 'log or
'stdout. Returns the new destination. Signals an error is wrong
destination is specified."
  (if (or (eq? value 'log) (eq? value 'stdout))
      (begin (set! %check-log-destination value) value)
      (error (G_ "Wrong check log destination!"))))

(define (current-check-log-destination)
  "Returns current destination for the check-log! procedure."
  %check-log-destination)

(define (check-log! level message . format-args)
  "Logs MESSAGE formatted using FORMAT-ARGS with severity
LEVEL. The output goes to the destination defined using
set-check-log-destination!."
  (define (stdout-log! level message . format-args)
    (apply format #t message format-args)
    (newline))

  (let ((log-func (case %check-log-destination
                    ((stdout) stdout-log!)
                    ((log) log!)
                    (else stdout-log!))))
    (apply log-func level message format-args)))
