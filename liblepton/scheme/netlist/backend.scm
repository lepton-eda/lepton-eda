;;; Lepton EDA netlister
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


(define-module (netlist backend)
  #:use-module (ice-9 regex)

  #:export (backend-filename->proc-name
            run-backend))


(define (backend-filename->proc-name filename)
  ( let*
    (
    ( bname (basename filename) )
    ( re    (string-match "^gnet-(.*).scm$" bname) )
    )

    ;; Return the function name if the file name matched the above
    ;; regexp, otherwise return #f.
    (and re
         (match:substring re 1))))


(define (run-backend backend output-filename)
  (let ((backend-proc (primitive-eval (string->symbol backend))))
    (if output-filename
        ;; output-filename is defined, output to it.
        (with-output-to-file output-filename
          (lambda () (backend-proc output-filename)))
        ;; output-filename is #f, output to stdout.
        (backend-proc output-filename))))
