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
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 i18n)
  #:use-module (srfi srfi-1)

  #:use-module (lepton gettext)
  #:use-module (lepton log)

  #:export (backend-filename->proc-name
            lookup-backends
            run-backend))

(define %backend-prefix "gnet-")
(define %backend-suffix ".scm")
(define %backend-prefix-length (string-length %backend-prefix))
(define %backend-suffix-length (string-length %backend-suffix))


(define (backend-filename? filename)
  "Return #t if FILENAME is a backend filename, otherwise return
#f."
  (and (string-prefix? %backend-prefix filename)
       (string-suffix? %backend-suffix filename)))


(define (backend-filename->proc-name filename)
  "Transforms FILENAME to a backend name which is also the name of
the procedure the backend runs.  For legacy backends, the name is
formed by dropping the prefix \"gnet-\" and the extenstion
\".scm\".  Returns the resulting string or #f if FILENAME does not
meet the specified requirements."
  (let ((base (basename filename)))
    (and (backend-filename? base)
         (string-drop-right (string-drop base %backend-prefix-length)
                            %backend-suffix-length))))


(define (run-backend backend output-filename)
  (let ((backend-proc (primitive-eval (string->symbol backend))))
    (if output-filename
        ;; output-filename is defined, output to it.
        (with-output-to-file output-filename
          (lambda () (backend-proc output-filename)))
        ;; output-filename is #f, output to stdout.
        (backend-proc output-filename))))


(define (lookup-backends)
  "Searches %load-path for available lepton-netlist backends and
prints the resulting list.  A file is considered to be a backend
if its basename begins with \"gnet-\" and ends with \".scm\"."
  (define (path-backends path)
    (or (scandir path backend-filename?)
        (begin
          (log! 'warning (G_ "Can't open directory ~S.\n") path)
          '())))

  (define backend-files
    (append-map path-backends (delete-duplicates %load-path)))

  (define backend-names
    (map backend-filename->proc-name backend-files))

  (display (string-join
            (sort! backend-names string-locale<?)
            "\n"
            'suffix)))