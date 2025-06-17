;;; Lepton EDA library
;;; Scheme API
;;; Copyright (C) 2017-2020 Lepton EDA Contributors
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

;;; Common procedures for advanced option processing.

(define-module (lepton option)
  #:use-module ((srfi srfi-1) #:select (filter-map))

  #:export (option-ref-get-list-keys
            list-option-ref))

(define (option-ref-get-list-keys default-options)
  "Having an alist of DEFAULT-OPTIONS, returns those keys whose
values can appear more than once in command line and thus should
form a list of values for subsequent processing.  The sign that an
option should be processed in such a way is its value of '().
This is useful in cases when an option may appear several times in
the command line.  For example, to load several Guile libraries,
you can specify the option '-L' several times."
  (filter-map
   (lambda (x) (and (eq? (cdr x) '()) (car x)))
   default-options))


;;; This function extends option-ref so that for keys which may
;;; repeat on command line, it returns their value as value lists
;;; (e.g. "cmd -x a -x b" produces '("a" "b") for the key 'x).
(define (list-option-ref options key default)
  "The function extends option-ref in that for keys which may
repeat on command line, it returns their value as value lists (for
example, when using the command \"cmd -x a -x b\" it returns the
value '(\"a\" \"b\") for the key 'x)."
  (or (reverse (filter-map
                (lambda (x) (and (eq? (car x) key) (cdr x)))
                options))
      default))
