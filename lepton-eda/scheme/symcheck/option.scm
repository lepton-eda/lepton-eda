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

(define-module (symcheck option)
  #:use-module (ice-9 getopt-long)
  #:use-module (lepton option)

  #:export (%default-symcheck-options
            symcheck-option-ref
            symcheck-option-ref-length))

;;; Empty lists are default values for the keys which may repeat
;;; on command line.
(define %default-symcheck-options
  '((quiet . #f)
    (verbose . ())
    (help . #f)
    (version . #f)
    (interactive . #f)))

;;; This list contains key names which values must be lists.
(define %list-keys
  (option-ref-get-list-keys %default-symcheck-options))

;;; getopt-long compatible symcheck options.
(define (%symcheck-options)
  (getopt-long (program-arguments)
               ;; option spec
               '((quiet (single-char #\q))
                 (verbose (single-char #\v))
                 (help (single-char #\h))
                 (version (single-char #\V))
                 (interactive (single-char #\i)))))

(define (symcheck-option-ref key)
  "Returns value of symcheck option KEY. Use '() to request schematics."
  (let ((default (assq-ref %default-symcheck-options key))
        (is-list-key? (memq key %list-keys)))
    ((if is-list-key? list-option-ref option-ref) (%symcheck-options) key default)))

(define (symcheck-option-ref-length key)
  (length (symcheck-option-ref key)))
