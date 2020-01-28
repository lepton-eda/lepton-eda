;;; Lepton EDA Symbol Checker
;;; Scheme API
;;; Copyright (C) 2017 Lepton EDA Contributors
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
;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA

(define-module (symcheck option)
  #:use-module (ice-9 getopt-long)
  #:use-module ((srfi srfi-1) #:select (filter-map))
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
  (filter-map
   (lambda (x) (and (eq? (cdr x) '()) (car x)))
   %default-symcheck-options))

;;; getopt-long compatible symcheck options.
(define %symcheck-options
  (getopt-long (program-arguments)
               ;; option spec
               '((quiet (single-char #\q))
                 (verbose (single-char #\v))
                 (help (single-char #\h))
                 (version (single-char #\V))
                 (interactive (single-char #\i)))))

;;; This function extends option-ref so that for keys which may
;;; repeat on command line, it returns their value as value lists
;;; (e.g. "cmd -x a -x b" produces '("a" "b") for the key 'x).
 (define (list-option-ref options key default)
  (or (filter-map
       (lambda (x) (and (eq? (car x) key) (cdr x)))
       options)
      default))

(define (symcheck-option-ref key)
  "Returns value of symcheck option KEY. Use '() to request schematics."
  (let ((default (assq-ref %default-symcheck-options key))
        (is-list-key? (memq key %list-keys)))
    ((if is-list-key? list-option-ref option-ref) %symcheck-options key default)))

(define (symcheck-option-ref-length key)
  (length (symcheck-option-ref key)))
