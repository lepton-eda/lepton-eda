;;; Lepton EDA netlister
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
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;;; MA 02111-1301 USA.

(define-module (gnetlist option)
  #:use-module (ice-9 getopt-long)
  #:use-module ((srfi srfi-1) #:select (filter-map))
  #:export (%default-gnetlist-options
            gnetlist-option-ref))

;;; Empty lists are default values for the keys which may repeat
;;; on command line.
(define %default-gnetlist-options
  '((quiet . #f)
    (verbose . #f)
    (load-path . ())
    (backend . #f)
    (backend-option . ())
    (list-backends . #f)
    (output . "output.net")
    (pre-load . ())
    (post-load . ())
    (eval-code . ())
    (interactive . #f)
    (help . #f)
    (version . #f)))

;;; This list contains key names which values must be lists.
(define %list-keys
  (filter-map
   (lambda (x) (and (eq? (cdr x) '()) (car x)))
   %default-gnetlist-options))

;;; getopt-long compatible gnetlist options.
(define %gnetlist-options
  (getopt-long (program-arguments)
               ;; option spec
               '((quiet (single-char #\q))
                 (verbose (single-char #\v))
                 (load-path (single-char #\L) (value #t))
                 (backend (single-char #\g) (value #t))
                 (backend-option (single-char #\O) (value #t))
                 (list-backends)
                 (output (single-char #\o) (value #t))
                 (pre-load (single-char #\l) (value #t))
                 (post-load (single-char #\m) (value #t))
                 (eval-code (single-char #\c) (value #t))
                 (interactive (single-char #\i))
                 (help (single-char #\h))
                 (version (single-char #\V)))))

;;; This function extends option-ref so that for keys which may
;;; repeat on command line, it returns their value as value lists
;;; (e.g. "cmd -x a -x b" produces '("a" "b") for the key 'x).
(define (list-option-ref options key default)
  (or (filter-map
       (lambda (x) (and (eq? (car x) key) (cdr x)))
       options)
      default))

(define (gnetlist-option-ref key)
  "Returns value of gnetlist option KEY. Use '() to request schematics."
  (let ((default (assq-ref %default-gnetlist-options key))
        (is-list-key? (memq key %list-keys)))
    ((if is-list-key? list-option-ref option-ref) %gnetlist-options key default)))
