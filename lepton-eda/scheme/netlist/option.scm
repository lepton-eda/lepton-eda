;;; Lepton EDA netlister
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

(define-module (netlist option)
  #:use-module (srfi srfi-1)
  #:use-module ((ice-9 getopt-long) #:select (option-ref))
  #:use-module (lepton option)

  #:export (%default-netlist-options
            %netlist-options
            init-netlist-options!
            netlist-option-ref
            netlist-option-ref/toplevel
            set-netlist-option!))

;;; Empty lists are default values for the keys which may repeat
;;; on command line.
(define %default-netlist-options
  '((quiet . #f)
    (verbose . #f)
    (load-path . ())
    (backend . #f)
    (file-backend . #f)
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
  (option-ref-get-list-keys %default-netlist-options))

;;; Checks if KEY must return a list.
(define (is-list-key? key)
  (memq key %list-keys))

;;; Custom function to account for list keys.
(define (netlist-option-ref/toplevel getopt-long-options key default)
  "Searches for KEY in GETOPT-LONG-OPTIONS and returns its value
if found, otherwise returns DEFAULT. For option keys that may
repeat on command line (which is specified in
%default-netlist-options alist by value '()), returns the value as
a list containing option arguments for each instance. Thus, the
procedure extends getopt-long's option-ref procedure in that it
can return list values. WARNING: the procedure is intended for
using ONLY in toplevel code."
  (define ref-func
    (if (is-list-key? key) list-option-ref option-ref))
  (ref-func getopt-long-options key default))

;;; Current netlister options.
;;; First, define them as a mutable copy of default options.
(define %netlist-options
  ;; Prepend empty list to get file names, the same way as with
  ;; option-ref.
  (cons (list '())
        (map (lambda (p) (cons (car p) (cdr p)))
             %default-netlist-options)))

(define (netlist-option-ref key)
  "Returns value of netlister option KEY. Use '() to request schematics."
  (assq-ref %netlist-options key))

(define (set-netlist-option! key value)
  "Sets lepton-netlist option KEY to VALUE. Returns the new value
if the key is available, otherwise returns #f."
  (and (assq key %netlist-options)
       (begin
         (set! %netlist-options
               (assq-set! %netlist-options key value))
         value)))

(define (init-netlist-options! getopt-long-options)
  "Initializes lepton-netlist options using GETOPT-LONG-OPTIONS,
so they can be referenced by the netlist-option-ref
procedure. Returns the resulting netlister option list stored in
%netlist-options. WARNING: the procedure is intended for using
ONLY in toplevel code."
  (define (init-option! key-default-pair)
    (let ((key (car key-default-pair))
          (default (cdr key-default-pair)))
      (set-netlist-option! key
                           (netlist-option-ref/toplevel getopt-long-options
                                                        key
                                                        default))))

  (for-each init-option! %netlist-options)
  %netlist-options)
