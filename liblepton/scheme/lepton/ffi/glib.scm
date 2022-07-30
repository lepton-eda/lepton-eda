;;; Lepton EDA library - Scheme API
;;; Copyright (C) 2020-2022 Lepton EDA Contributors
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

(define-module (lepton ffi glib)
  #:use-module (ice-9 match)
  #:use-module (system foreign)

  #:use-module (lepton ffi lib)
  #:use-module (lepton ffi lff)

  #:export (g_clear_error
            g_free
            g_list_append
            g_list_free
            g_list_remove
            g_list_remove_all
            g_log

            ;; Mock glib functions.
            glist-data
            glist-next
            glist-prev
            glist->list))

;;; Simplify definition of functions by omitting the library
;;; argument.
(define-syntax-rule (define-lff arg ...)
  (define-lff-lib arg ... libglib))


(define-lff g_clear_error void '(*))

(define-lff g_free void '(*))

(define-lff g_list_append '* '(* *))
(define-lff g_list_free void '(*))
(define-lff g_list_remove '* '(* *))
(define-lff g_list_remove_all '* '(* *))

(define-lff g_log void (list '* int '* '*))


;;; Glist struct is {data*, next*, prev*}.  We could use
;;; functions to get data, but it's easier to parse the struct
;;; directly.
(define (parse-glist gls)
  (parse-c-struct gls '(* * *)))

(define (glist-next gls)
  (let ((pointer-ls (parse-glist gls)))
    (match pointer-ls
      ((data next prev) next)
      (_ (error "Wrong Glist in glist-next()")))))

(define (glist-prev gls)
  (let ((pointer-ls (parse-glist gls)))
    (match pointer-ls
      ((data next prev) prev)
      (_ (error "Wrong Glist in glist-prev()")))))

(define (glist-data gls)
  (let ((pointer-ls (parse-glist gls)))
    (match pointer-ls
      ((data next prev) data)
      (_ (error "Wrong Glist in glist-data()")))))

(define (glist->list gls convert-func)
  "Convert C GList GLS into Scheme list of objects using the
function CONVERT-FUNC to transform foreign pointers to Scheme
objects."
  (let loop ((gls gls)
             (ls '()))
    (if (null-pointer? gls)
        (reverse ls)
        (loop (glist-next gls)
              (cons (convert-func (glist-data gls)) ls)))))
