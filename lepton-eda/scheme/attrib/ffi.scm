;;; Lepton EDA Schematic Capture
;;; Scheme API
;;; Copyright (C) 2026 Lepton EDA Contributors
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

(define-module (attrib ffi)
  #:use-module (system foreign)

  #:use-module (lepton ffi lff)
  #:use-module (lepton ffi lib)

  #:export (gtk_init

            set_verbose_mode

            x_fileselect_open

            attrib_activate
            attrib_run
            attrib_window_new))

;;; Simplify definition of functions by omitting the library
;;; argument.
(define libleptonattrib
  (dynamic-link (or (getenv "LIBLEPTONATTRIB") %libleptonattrib)))

(define-syntax-rule (define-lff arg ...)
  (define-lff-lib arg ... libleptonattrib))

(define-lff-lib gtk_init void '(* *) libgtk)

;;; s_misc.c
(define-lff set_verbose_mode void '())

;;; x_fileselect.c
(define-lff x_fileselect_open '* '())

;;; x_window.c
(define-lff attrib_activate int '(* *))
(define-lff attrib_run int '(* *))
(define-lff attrib_window_new '* '(*))
