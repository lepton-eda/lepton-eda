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

            attrib_set_sheet_data
            attrib_set_toplevel

            set_verbose_mode

            x_fileselect_open

            s_sheet_data_new
            s_sheet_data_add_master_comp_list_items
            s_sheet_data_add_master_comp_attrib_list_items
            s_sheet_data_add_master_net_list_items
            s_sheet_data_add_master_net_attrib_list_items
            s_sheet_data_add_master_pin_list_items
            s_sheet_data_add_master_pin_attrib_list_items

            attrib_activate
            attrib_run
            attrib_window_new
            attrib_window_set_window_widget
            x_window_init
            ))

;;; Simplify definition of functions by omitting the library
;;; argument.
(define libleptonattrib
  (dynamic-link (or (getenv "LIBLEPTONATTRIB") %libleptonattrib)))

(define-syntax-rule (define-lff arg ...)
  (define-lff-lib arg ... libleptonattrib))

(define-lff-lib gtk_init void '(* *) libgtk)

;;; attrib.c
(define-lff attrib_set_sheet_data void '(*))
(define-lff attrib_set_toplevel void '(*))

;;; s_misc.c
(define-lff set_verbose_mode void '())

;;; x_fileselect.c
(define-lff x_fileselect_open '* '())

;;; s_sheet_data.c
(define-lff s_sheet_data_new '* '())
(define-lff s_sheet_data_add_master_comp_list_items void '(*))
(define-lff s_sheet_data_add_master_comp_attrib_list_items void '(*))
(define-lff s_sheet_data_add_master_net_list_items void '(*))
(define-lff s_sheet_data_add_master_net_attrib_list_items void '(*))
(define-lff s_sheet_data_add_master_pin_list_items void '(*))
(define-lff s_sheet_data_add_master_pin_attrib_list_items void '(*))

;;; x_window.c
(define-lff attrib_activate int '(* *))
(define-lff attrib_run int '(* *))
(define-lff attrib_window_new '* '(*))
(define-lff attrib_window_set_window_widget void '(*))
(define-lff x_window_init void '())
