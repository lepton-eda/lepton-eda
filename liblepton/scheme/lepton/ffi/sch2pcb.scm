;;; Lepton EDA library - Scheme API
;;; Copyright (C) 2022-2023 Lepton EDA Contributors
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

;;; This module exports foreign functions for lepton-sch2pcb.

(define-module (lepton ffi sch2pcb)
  #:use-module (system foreign)
  #:use-module (lepton ffi lib)
  #:use-module (lepton ffi lff)

  #:export (pcb_element_free
            pcb_element_get_changed_description
            pcb_element_set_changed_description
            pcb_element_get_changed_value
            pcb_element_set_changed_value
            pcb_element_get_description
            pcb_element_set_description
            pcb_element_get_flags
            pcb_element_get_hi_res_format
            pcb_element_set_hi_res_format
            pcb_element_get_omit_PKG
            pcb_element_get_pkg_name_fix
            pcb_element_get_quoted_flags
            pcb_element_get_refdes
            pcb_element_set_refdes
            pcb_element_get_res_char
            pcb_element_set_res_char
            pcb_element_get_still_exists
            pcb_element_set_still_exists
            pcb_element_get_tail
            pcb_element_get_value
            pcb_element_set_value
            pcb_element_get_x
            pcb_element_get_y
            pcb_element_line_parse
            pcb_element_pkg_to_element

            sch2pcb_buffer_to_file
            sch2pcb_get_empty_footprint_name
            sch2pcb_set_empty_footprint_name
            sch2pcb_find_element
            sch2pcb_increment_verbose_mode
            sch2pcb_insert_element
            sch2pcb_get_n_empty
            sch2pcb_get_n_none
            sch2pcb_get_n_unknown
            sch2pcb_parse_schematics
            sch2pcb_get_pcb_element_list
            sch2pcb_pcb_element_list_append
            sch2pcb_get_verbose_mode
            sch2pcb_open_file_to_read
            sch2pcb_open_file_to_write
            sch2pcb_close_file))

;;; Simplify definition of functions by omitting the library
;;; argument.
(define-syntax-rule (define-lff arg ...)
  (define-lff-lib arg ... liblepton))

(define-lff pcb_element_free void '(*))
(define-lff pcb_element_get_changed_description '* '(*))
(define-lff pcb_element_set_changed_description void '(* *))
(define-lff pcb_element_get_changed_value '* '(*))
(define-lff pcb_element_set_changed_value void '(* *))
(define-lff pcb_element_get_description '* '(*))
(define-lff pcb_element_set_description void '(* *))
(define-lff pcb_element_get_flags '* '(*))
(define-lff pcb_element_get_hi_res_format int '(*))
(define-lff pcb_element_set_hi_res_format void (list '* int))
(define-lff pcb_element_get_omit_PKG int '(*))
(define-lff pcb_element_get_pkg_name_fix '* '(*))
(define-lff pcb_element_get_quoted_flags int '(*))
(define-lff pcb_element_get_refdes '* '(*))
(define-lff pcb_element_set_refdes void '(* *))
(define-lff pcb_element_get_res_char int '(*))
(define-lff pcb_element_set_res_char void (list '* int))
(define-lff pcb_element_get_still_exists int '(*))
(define-lff pcb_element_set_still_exists void (list '* int))
(define-lff pcb_element_get_tail '* '(*))
(define-lff pcb_element_get_value '* '(*))
(define-lff pcb_element_set_value void '(* *))
(define-lff pcb_element_get_x '* '(*))
(define-lff pcb_element_get_y '* '(*))
(define-lff pcb_element_line_parse '* '(*))
(define-lff pcb_element_pkg_to_element '* '(*))

(define-lff sch2pcb_buffer_to_file void '(* *))
(define-lff sch2pcb_get_empty_footprint_name '* '())
(define-lff sch2pcb_set_empty_footprint_name void '(*))
(define-lff sch2pcb_find_element '* '(* *))
(define-lff sch2pcb_increment_verbose_mode void '())
(define-lff sch2pcb_insert_element int '(* * * * *))
(define-lff sch2pcb_get_n_empty int '())
(define-lff sch2pcb_get_n_none int '())
(define-lff sch2pcb_get_n_unknown int '())
(define-lff sch2pcb_parse_schematics '* '(*))
(define-lff sch2pcb_get_pcb_element_list '* '())
(define-lff sch2pcb_pcb_element_list_append void '(*))
(define-lff sch2pcb_get_verbose_mode int '())
(define-lff sch2pcb_open_file_to_read '* '(*))
(define-lff sch2pcb_open_file_to_write '* '(*))
(define-lff sch2pcb_close_file void '(*))
