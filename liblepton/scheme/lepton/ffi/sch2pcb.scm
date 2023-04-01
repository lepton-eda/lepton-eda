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

  #:export (pcb_element_exists
            pcb_element_free
            pcb_element_get_omit_PKG
            pcb_element_line_parse
            pcb_element_pkg_to_element
            sch2pcb_buffer_to_file
            sch2pcb_element_directory_list_append
            sch2pcb_element_directory_list_prepend
            sch2pcb_get_empty_footprint_name
            sch2pcb_set_empty_footprint_name
            sch2pcb_get_fix_elements
            sch2pcb_set_fix_elements
            sch2pcb_set_force_element_files
            sch2pcb_increment_verbose_mode
            sch2pcb_get_n_PKG_removed_new
            sch2pcb_get_n_PKG_removed_old
            sch2pcb_get_n_added_ef
            sch2pcb_get_n_added_m4
            sch2pcb_get_n_changed_value
            sch2pcb_get_n_deleted
            sch2pcb_get_n_empty
            sch2pcb_get_n_fixed
            sch2pcb_get_n_none
            sch2pcb_get_n_not_found
            sch2pcb_get_n_preserved
            sch2pcb_get_n_unknown
            sch2pcb_get_need_PKG_purge
            sch2pcb_set_need_PKG_purge
            sch2pcb_parse_next_line
            sch2pcb_parse_schematics
            sch2pcb_get_pcb_element_list
            sch2pcb_pcb_element_list_append
            sch2pcb_set_preserve
            sch2pcb_prune_elements
            sch2pcb_set_remove_unfound_elements
            sch2pcb_update_element_descriptions
            sch2pcb_verbose_print_separator
            sch2pcb_get_verbose_mode
            sch2pcb_open_file_to_write
            sch2pcb_close_file))

;;; Simplify definition of functions by omitting the library
;;; argument.
(define-syntax-rule (define-lff arg ...)
  (define-lff-lib arg ... liblepton))

(define-lff pcb_element_exists '* (list '* int))
(define-lff pcb_element_free void '(*))
(define-lff pcb_element_get_omit_PKG int '(*))
(define-lff pcb_element_line_parse '* '(*))
(define-lff pcb_element_pkg_to_element '* '(*))

(define-lff sch2pcb_buffer_to_file void '(* *))
(define-lff sch2pcb_element_directory_list_append void '(*))
(define-lff sch2pcb_element_directory_list_prepend void '(*))
(define-lff sch2pcb_get_empty_footprint_name '* '())
(define-lff sch2pcb_set_empty_footprint_name void '(*))
(define-lff sch2pcb_get_fix_elements int '())
(define-lff sch2pcb_set_fix_elements void (list int))
(define-lff sch2pcb_set_force_element_files void (list int))
(define-lff sch2pcb_increment_verbose_mode void '())
(define-lff sch2pcb_get_n_PKG_removed_new int '())
(define-lff sch2pcb_get_n_PKG_removed_old int '())
(define-lff sch2pcb_get_n_added_ef int '())
(define-lff sch2pcb_get_n_added_m4 int '())
(define-lff sch2pcb_get_n_changed_value int '())
(define-lff sch2pcb_get_n_deleted int '())
(define-lff sch2pcb_get_n_empty int '())
(define-lff sch2pcb_get_n_fixed int '())
(define-lff sch2pcb_get_n_none int '())
(define-lff sch2pcb_get_n_not_found int '())
(define-lff sch2pcb_get_n_preserved int '())
(define-lff sch2pcb_get_n_unknown int '())
(define-lff sch2pcb_get_need_PKG_purge int '())
(define-lff sch2pcb_set_need_PKG_purge void (list int))
(define-lff sch2pcb_parse_next_line int (list '* '* '* int int))
(define-lff sch2pcb_parse_schematics '* '(*))
(define-lff sch2pcb_get_pcb_element_list '* '())
(define-lff sch2pcb_pcb_element_list_append void '(*))
(define-lff sch2pcb_set_preserve void (list int))
(define-lff sch2pcb_prune_elements void '(* *))
(define-lff sch2pcb_set_remove_unfound_elements void (list int))
(define-lff sch2pcb_update_element_descriptions void '(* *))
(define-lff sch2pcb_verbose_print_separator void '())
(define-lff sch2pcb_get_verbose_mode int '())
(define-lff sch2pcb_open_file_to_write '* '(*))
(define-lff sch2pcb_close_file void '(*))
