;;; Lepton EDA library - Scheme API
;;; Copyright (C) 2022 Lepton EDA Contributors
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

  #:export (sch2pcb_add_default_m4_files
            sch2pcb_add_elements
            sch2pcb_get_args
            sch2pcb_element_directory_list_append
            sch2pcb_set_default_m4_pcbdir
            sch2pcb_set_m4_pcbdir
            sch2pcb_load_extra_project_files
            sch2pcb_get_pcb_element_list
            sch2pcb_get_sch_basename
            sch2pcb_get_schematics
            sch2pcb_main
            sch2pcb_run_netlister
            sch2pcb_usage
            sch2pcb_get_verbose_mode))

;;; Simplify definition of functions by omitting the library
;;; argument.
(define-syntax-rule (define-lff arg ...)
  (define-lff-lib arg ... liblepton))

(define-lff sch2pcb_add_default_m4_files void '())
(define-lff sch2pcb_add_elements int '(*))
(define-lff sch2pcb_get_args void (list int '*))
(define-lff sch2pcb_element_directory_list_append void '(*))
(define-lff sch2pcb_set_default_m4_pcbdir void '(*))
(define-lff sch2pcb_set_m4_pcbdir void '(*))
(define-lff sch2pcb_load_extra_project_files void '())
(define-lff sch2pcb_get_pcb_element_list void '(*))
(define-lff sch2pcb_get_sch_basename '* '())
(define-lff sch2pcb_get_schematics '* '())
(define-lff sch2pcb_main int (list '* '* '* '* '* int))
(define-lff sch2pcb_run_netlister int '(* * * * *))
(define-lff sch2pcb_usage int '())
(define-lff sch2pcb_get_verbose_mode int '())
