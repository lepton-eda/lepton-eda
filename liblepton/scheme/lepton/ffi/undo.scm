;;; Lepton EDA library - Scheme API
;;; Copyright (C) 2023 Lepton EDA Contributors
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

;;; This module provides FFI for Lepton undo system.

(define-module (lepton ffi undo)
  #:use-module (system foreign)
  #:use-module (lepton ffi lib)
  #:use-module (lepton ffi lff)

  #:export (lepton_undo_set_filename
            lepton_undo_set_object_list
            lepton_undo_get_next
            lepton_undo_get_prev
            lepton_undo_get_type))

;;; Simplify definition of functions by omitting the library
;;; argument.
(define-syntax-rule (define-lff arg ...)
  (define-lff-lib arg ... liblepton))

(define-lff lepton_undo_set_filename void '(* *))
(define-lff lepton_undo_set_object_list void '(* *))
(define-lff lepton_undo_get_next '* '(*))
(define-lff lepton_undo_get_prev '* '(*))
(define-lff lepton_undo_get_type int '(*))
