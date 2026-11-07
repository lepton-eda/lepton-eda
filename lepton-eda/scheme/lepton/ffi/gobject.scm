;;; Lepton EDA library - Scheme API
;;; Copyright (C) 2020-2026 Lepton EDA Contributors
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

(define-module (lepton ffi gobject)
  #:use-module (system foreign)

  #:use-module (lepton ffi lib)
  #:use-module (lepton ffi lff)

  #:export (g_object_get_data
            g_object_set_data
            g_object_set_data_full
            g_object_ref
            g_object_unref
            *g_object_unref
            g_signal_connect))

;;; Defined in glib/gtypes.h
(define gulong unsigned-long)

;;; Simplify definition of functions by omitting the library
;;; argument.
(define-syntax-rule (define-lff arg ...)
  (define-lff-lib arg ... libgobject))

(define-syntax-rule (define-lfc arg ...)
  (define-lfc-lib arg ... libgobject))

(define-lff g_object_get_data '* '(* *))
(define-lff g_object_set_data void '(* * *))
(define-lff g_object_set_data_full void '(* * * *))
(define-lff g_object_ref '* '(*))
(define-lff g_object_unref void '(*))
(define-lfc *g_object_unref)

;;; Defined in gobject/gsignal.h
(define-lff g_signal_connect_data gulong (list '* '* '* '* '* int))
(define-syntax-rule (g_signal_connect *instance
                                      *detailed-signal
                                      *c-handler
                                      *data)
  (g_signal_connect_data *instance
                         *detailed-signal
                         *c-handler
                         *data
                         %null-pointer  ; *destroy-data
                         0              ; connect-flags
                         ))
