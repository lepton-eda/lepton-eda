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


;;; Syntax macros for defining lazy foreign functions.

(define-module (lepton ffi lff)
  #:use-module (system foreign)

  #:use-module (lepton ffi lib)

  #:export (define-lff-lib
            define-lfc-lib))


;;; Syntax to define a lazy foreign function from given library.
(define-syntax define-lff-lib
  (syntax-rules ()
    ((_ name type args lib)
     (define name
       (let ((proc (delay (pointer->procedure
                           type
                           (dynamic-func (symbol->string (quote name)) lib)
                           args))))
         (force proc))))))


;;; Brief syntax macro for defining lazy foreign callbacks.
;;; Unlike 'define-lff-lib' above, it returns a pointer to a C
;;; function by name, not a Scheme procedure wrapping it.
;;;
;;; By convention, the first character of a callback function name
;;; should be '*'.  The first character is dropped here before
;;; dlopening the function in any case, so be careful when
;;; composing callback names.
(define-syntax define-lfc-lib
  (syntax-rules ()
    ((_ name lib)
     (define name
       (let ((*callback
              (delay (dynamic-func (string-drop (symbol->string (quote name)) 1)
                                   lib))))
         (force *callback))))))
