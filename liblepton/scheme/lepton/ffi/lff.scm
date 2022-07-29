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


;;; Syntax macros for defining lazy foreign functions.

(define-module (lepton ffi lff)
  #:use-module (system foreign)

  #:use-module (lepton ffi lib)

  #:export (define-lff))


(define-syntax define-lff
  (syntax-rules ()
    ((_ name type args)
     (define name
       (let ((proc (delay (pointer->procedure
                           type
                           (dynamic-func (symbol->string (quote name)) liblepton)
                           args))))
         (force proc))))
    ((_ name type args lib)
     (define name
       (let ((proc (delay (pointer->procedure
                           type
                           (dynamic-func (symbol->string (quote name)) lib)
                           args))))
         (force proc))))))
