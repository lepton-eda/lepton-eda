;;; Lepton EDA Schematic Capture
;;; Scheme API
;;; Copyright (C) 2021 Lepton EDA Contributors
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

(define-module (lepton ffi lib)
  #:use-module (system foreign)
  #:use-module (lepton m4)

  #:export (libglib
            libgobject
            liblepton
            libleptonattrib))

(define LIBLEPTON
  (if CYGWIN
      (string-append "cyglepton-" (number->string LIBLEPTON_MAJOR))
      "liblepton"))

(define LIBLEPTONATTRIB
  (if CYGWIN
      (string-append "cygleptonattrib-"
                     (number->string LIBLEPTONATTRIB_MAJOR))
      "libleptonattrib"))

(define LIBGLIB
  (if CYGWIN "cygglib-2.0-0" "libglib-2.0"))

(define LIBGOBJECT
  (if CYGWIN "cyggobject-2.0-0" "libgobject-2.0"))

(define libglib (dynamic-link LIBGLIB))

(define libgobject (dynamic-link LIBGOBJECT))

(define liblepton
  (dynamic-link (or (getenv "LIBLEPTON") LIBLEPTON)))

(define libleptonattrib (dynamic-link LIBLEPTONATTRIB))
