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

  #:export (libgtk
            libglib
            libgobject
            liblepton
            %libleptonattrib
            %libleptongui))

(define (ldconfig-check-libname libname)
  (and (string= (major-version) "3")
       (not (string-null? libname))
       libname))

(define %liblepton
  (if %m4-use-cygwin
      (string-append "cyglepton-"
                     (number->string %m4-liblepton-major))
      "liblepton"))

(define %libleptonattrib
  (if %m4-use-cygwin
      (string-append "cygleptonattrib-"
                     (number->string %m4-libleptonattrib-major))
      "libleptonattrib"))

(define %libleptongui
  (if %m4-use-cygwin
      (string-append "cygleptongui-"
                     (number->string %m4-libleptongui-major))
      "libleptongui"))


(define %libglib
  (or (ldconfig-check-libname %ldconfig-libglib)
      (if %m4-use-cygwin "cygglib-2.0-0" "libglib-2.0")))

(define %libgobject
  (or (ldconfig-check-libname %ldconfig-libgobject)
      (if %m4-use-cygwin "cyggobject-2.0-0" "libgobject-2.0")))

(define %libgtk
  (if %m4-use-gtk3
      (or (ldconfig-check-libname %ldconfig-libgtk3)
          (if %m4-use-cygwin "cyggtk-3-0" "libgtk-3"))
      (or (ldconfig-check-libname %ldconfig-libgtk2)
          (if %m4-use-cygwin "cyggtk-x11-2.0-0" "libgtk-x11-2.0"))))

(define libglib (dynamic-link %libglib))

(define libgobject (dynamic-link %libgobject))

(define libgtk (dynamic-link %libgtk))

(define liblepton
  (dynamic-link (or (getenv "LIBLEPTON") %liblepton)))
