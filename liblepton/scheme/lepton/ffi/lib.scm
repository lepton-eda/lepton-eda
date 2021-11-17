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
            libleptonattrib
            libleptongui))

(define LIBLEPTON
  (if %m4-use-cygwin
      (string-append "cyglepton-"
                     (number->string %m4-liblepton-major))
      "liblepton"))

(define LIBLEPTONATTRIB
  (if %m4-use-cygwin
      (string-append "cygleptonattrib-"
                     (number->string %m4-libleptonattrib-major))
      "libleptonattrib"))

(define LIBLEPTONGUI
  (if %m4-use-cygwin
      (string-append "cygleptongui-"
                     (number->string %m4-libleptongui-major))
      "libleptongui"))


(define LIBGLIB
  (if (string-null? %ldconfig-libglib)
      (if %m4-use-cygwin "cygglib-2.0-0" "libglib-2.0")
      %ldconfig-libglib))

(define LIBGOBJECT
  (if (string-null? %ldconfig-libgobject)
      (if %m4-use-cygwin "cyggobject-2.0-0" "libgobject-2.0")
      %ldconfig-libgobject))

(define LIBGTK
  (if %m4-use-gtk3
      (if (string-null? %ldconfig-libgtk3)
          (if %m4-use-cygwin "cyggtk-3-0" "libgtk-3")
          %ldconfig-libgtk3)
      (if (string-null? %ldconfig-libgtk2)
          (if %m4-use-cygwin "cyggtk-x11-2.0-0" "libgtk-x11-2.0")
          %ldconfig-libgtk2)))

(define libglib (dynamic-link LIBGLIB))

(define libgobject (dynamic-link LIBGOBJECT))

(define liblepton
  (dynamic-link (or (getenv "LIBLEPTON") LIBLEPTON)))

(define libleptonattrib (dynamic-link LIBLEPTONATTRIB))

(define libgtk (dynamic-link LIBGTK))

(define libleptongui
  (dynamic-link (or (getenv "LIBLEPTONGUI") LIBLEPTONGUI)))
