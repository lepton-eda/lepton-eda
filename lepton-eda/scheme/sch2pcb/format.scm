;;; Lepton EDA Schematic to PCB conversion
;;; Scheme API
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

(define-module (sch2pcb format)
  #:use-module (lepton ffi sch2pcb)
  #:use-module (lepton gettext)

  #:export (format-error
            format-message
            format-warning
            verbose-format
            extra-verbose-format))


;;; Save current ports to put messages, warnings, and errors there
;;; afterwards.
(define %message-port (current-output-port))
(define %warning-port (current-error-port))
(define %error-port (current-error-port))

(define-syntax-rule (format-message msg arg ...)
  (format %message-port (G_ msg) arg ...))

(define-syntax-rule (format-warning msg arg ...)
  (format %warning-port (G_ msg) arg ...))

(define-syntax-rule (format-error arg ...)
  (format %error-port (G_ "ERROR: ~?.\n") arg ...))

(define-syntax-rule (verbose-format msg arg ...)
  (when (> (sch2pcb_get_verbose_mode) 0)
    (format %message-port (G_ msg) arg ...)))

(define-syntax-rule (extra-verbose-format msg arg ...)
  (when (> (sch2pcb_get_verbose_mode) 1)
    (format %message-port (G_ msg) arg ...)))
