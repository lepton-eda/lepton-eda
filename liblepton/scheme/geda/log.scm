;; gEDA - GPL Electronic Design Automation
;; libgeda - gEDA's library - Scheme API
;; Copyright (C) 2016 Peter Brett <peter@peter-b.co.uk>
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA
;;

(define-module (geda log)
  #:use-module (geda core log)
  #:use-module (geda core gettext)

  #:use-module (geda os)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26))

;; ================================================================
;; Logging messages
;; ================================================================

#|
Function::

  log! level message [format-args]

Log a new message with the specified log level and contents.

The LEVEL should describe the log level -- for example, one of the
symbols "message", "warning", "critical", "error", "info" or
"debug".  "error"-level messages are fatal.

A newline character is automatically appended to the message.
|#
(define-public (log! level message . format-args)
  (let ((formatted (apply format #f message format-args)))
    (%log! #f level (string-append formatted))))
