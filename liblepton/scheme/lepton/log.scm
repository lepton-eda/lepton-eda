;;; Lepton EDA library - Scheme API
;;; Copyright (C) 2016 Peter Brett <peter@peter-b.co.uk>
;;; Copyright (C) 2017-2020 Lepton EDA Contributors
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

(define-module (lepton log)
  #:use-module (ice-9 format)
  #:use-module (system foreign)

  #:export (init-log
            log!))

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
(define (log! level message . format-args)
  (let ((formatted (apply format #f message format-args)))
    (%log! #f level (string-append formatted))))

(define (init-log domain)
  "Init log file using given DOMAIN prefix."
  (let ((s_log_init (delay (pointer->procedure
                            void
                            (dynamic-func "s_log_init" liblepton)
                            (list '*)))))
    ((force s_log_init) (string->pointer domain))))
