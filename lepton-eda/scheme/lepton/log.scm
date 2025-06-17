;;; Lepton EDA library - Scheme API
;;; Copyright (C) 2016 Peter Brett <peter@peter-b.co.uk>
;;; Copyright (C) 2017-2022 Lepton EDA Contributors
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

;;; Scheme API logging support.

(define-module (lepton log)
  #:use-module (ice-9 format)
  #:use-module (system foreign)

  #:use-module (lepton ffi glib)
  #:use-module (lepton ffi)

  #:export (init-log
            log!))

;; ================================================================
;; Logging messages
;; ================================================================

(define-syntax-rule (define-getter <name>)
  (define <name>
    (let ((proc (delay (pointer->procedure
                        int
                        (dynamic-func (symbol->string (quote <name>))
                                      liblepton)
                        '()))))
      ((force proc)))))


(define-getter lepton_log_flag_fatal)
(define-getter lepton_log_level_error)
(define-getter lepton_log_level_critical)
(define-getter lepton_log_level_warning)
(define-getter lepton_log_level_message)
(define-getter lepton_log_level_info)
(define-getter lepton_log_level_debug)

(define %lepton-log-level-alist
  `((error    . ,(logior lepton_log_flag_fatal
                         lepton_log_level_error))
    (critical . ,lepton_log_level_critical)
    (warning  . ,lepton_log_level_warning)
    (message  . ,lepton_log_level_message)
    (info     . ,lepton_log_level_info)
    (debug    . ,lepton_log_level_debug)))

(define (decode-log-level level)
  (assq-ref %lepton-log-level-alist level))

;;; Emit log MESSAGE to the message log.  The log domain DOMAIN
;;; should normally be #f, and MESSAGE should almost always be
;;; translated for all log levels other than 'debug.  LEVEL is the
;;; log level and it should be one of the symbols 'error,
;;; 'critical, 'message, 'info, or 'debug.
(define (%log! domain level message)
  (g_log (if (string? domain)
             (string->pointer domain)
             %null-pointer)
         (decode-log-level level)
         (string->pointer "%s")
         (string->pointer message)))

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
