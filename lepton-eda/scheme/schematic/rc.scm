;;; Lepton EDA Schematic Capture
;;; Scheme API
;;; Copyright (C) 2021-2026 Lepton EDA Contributors
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

(define-module (schematic rc)
  #:use-module (system foreign)

  #:use-module (lepton ffi boolean)
  #:use-module (lepton ffi)
  #:use-module (lepton gerror)
  #:use-module (lepton gettext)
  #:use-module (lepton log)
  #:use-module (lepton rc)

  #:use-module (schematic dialog)
  #:use-module (schematic ffi)

  #:export (parse-gschemrc))


(define (parse-error **err *program-name)
  (define *err (if (null-pointer? **err)
                   (error "NULL GError.")
                   (dereference-pointer **err)))

  (define program-name
    (if (null-pointer? *program-name)
        "lepton-schematic"
        (pointer->string *program-name)))

  (define more-info-message
    (G_ "The lepton-schematic log may contain more information."))

  (define unknown-error-message
    (G_ "An unknown error occurred while parsing configuration files."))

  (define error-message
    (if (null-pointer? *err)
        ;; Take no chances; if err was not set for some reason,
        ;; it's a problem.
        unknown-error-message
        (gerror-message *err)))

  (define primary-dialog-message
    (G_ "Cannot load lepton-schematic configuration."))

  ;; Secondary dialog text.
  (define secondary-dialog-message
    (string-append error-message "\n\n" more-info-message))

  ;; Config files are allowed to be missing or skipped; check for
  ;; this.
  (unless (or (true? (config_error_file_noent *err))
              (true? (config_error_rc_twice *err)))
    (log! 'message (G_ "ERROR: ~A") error-message)

    (schematic-error-dialog primary-dialog-message
                            #:secondary-text secondary-dialog-message
                            #:title program-name)))


(define toplevel-initialized? #f)

(define (parse-gschemrc *toplevel)
  "Loads old (system, user, etc.) \"gschemrc\" files and new
configuration \".conf\" files.  Saves the values in the foreign
LeptonToplevel structure *TOPLEVEL and returns it.  Instead of
exiting on error as CLI tools do, displays error dialogs with
explanatory messages."
  (unless toplevel-initialized?
    (parse-rc "lepton-schematic"
              "gschemrc"
              #:handler parse-error
              #:*toplevel *toplevel)
    (set! toplevel-initialized? #t))
  *toplevel)
