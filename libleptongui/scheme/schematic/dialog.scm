;;; Lepton EDA Schematic Capture
;;; Scheme API
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


(define-module (schematic dialog)
  #:use-module (srfi srfi-1)
  #:use-module (system foreign)

  #:use-module (lepton ffi glib)
  #:use-module (lepton ffi)
  #:use-module (lepton log)
  #:use-module (schematic ffi)

  #:export (schematic-message-dialog
            schematic-confirm-dialog
            schematic-fileselect-dialog))

(define (schematic-message-dialog message)
  "Opens GTK message dialog with MESSAGE and one OK button."
  (generic_msg_dialog (string->pointer message)))

(define (schematic-confirm-dialog message)
  "Opens GTK confirmation dialog with MESSAGE and buttons YES and
NO.  Returns #t if the button YES was pressed.  Otherwise returns
#f."
  (not (zero? (generic_confirm_dialog (string->pointer message)))))

(define (schematic-fileselect-dialog message template . flags)
  "Opens GTK file selection dialog with MESSAGE as dialog's title.
TEMPLATE is used to select files when appropriate.  Several FLAGS
can be used to change the behaviour of the dialog: 'may_exist,
'must_exist, 'must_not_exist, 'save, and 'open.  Returns the name
of selected file, or #f if nothing has been selected."
  ;; Corresponds to FSB_* flags in C code.
  (define flag-alist
    '((may_exist . 1)
      (must_exist . 2)
      (must_not_exist . 4)
      (save . 256)
      (open . 512)))

  (define (flag->int flag)
    (let ((result (assq-ref flag-alist flag)))
      (when (not result)
        (log! 'warning
              "schematic-fileselect-dialog: Flag ~S is not supported."
              flag))
      result))

  (let* ((bit-flags (apply logior (filter-map flag->int flags)))
         (str-pointer (generic_filesel_dialog (string->pointer message)
                                              (string->pointer template)
                                              bit-flags)))
    (and (not (null-pointer? str-pointer))
         (let ((filename (pointer->string str-pointer)))
           (g_free str-pointer)
           filename))))
