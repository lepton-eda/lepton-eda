;;; Lepton EDA Schematic Capture
;;; Scheme API
;;; Copyright (C) 2023-2026 Lepton EDA Contributors
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


(define-module (schematic dialog close-page)
  #:use-module (system foreign)

  #:use-module (lepton page foreign)
  #:use-module (lepton page)

  #:use-module (schematic ffi gtk)
  #:use-module (schematic ffi)
  #:use-module (schematic gtk helper)
  #:use-module (schematic window foreign)
  #:use-module (schematic window page)

  #:export (close-page-dialog))

(define (close-page-dialog-run window page)
  (define *window (check-window window 1))
  (define *page (check-page page 2))
  (define *dialog (schematic_close_page_dialog_new *page))
  (define response
    (gtk-response->symbol
     (schematic_close_confirmation_dialog_run *dialog)))

  (define (save-page!)
    (window-set-toplevel-page! window page)
    (i_callback_file_save %null-pointer *window)
    ;; Has the page been really saved?
    (not (page-dirty? page)))

  (define result
    (case response
      ;; Close without saving discarding changes.
      ((no) #t)
      ;; Save the page or, if the page is untitled and
      ;; the user cancels saving it, do not close it.
      ((yes) (save-page!))
      ;; When the user hit cancel or did other actions
      ;; that otherwise destroyed the dialog window
      ;; without a proper response, there is nothing to
      ;; do.
      (else #f)))

  (gtk_widget_destroy *dialog)
  result)


(define (close-page-dialog window page)
  "Runs the dialog that asks the user for confirmation before
closing PAGE in WINDOW if it has unsaved changes.  It displays a
message inviting the user to cancel the closing, or to discard the
changes, or to save the changes to a file.  Returns #t if it is OK
to continue with closing the page, and #f otherwise."
  (define *window (check-window window 1))
  (define *page (check-page page 2))
  (define active-page
    (pointer->page (schematic_window_get_active_page *window)))

  (define (run-dialog)
    (let ((result (close-page-dialog-run window page)))
      ;; Switch back to the page we were on if it wasn't the one
      ;; being closed.
      (when (and active-page
                 (not (eq? active-page page)))
        (window-set-toplevel-page! window active-page))
      result))

  (or (not (page-dirty? page))
      (run-dialog)))
