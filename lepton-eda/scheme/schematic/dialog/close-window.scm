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


(define-module (schematic dialog close-window)
  #:use-module (srfi srfi-1)
  #:use-module (system foreign)

  #:use-module (lepton ffi glib)
  #:use-module (lepton ffi)
  #:use-module (lepton page foreign)
  #:use-module (lepton page)

  #:use-module (schematic ffi gtk)
  #:use-module (schematic ffi)
  #:use-module (schematic gtk helper)
  #:use-module (schematic window foreign)
  #:use-module (schematic window page)

  #:export (close-window-dialog))


(define (make-close-window-dialog *window *changed-pages)
  (schematic_close_confirmation_dialog_new *window *changed-pages))


(define (run-close-window-dialog *dialog *window *toplevel)
  (define window (pointer->window *window))
  (define get-selected-pages
    schematic_close_confirmation_dialog_get_selected_pages)
  (define response
    (gtk-response->symbol
     (schematic_close_confirmation_dialog_run *dialog)))

  (define not-changed? (negate page-dirty?))

  (define (save-page page)
    (window-set-toplevel-page! window page)
    (i_callback_file_save %null-pointer *window)

    page)

  (define (all-pages-saved?)
    (let ((selected-pages
           (glist->list (get-selected-pages *dialog)
                        pointer->page
                        'free)))
      ;; For untitled pages, i_callback_file_save() starts the
      ;; file name selection dialog.  If the user cancels it for a
      ;; page, the page remains changed but unsaved.  In such a
      ;; case #f is returned and the window won't be closed.
      (every not-changed?
             (map save-page selected-pages))))

  (case response
    ;; Close without saving.  Just quit discarding changes.
    ((no) 'close)
    ;; Save changes and exit if all saved.
    ((yes)
     (if (all-pages-saved?)
         ;; Anything has been saved.  Exit.
         'close
         ;; If some untitled page has been not saved yet, the
         ;; window won't be closed.  Switch back to the page we
         ;; were on.
         'restore))
    ;; When the user hit cancel or did other actions that
    ;; otherwise destroyed the dialog window without a proper
    ;; response, there is nothing to do.
    (else #f)))


(define (close-window-dialog window)
  (define *window (check-window window 1))
  (define *toplevel (schematic_window_get_toplevel *window))
  (define *unsaved-pages (lepton_toplevel_get_changed_pages *toplevel))
  (define (run-dialog-and-get-result)
    (define *dialog (make-close-window-dialog *window *unsaved-pages))
    (define result (run-close-window-dialog *dialog *window *toplevel))
    ;; First close the dialog.
    (gtk_widget_destroy *dialog)
    result)

  ;; Last chance to save possible unsaved pages.
  (if (null-pointer? *unsaved-pages)
      ;; If there is no page with unsaved changes, just close the
      ;; window.
      'close
      ;; Otherwise, actually run the close confirmation dialog and
      ;; return the result.
      (run-dialog-and-get-result)))
