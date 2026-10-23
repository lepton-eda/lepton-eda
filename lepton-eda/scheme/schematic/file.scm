;;; Lepton EDA Schematic Capture
;;; Scheme API
;;; Copyright (C) 2024-2026 Lepton EDA Contributors
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

(define-module (schematic file)
  #:use-module (ice-9 format)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)

  #:use-module (lepton ffi boolean)
  #:use-module (lepton ffi glib)
  #:use-module (lepton ffi)
  #:use-module (lepton file)
  #:use-module (lepton gerror)
  #:use-module (lepton gettext)
  #:use-module (lepton log)
  #:use-module (lepton page foreign)
  #:use-module (lepton toplevel foreign)
  #:use-module (lepton toplevel)

  #:use-module (schematic ffi)
  #:use-module (schematic window global)

  #:export (open-schematic))


(define (report-error error-message *main-window *filename)
  (log! 'warning "~A" error-message)
  (schematic_dialog_load_file_error
   *main-window
   *filename
   (string->pointer error-message)))


(define (*open-schematic filename)
  (define (get-gerror-message **err)
    (let ((*err (dereference-pointer **err)))
      (and (not (null-pointer? *err))
           (gerror-message *err))))

  (define (gerror-error-message **err)
    (let ((msg (get-gerror-message **err)))
      (g_clear_error **err)
      msg))

  (define (report-autosave-backup-error **err)
    (let ((message (gerror-error-message **err)))
      (and message
           (log! 'warning "~A" message))))

  (define *window (*current-window))
  (define *toplevel (toplevel->pointer (current-toplevel)))
  (define *main-window (schematic_window_get_main_window *window))
  (define *filename (string->pointer filename))
  (define *page (lepton_page_new *toplevel *filename))
  (define *full-filename (lepton_page_get_filename *page))

  (let* ((*tmp-error (bytevector->pointer (make-bytevector (sizeof '*) 0)))
         (active_backup (f_has_active_autosave *full-filename *tmp-error))
         (stat_error (if (null-pointer? (dereference-pointer *tmp-error))
                         FALSE
                         TRUE)))
    (report-autosave-backup-error *tmp-error)

    (let* ((*backup-filename (if (true? active_backup)
                                 (f_get_autosave_filename *full-filename)
                                 %null-pointer))
           (*message (if (true? active_backup)
                         (f_backup_message *backup-filename stat_error)
                         %null-pointer))
           (flags (if (and (true? active_backup)
                           (true? (x_fileselect_load_backup *window *message)))
                      (logior F_OPEN_RC F_OPEN_FORCE_BACKUP)
                      F_OPEN_RC)))
      (when (true? active_backup)
        (g_string_free *message TRUE))

      (let* ((**gerror
              (bytevector->pointer (make-bytevector (sizeof '*) 0)))
             (*updated-page (load-schematic *toplevel
                                            *page
                                            *full-filename
                                            flags
                                            **gerror)))
        (if *updated-page
            (pointer->page *updated-page)
            ;; Loading failed.  Report the error and delete the
            ;; new page.
            (let ((error-message (or (gerror-error-message **gerror)
                                     (G_ "Unknown error"))))
              (report-error error-message *main-window *filename)
              (lepton_page_delete *toplevel *page)
              #f))))))


(define (open-schematic filename)
  "Opens file FILENAME and returns schematic page.  On errors,
reports them to log and raises a dialog window with error message."
  (catch #t
    (lambda () (*open-schematic filename))
    (lambda (key subr message args rest)
      (let ((error-message (format #f "~?" message args)))
        (report-error error-message
                      (schematic_window_get_main_window (*current-window))
                      (string->pointer filename)))
      #f)))
