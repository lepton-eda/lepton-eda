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
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)

  #:use-module (lepton ffi boolean)
  #:use-module (lepton ffi glib)
  #:use-module (lepton ffi)
  #:use-module (lepton file)
  #:use-module (lepton gerror)
  #:use-module (lepton log)

  #:use-module (schematic ffi)
  #:use-module (schematic window global)

  #:export (open-schematic))


(define (open-schematic *page **gerror)
  (define (gerror-error *error)
    (unless (null-pointer? *error)
      (let ((*err (dereference-pointer *error)))
        (unless (null-pointer? *err)
          (let ((message (gerror-message *err)))
            (g_clear_error *error)
            (log! 'warning "~A" message))))))

  (define *window (*current-window))
  (define *full-filename (lepton_page_get_filename *page))

  (let* ((*tmp-error (bytevector->pointer (make-bytevector (sizeof '*) 0)))
         (active_backup (f_has_active_autosave *full-filename *tmp-error))
         (stat_error (if (null-pointer? (dereference-pointer *tmp-error))
                         FALSE
                         TRUE)))

    (gerror-error *tmp-error)

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

      (load-schematic (lepton_page_get_toplevel *page)
                      *page
                      *full-filename
                      flags
                      **gerror))))
