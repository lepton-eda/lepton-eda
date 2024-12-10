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
  #:use-module (lepton gerror)
  #:use-module (lepton log)

  #:use-module (schematic ffi)

  #:export (open-schematic))


;;; Flags defined in struct.h.
(define F_OPEN_RC 1)
(define F_OPEN_CHECK_BACKUP 2)
(define F_OPEN_FORCE_BACKUP 4)
(define F_OPEN_RESTORE_CWD 8)


(define (open-schematic *window *page *filename **gerror)
  (define (gerror-error *error)
    (unless (null-pointer? *error)
      (let ((*err (dereference-pointer *error)))
        (unless (null-pointer? *err)
          (let ((message (gerror-message *err)))
            (g_clear_error *error)
            (log! 'warning "~A" message))))))

  (when (null-pointer? *window)
    (error "NULL window"))

  (let* ((*tmp-error (bytevector->pointer (make-bytevector (sizeof '*) 0)))
         (active_backup (f_has_active_autosave *filename *tmp-error))
         (stat_error (if (null-pointer? (dereference-pointer *tmp-error))
                         FALSE
                         TRUE)))

    (gerror-error *tmp-error)

    (let* ((*backup-filename (if (true? active_backup)
                                 (f_get_autosave_filename *filename)
                                 %null-pointer))
           (*message (if (true? active_backup)
                         (f_backup_message *backup-filename stat_error)
                         %null-pointer))
           (flags (if (false? active_backup)
                      F_OPEN_RC
                      (if (true? (x_fileselect_load_backup *window *message))
                          (logior F_OPEN_RC F_OPEN_FORCE_BACKUP)
                          F_OPEN_RC))))
      (schematic_file_open *window
                           *page
                           *filename
                           **gerror
                           active_backup
                           *message
                           flags))))
