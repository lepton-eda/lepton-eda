;;; Lepton EDA library - Scheme API
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

(define-module (lepton file)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)

  #:use-module (lepton ffi boolean)
  #:use-module (lepton ffi glib)
  #:use-module (lepton ffi)
  #:use-module (lepton gerror)
  #:use-module (lepton gettext)
  #:use-module (lepton log)

  #:export (load-schematic
            backup-message

            ;; Flags
            F_OPEN_RC
            F_OPEN_CHECK_BACKUP
            F_OPEN_FORCE_BACKUP
            F_OPEN_RESTORE_CWD))



;;; Behaviour flags when opening files.

;;; Execute RC files found in the target directory.
(define F_OPEN_RC 1)
;;; Log a warning if a backup is found for the file being loaded.
(define F_OPEN_CHECK_BACKUP 2)
;;; Load the last autosaved file backup if it is newer than the
;;; file itself.
(define F_OPEN_FORCE_BACKUP 4)
;;; Do not restore the previous working directory after loading
;;; file.
(define F_OPEN_RESTORE_CWD 8)




(define (backup-message backup-filename stat-error?)
  (string-append
   (format #f
           (G_ "\nWARNING: Found an autosave backup file:\n  ~S.\n\n")
           backup-filename)
   (if stat-error?
       (G_ "I could not guess if it is newer, so you have to do it manually.\n")
       (G_ "The backup copy is newer than the schematic, so it seems you should load it instead of the original file.\n"))
   (G_ "lepton-schematic usually makes backup copies automatically, and this situation happens when it crashed or it was forced to exit abruptly.")))


(define (check-backup *full-filename *backup-filename)
  (define **err (bytevector->pointer (make-bytevector (sizeof '*) 0)))
  ;; Check if there is a newer autosave backup file.
  (define active-backup?
    (true? (f_has_active_autosave *full-filename **err)))

  (define stat-error?
    (not (null-pointer? (dereference-pointer **err))))

  (when stat-error?
    (log! 'warning "~A" (gerror-message **err))
    (g_clear_error **err))

  (when active-backup?
    (log! 'warning
          "~A"
          (backup-message (pointer->string *backup-filename)
                          stat-error?))
    (log! 'warning
          (G_ "\nRun lepton-schematic and correct the situation.\n"))))


(define (load-schematic *toplevel *page *filename flags **gerror)
  "Loads the schematic file *FILENAME to *PAGE in the toplevel
environment *TOPLEVEL.  The file is loaded with fine-grained
control over behaviour.  The function carries out a number of
actions depending on the FLAGS set.  If F_OPEN_RC is set, executes
RC files found in the target directory.  If F_OPEN_CHECK_BACKUP is
set, warns user if a backup is found for the file being loaded.
If F_OPEN_FORCE_BACKUP is set, unconditionally loads the backup
instead of the file.  If F_OPEN_RESTORE_CWD is set, restores the
working directory to that which was current before file loading.
Errors are reported via **GERROR.  Use NULL to disable error
reporting.  The function returns #f on failure, updated *PAGE on
success."
  ;; Cache the cwd so we can restore it later.
  (define saved-cwd (getcwd))

  (define filename (pointer->string *filename))
  ;; Get absolute path to the file.
  (define full-filename (canonicalize-path filename))
  (define *full-filename (string->pointer full-filename))

  (define file-directory (and=> full-filename dirname))
  (define *file-directory (if file-directory
                              (string->pointer file-directory)
                              %null-pointer))
  (define (open-file)
    ;; Set page filename to the absolute path to the file.
    (lepton_page_set_filename *page *full-filename)

    ;; Before we open the page, let's load the corresponding
    ;; gafrc.  First cd into file's directory.  Possible errors
    ;; should be handled in the caller of the function.
    (and file-directory
         ;; FIXME[2017-02-21] Libraries should not be changing the
         ;; current working directory.
         (chdir file-directory))

    ;; Now open RC and process file.
    (when (logtest flags F_OPEN_RC)
      (let ((**err (bytevector->pointer (make-bytevector (sizeof '*) 0))))
        (g_rc_parse_local *toplevel
                          (string->pointer "gafrc")
                          *file-directory
                          **err)
        (f_process_error (dereference-pointer **err))
        (g_clear_error **err)))

    (let ((*backup-filename (f_get_autosave_filename *full-filename))
          (load-backup? (logtest flags F_OPEN_FORCE_BACKUP)))
      (when (and (logtest flags F_OPEN_CHECK_BACKUP)
                 (not load-backup?))
        (check-backup *full-filename *backup-filename))

      ;; Now that we have set the current directory and read
      ;; the RC file, it's time to read in the file.
      (if load-backup?
          ;; Load the backup file.
          (o_read *page *backup-filename **gerror)
          ;; Load the original file.
          (o_read *page *full-filename **gerror))

      (let ((opened?
             (or (null-pointer? **gerror)
                 (null-pointer? (dereference-pointer **gerror)))))
        (if (not load-backup?)
            ;; If it's not the backup file.
            (lepton_page_set_changed *page 0)
            ;; We are loading the backup file, so the program
            ;; should ask the user if save it or not when closing
            ;; the page.
            (lepton_page_set_changed *page 1))

        ;; Reset the directory to the value it had when the
        ;; function was called.
        (when (logtest flags F_OPEN_RESTORE_CWD)
          ;; FIXME[2017-02-21] Libraries should not be changing
          ;; the current working directory.
          (chdir saved-cwd))
        opened?)))

  (and (open-file)
       *page))
