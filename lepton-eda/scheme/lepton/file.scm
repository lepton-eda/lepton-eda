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

  #:export (load-schematic

            ;; Flags
            F_OPEN_RC
            F_OPEN_CHECK_BACKUP
            F_OPEN_FORCE_BACKUP
            F_OPEN_RESTORE_CWD))


;;; Flags defined in struct.h.
(define F_OPEN_RC 1)
(define F_OPEN_CHECK_BACKUP 2)
(define F_OPEN_FORCE_BACKUP 4)
(define F_OPEN_RESTORE_CWD 8)


(define (load-schematic *toplevel *page *filename flags **gerror)
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

    (let ((opened? (true? (f_open *page
                                  *full-filename
                                  flags
                                  **gerror
                                  ;; Backup filename.
                                  (f_get_autosave_filename *full-filename)))))
      ;; Reset the directory to the value it had when f_open() was
      ;; called.
      (when (logtest flags F_OPEN_RESTORE_CWD)
        ;; FIXME[2017-02-21] Libraries should not be changing the
        ;; current working directory.
        (chdir saved-cwd))
      opened?))

  (and (open-file)
       *page))
