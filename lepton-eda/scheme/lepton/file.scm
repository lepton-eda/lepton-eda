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
  #:use-module (system foreign)

  #:use-module (lepton ffi boolean)
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
  (define *saved-cwd
    (if (logtest flags F_OPEN_RESTORE_CWD)
        (string->pointer (getcwd))
        %null-pointer))

  (define filename (pointer->string *filename))
  ;; Get absolute path to the file.
  (define full-filename (canonicalize-path filename))
  (define *full-filename (string->pointer full-filename))

  (define (open-file)
    ;; Set page filename to the absolute path to the file.
    (lepton_page_set_filename *page *full-filename)

    (true? (f_open *toplevel
                   *page
                   *full-filename
                   flags
                   **gerror
                   *saved-cwd)))

  (and (open-file)
       *page))
