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


(define-module (schematic hierarchy)
  #:use-module (system foreign)

  #:use-module (lepton ffi check-args)
  #:use-module (lepton library)
  #:use-module (lepton page foreign)

  #:use-module (schematic ffi)
  #:use-module (schematic window foreign)

  #:export (hierarchy-down-schematic))


(define (*hierarchy-down-schematic window
                                   filename
                                   parent-page
                                   page-control
                                   *error)
  (define *window (check-window window 1))
  (define *filename (and (check-string filename 2)
                         (string->pointer filename)))
  (define *parent-page (check-page parent-page 3))

  (check-integer page-control 4)

  (let ((source-filename (get-source-library-file filename)))
    (if source-filename
        (s_hierarchy_down_schematic_single *window
                                           (string->pointer source-filename)
                                           *parent-page
                                           0
                                           *error)
        (begin
          (schematic_hierarchy_set_error_nolib *error)

          %null-pointer))))


(define (hierarchy-down-schematic window
                                  filename
                                  parent-page
                                  page-control
                                  *error
                                  scheme-error-handler)
  (catch #t
    (lambda ()
      (*hierarchy-down-schematic window
                                 filename
                                 parent-page
                                 page-control
                                 *error))
    ;; (lambda (key subr message args rest) ...)
    scheme-error-handler))
