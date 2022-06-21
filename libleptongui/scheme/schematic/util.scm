;; Lepton EDA Schematic Capture
;; Scheme API
;; Copyright (C) 2011 Peter Brett <peter@peter-b.co.uk>
;; Copyright (C) 2017-2022 Lepton EDA Contributors
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
;;
(define-module (schematic util)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)

  #:use-module (lepton ffi)
  #:use-module (lepton gerror)

  #:use-module (schematic core gettext)
  #:use-module (schematic ffi)
  #:use-module (schematic window foreign)
  #:use-module (schematic window)

  #:export (show-file
            show-uri))

(define (show-uri uri)
  "Shows URI in the associated default application.  Raises an
error on failure."
  (define *window
    (or (and=> (current-window) window->pointer)
        (error "~S: Current window is unavailable." 'show-uri)))

  (define unknown-error (G_ "Unknown error"))

  (define (get-gerror-msg *error)
    (let ((*err (dereference-pointer *error)))
      (if (null-pointer? *err)
          unknown-error
          (gerror-message *err))))

  (define (process-gerror *error)
    (if (null-pointer? *error)
        unknown-error
        (let ((message (get-gerror-msg *error)))
          (g_clear_error *error)
          message)))

  (check-string uri 1)

  (let ((*error (bytevector->pointer (make-bytevector (sizeof '*) 0))))
    (unless (true? (x_show_uri *window
                               (string->pointer uri)
                               *error))

      (error (G_ "Could not launch URI ~S: ~A")
             uri
             (process-gerror *error)))))


(define (show-file filename)
  (show-uri (string-append "file://" filename)))
