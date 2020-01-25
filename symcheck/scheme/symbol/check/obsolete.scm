;;; Lepton EDA Symbol Checker
;;; Scheme API
;;; Copyright (C) 2017-2020 Lepton EDA Contributors
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
;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA

(define-module (symbol check obsolete)
  #:use-module (geda attrib)
  #:use-module (lepton object)
  #:use-module (symbol gettext)
  #:use-module (symbol blame)

  #:export (check-obsolete-pin-attrib
            check-obsolete-floating-attrib))

(define-syntax blame-error
  (syntax-rules ()
    ((_ object msg arg ...)
     (blame-object object 'error (format #f (gettext msg) arg ...)))))

(define regex-old-pin (make-regexp "^pin[0-9]+$"))
(define regex-old-slot (make-regexp "^slot[0-9]+$"))

(define (check-obsolete-pin-attrib object)
  "Checks if OBJECT is an obsolete pin attribute. Returns OBJECT."
  (let ((name (attrib-name object))
        (value (attrib-value object)))
    (when (regexp-exec regex-old-pin name)
      (blame-error object "Obsolete pin#=# attribute: ~A=~A" name value))
    object))

(define (check-obsolete-floating-attrib object)
  "Checks if OBJECT is an obsolete floating attribute. Returns OBJECT."
  (let ((name (attrib-name object))
        (value (attrib-value object)))
    (when (regexp-exec regex-old-slot name)
      (blame-error object "Obsolete slot#=# attribute: ~A=~A" name value))
    object))
