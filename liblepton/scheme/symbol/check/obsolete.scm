;;; Lepton EDA Symbol Checker
;;; Scheme API
;;; Copyright (C) 2017-2021 Lepton EDA Contributors
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

(define-module (symbol check obsolete)
  #:use-module (lepton attrib)
  #:use-module (lepton core gettext)
  #:use-module (lepton object)
  #:use-module (symbol blame)

  #:export (check-obsolete-pin-attrib
            check-obsolete-floating-attrib
            obsolete-pin#-attrib?
            obsolete-slot#-attrib?))

(define-syntax blame-error
  (syntax-rules ()
    ((_ object msg arg ...)
     (blame-object object 'error (format #f msg arg ...)))))

(define regex-old-pin (make-regexp "^pin[0-9]+$"))
(define regex-old-slot (make-regexp "^slot[0-9]+$"))

(define (obsolete-pin#-attrib? object)
  "Predicate that returns #t if OBJECT is an obsolete pin#=
attribute.  Otherwise returns #f."
  (and (attribute? object)
       (not (not (regexp-exec regex-old-pin (attrib-name object))))))

(define (obsolete-slot#-attrib? object)
  "Predicate that returns #t if OBJECT is an obsolete slot#=
attribute.  Otherwise returns #f."
  (and (attribute? object)
       (regexp-exec regex-old-slot (attrib-name object))))

(define (check-obsolete-pin-attrib object)
  "Checks if OBJECT is an obsolete pin attribute. Returns OBJECT."
  (let ((name (attrib-name object))
        (value (attrib-value object)))
    (when (obsolete-pin#-attrib? object)
      (blame-error object (G_ "Obsolete pin#=# attribute: ~A=~A") name value))
    object))

(define (check-obsolete-floating-attrib object)
  "Checks if OBJECT is an obsolete floating attribute. Returns OBJECT."
  (let ((name (attrib-name object))
        (value (attrib-value object)))
    (when (obsolete-slot#-attrib? object)
      (blame-error object (G_ "Obsolete slot#=# attribute: ~A=~A") name value))
    object))
