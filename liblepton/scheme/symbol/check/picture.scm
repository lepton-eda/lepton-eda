;;; Lepton EDA Symbol Checker
;;; Scheme API
;;; Copyright (C) 2017-2022 Lepton EDA Contributors
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

(define-module (symbol check picture)
  #:use-module (ice-9 match)

  #:use-module (lepton gettext)
  #:use-module (lepton object)
  #:use-module (symbol blame)

  #:export (check-picture-size
            check-picture-file
            check-picture))

(define (check-picture-size object)
  "Checks picture OBJECT size."
  (define (blame-zero-picture object)
    (blame-object object
                     'error
                     (format #f
                             (G_ "Zero sized picture at ~A")
                             (picture-top-left object))))

  (match `(,(picture-top-left object) . ,(picture-bottom-right object))
    (((x . y0) . (x . y1)) (blame-zero-picture object))
    (((x0 . y) . (x1 . y)) (blame-zero-picture object))
    (_ #f)))


(define (check-picture-file object)
  "Checks that picture OBJECT's file exists and is readable."
  (let ((filename (picture-filename object)))
   (and (not (access? filename R_OK))
        (blame-object object
                      'error
                      (format #f
                              (G_ "Picture file ~S does not exist or is not readable.")
                              filename)))))

(define (check-picture object)
  "Checks picture OBJECT:
  * Checks if its file exist and is readable.
  * Checks that it has non-zero size on canvas."
  (check-picture-file object)
  (check-picture-size object))
