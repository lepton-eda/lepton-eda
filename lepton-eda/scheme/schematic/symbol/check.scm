;; Lepton EDA Schematic Capture
;; Scheme API
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

(define-module (schematic symbol check)
  #:use-module (srfi srfi-1)
  #:use-module (system foreign)
  #:use-module (lepton object foreign)
  #:use-module (lepton page)

  #:use-module (schematic gettext)
  #:use-module (schematic dialog)
  #:use-module (schematic window)
  #:use-module (symbol blame)
  #:use-module ((symbol check) #:prefix sym:)

  #:export (check-symbol
            object-blaming-info))

(define (blaming-info->string x)
  (string-join (map cdr (object-blames x)) "\n" 'suffix))

(define (check-symbol)
  "Checks the active page which should be a symbol page, and returns
its blamed objects, that is, the objects that would trigger report
of issues when checking the file by the lepton-symcheck utility."
  (define (warning-or-error blame)
    (or (eq? 'error (car blame)) (eq? 'warning (car blame))))

  (define (blamed-object? object)
    (not (null? (filter warning-or-error (object-blames object)))))

  (let ((objects (sym:check-symbol (active-page)))
        (page-info (blaming-info->string (active-page))))
    (schematic-message-dialog (if (string-null? page-info)
                                  (G_ "Symbol has no pin info.")
                                  page-info))
    ;; Return the list of pointers to further process them in C
    ;; code.
    (map object->pointer (filter blamed-object? objects))))

;;; OBJECT* is a foreign pointer to LeptonObject.
(define (object-blaming-info *object)
  "Returns concatenated string of blaming info for *OBJECT which
should be a C foreign pointer to a LeptonObject instance.  The
info is identical with what the lepton-symcheck utility returns.
Each line in the string represents one issue with the *OBJECT."
  (blaming-info->string (pointer->object *object)))
