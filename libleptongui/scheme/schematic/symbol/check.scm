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
  #:use-module (lepton page)
  #:use-module (schematic core gettext)
  #:use-module (schematic dialog)
  #:use-module (schematic window)
  #:use-module (symbol blame)
  #:use-module ((symbol check) #:prefix sym:))

(define-public (check-symbol)
  "Checks the active page which should be a symbol page, and returns
its blamed objects, that is, the objects that would trigger report
of issues when checking the file by the lepton-symcheck utility."
  (define (warning-or-error blame)
    (or (eq? 'error (car blame)) (eq? 'warning (car blame))))

  (define (blamed-object? object)
    (not (null? (filter warning-or-error (object-blames object)))))

  (let ((page (active-page)))
    (sym:check-symbol page)
    (let ((page-info (object-blaming-info page)))
      (schematic-message-dialog (if (string-null? page-info)
                                    (G_ "Symbol has no pin info.")
                                    page-info)))
    (filter blamed-object? (page-contents page))))

(define-public (object-blaming-info object)
  "Returns concatenated string of blaming info for OBJECT.  The info
is identical with what the lepton-symcheck utility returns.  Each
line in the string represents one issue with the OBJECT."
  (string-join (map cdr (object-blames object)) "\n" 'suffix))
