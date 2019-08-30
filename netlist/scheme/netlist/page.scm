;;; Lepton EDA netlister
;;; Copyright (C) 2016-2017 gEDA Contributors
;;; Copyright (C) 2017-2019 Lepton EDA Contributors
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
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;;; MA 02111-1301 USA.

(define-module (netlist page)
  ; Import C procedures and variables
  #:use-module (netlist core gettext)

  #:use-module ((ice-9 rdelim)
                #:select (read-string)
                #:prefix rdelim:)
  #:use-module (geda log)
  #:use-module (lepton page)
  #:use-module (netlist option)

  #:export (filename->page))


(define quiet-mode (netlist-option-ref 'quiet))

;;; Reads file FILENAME and outputs a page with the same name.
(define (file-contents->page filename)
  (with-input-from-file filename
    (lambda ()
      (when (not quiet-mode)
        (log! 'message (_ "Loading schematic ~S\n") filename))
      (string->page filename (rdelim:read-string)))))

;;; Returns an opened page from PAGES by FILENAME. If no
;;; corresponding page found, returns #f.
(define (page-by-filename filename pages)
  (and (not (null? pages))
       (let ((page (car pages)))
         (if (string= filename (page-filename page))
             page
             (page-by-filename filename (cdr pages))))))

(define* (filename->page filename #:optional new-page?)
  "Given FILENAME, returns an opened page for it, or a new page if
none exists. Optional argument NEW-PAGE? can be used to force
creation of a new page for given filename."
  (if new-page?
      (file-contents->page filename)
      (or (page-by-filename filename (active-pages))
          (file-contents->page filename))))
