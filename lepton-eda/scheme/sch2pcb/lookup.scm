;;; Lepton EDA schematic -> pcb conversion utility
;;;
;;; Copyright (C) 2022-2025 Lepton EDA Contributors
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
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

(define-module (sch2pcb lookup)
  #:use-module (ice-9 ftw)

  #:use-module (lepton file-system)
  #:use-module (sch2pcb format)

  #:export (lookup-footprint))


(define (lookup-footprint dir-path element-name)
  "Searches for a Pcb element (footprint) file by ELEMENT-NAME in
DIR-PATH recursively.  If an element is found, returns the path to
it as a string, otherwise returns #f."
  (define (fold-files filename path)
    (define (id name stat result) result)
    (define (enter? name stat result) result)
    (define down id)
    (define up id)
    (define skip id)
    (define (error name stat errno result)
      (format-warning "Could not open ~S: ~A~%" name (strerror errno))
      result)
    (define (leaf-proc name stat result)
      (if (directory? name)
          ;; Skip directories here.
          result
          ;; Search for "filename" and "filename.fp".
          (let ((bname (basename name))
                (alt-filename (string-append filename ".fp")))
            (if (or (string= bname filename)
                    (string= bname alt-filename))
                (cons name result)
                result))))

    (file-system-fold enter? leaf-proc down up skip error
                      '()
                      path))

  (let ((files (fold-files element-name dir-path)))
    ;; Return #f if no files found.
    (and (not (null? files))
         ;; Otherwise return the first file found.
         (car files))))
