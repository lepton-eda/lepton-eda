;;; Lepton EDA library - Scheme API
;;; Copyright (C) 2007-2016 gEDA Contributors
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

;;; File system related procedures.

(define-module (lepton file-system)

  #:export (regular-file?
            directory?))

(define (regular-file? path)
  "Returns #t if the given path is a regular file, otherwise #f."
  (eqv? (stat:type (stat path)) 'regular))

(define (directory? path)
  "Returns #t if the given path is a directory file, otherwise #f."
  (eqv? (stat:type (stat path)) 'directory ))
