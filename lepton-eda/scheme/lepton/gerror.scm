;;; Lepton EDA library - Scheme API
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

(define-module (lepton gerror)
  #:use-module (ice-9 match)
  #:use-module (system foreign)

  #:export (gerror-message))

(define (gerror-list *err)
  ;; GError struct consists of:
  ;; GQuark (uint32) domain
  ;; gint (int) code
  ;; gchar* (char*) message
  (parse-c-struct *err (list uint32 int '*)))

(define (gerror-message *err)
  (match (gerror-list *err)
    ((domain code message)
     (pointer->string message))
    (_ #f)))
