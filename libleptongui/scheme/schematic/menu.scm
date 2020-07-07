;;; Lepton EDA Schematic Capture
;;; Scheme API
;;; Copyright (C) 2020 Lepton EDA Contributors
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

;;; Procedures for working with menus.

(define-module (schematic menu)
  #:use-module (system foreign)

  #:export (add-menu))

(define libleptongui (dynamic-link "libleptongui"))

(define add-menu-entry!
  (pointer->procedure
   int
   (dynamic-func "s_menu_add_entry" libleptongui)
   (list '* '*)))

(define (add-menu name items)
  (and (string? name)
       (list? items)
       (add-menu-entry! (string->pointer name)
                        (scm->pointer items))
       #t))
