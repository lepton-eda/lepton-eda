;; gEDA - GPL Electronic Design Automation
;; gschem - gEDA Schematic Capture - Scheme API
;; Copyright (C) 2011 Peter Brett
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
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA
;;

(define-module (gschem attrib)

  #:use-module (gschem core attrib))

;; add-attrib! target name value visible attribute-mode
;;
;; Create a new attribute, either attached to a target object in the
;; current page, or floating in the current page if target is #f.  The
;; name and value for the attribute must be strings, and if visible is
;; #f, the attribute will be invisible.  The attribute-mode controls
;; which parts of the attribute will be visible, and must be one of
;; the following symbols:
;;
;;   name
;;   value
;;   both
;;
;; See also active-page in the (gschem window) module.
(define-public add-attrib! %add-attrib!)
