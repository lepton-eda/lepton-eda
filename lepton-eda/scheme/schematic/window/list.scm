;;; Lepton EDA Schematic Capture
;;; Scheme API
;;; Copyright (C) 2022 Lepton EDA Contributors
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


(define-module (schematic window list)
  #:export (schematic-windows
            window-exists?
            add-window!
            remove-window!))

;;; List of lepton-schematic windows.
(define %schematic-windows '())

(define (schematic-windows)
  "Returns the list of currently open windows."
  %schematic-windows)


(define (window-exists? window)
  "Returns #t if *WINDOW exists in the set of currently open
windows.  Otherwise returns #f."
  (and (memq window (schematic-windows)) #t))


(define (add-window! window)
  "Adds *WINDOW to the list of currently open windows."
  (set! %schematic-windows (cons window %schematic-windows)))


(define (remove-window! window)
  "Removes *WINDOW from the list of currently open windows."
  (set! %schematic-windows (delq window %schematic-windows)))
