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
  #:use-module (system foreign)

  #:use-module (lepton ffi sch2pcb)
  #:use-module (sch2pcb format)

  #:export (lookup-footprint))

(define (lookup-footprint path name)
  "Searches for a Pcb element (footprint) file by NAME in PATH.
If an element is found, returns a pointer to its C string name,
otherwise returns %null-pointer."
  (let* ((*dir-path (string->pointer path))
         (*dir (sch2pcb_find_element_open_dir *dir-path)))
    (if (null-pointer? *dir)
        %null-pointer
        (begin
          (extra-verbose-format "\t  Searching: ~S for ~S\n"
                                path
                                name)
          (sch2pcb_find_element *dir-path
                                (if name
                                    (string->pointer name)
                                    %null-pointer)
                                *dir)))))
