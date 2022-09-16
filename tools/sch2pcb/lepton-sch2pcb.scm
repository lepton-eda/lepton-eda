;;; lepton-sch2pcb -- transform schematics to PCB
;;;
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
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA


(use-modules (rnrs bytevectors)
             (system foreign)
             (lepton ffi sch2pcb))

;;; FIXME: this is a function from (lepton config).  Probably it
;;; has to be moved to some dedicated FFI module.
(define (string-list->bv-pointer ls)
  (bytevector->pointer
   (uint-list->bytevector
    (map pointer-address
         (append (map string->pointer ls)
                 ;; NULL-terminate the list of strings to be
                 ;; passed to g_strfreev().
                 (list %null-pointer)))
    (native-endianness)
    (sizeof '*))))


(sch2pcb_main (length (program-arguments))
              (string-list->bv-pointer (program-arguments)) )
