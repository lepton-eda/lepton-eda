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
             (lepton ffi sch2pcb)
             (lepton m4))

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


(define %pcb-data-path (getenv "PCBDATA"))

(define %pcb-m4-path
  (let ((pcb-configure-m4-directory (and (not (string-null? %pcb-m4-dir))
                                         %pcb-m4-dir)))
    (if %pcb-data-path
        ;; If PCBDATA is set, use the value.
        (string-append %pcb-data-path file-name-separator-string "m4")

        (if pcb-configure-m4-directory
            ;; Use the default value passed in from the configure
            ;; script instead of trying to hard code a value which
            ;; is very likely wrong.
            pcb-configure-m4-directory
            ;; Neither PCBDATA was set nor PCBM4DIR has been
            ;; configured.  Fall back to using the "m4" subdirectory
            ;; in the current directory.
            (string-append (getcwd) file-name-separator-string "m4")))))


(define *%pcb-m4-path (string->pointer %pcb-m4-path))

(sch2pcb_set_default_m4_pcbdir *%pcb-m4-path)
(sch2pcb_set_m4_pcbdir *%pcb-m4-path)

(let ((number-of-args (length (program-arguments))))
  (if (= 1 number-of-args)
      (sch2pcb_usage)
      (begin
        (sch2pcb_get_args number-of-args
                          (string-list->bv-pointer (program-arguments)))
        (sch2pcb_main))))
