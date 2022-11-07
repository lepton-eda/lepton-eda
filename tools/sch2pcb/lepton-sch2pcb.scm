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
             (srfi srfi-1)
             (system foreign)
             (lepton ffi boolean)
             (lepton ffi sch2pcb)
             (lepton gettext)
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

;;; Produces a backup file name given that BASE is an initial
;;; name.  If BASE exists, adds a numerical index as a suffix to
;;; it starting with 0 and checks if the file exists incrementing
;;; the index until a non-existing file is found.
(define (next-backup-name base)
  (let loop ((name base)
             (i 0))
    (if (file-exists? name)
        (let ((newname (format #f "~A~A" base i)))
          (loop newname (1+ i)))
        name)))


;;; Runs THUNK reporting errors to the current error port without
;;; backtrace.
(define (call-protected thunk)
  (catch #t
    thunk
    (lambda (key subr message args rest)
      (format (current-error-port) (G_ "ERROR: ~?.\n") message args))))


;;; A convenience function for deleting FILENAME with reporting
;;; possible errors without backtrace.
(define (delete-file* filename)
  (call-protected (lambda () (delete-file filename))))


;;; Report processing results.
(define (report-results pcb-filename
                        pcb-new-filename
                        bak-filename
                        pins-filename
                        net-filename
                        initial-pcb?)
  (sch2pcb_main (string->pointer pcb-filename)
                (string->pointer pcb-new-filename)
                (string->pointer bak-filename)
                (string->pointer pins-filename)
                (string->pointer net-filename)
                (if initial-pcb? TRUE FALSE)))


(let ((number-of-args (length (program-arguments))))
  (if (= 1 number-of-args)
      (sch2pcb_usage)
      (begin
        (sch2pcb_get_args number-of-args
                          (string-list->bv-pointer (program-arguments)))
        (sch2pcb_load_extra_project_files)
        (sch2pcb_add_default_m4_files)
        (if (null-pointer? (sch2pcb_get_schematics))
            (sch2pcb_usage)
            (begin
              ;; Defaults for the newlib element directory search path
              ;; if not configured in the project file.
              (when (not (zero? (sch2pcb_get_verbose_mode)))
                (format #t "Processing PCBLIBPATH=~S\n" %pcb-lib-path))
              (for-each
               (lambda (x)
                 (sch2pcb_element_directory_list_append (string->pointer x)))
               (filter-map
                (lambda (x) (false-if-exception (canonicalize-path x)))
                (cons "packages" (parse-path %pcb-lib-path))))
              (let* ((*schematic-basename (sch2pcb_get_sch_basename))
                     (schematic-basename (if (null-pointer? *schematic-basename)
                                             ""
                                             (pointer->string *schematic-basename)))
                     (pins-filename (string-append schematic-basename ".cmd"))
                     (net-filename (string-append schematic-basename ".net"))
                     (pcb-filename (string-append schematic-basename ".pcb"))
                     (bak-filename (next-backup-name (string-append schematic-basename
                                                                    ".pcb.bak")))
                     (pcb-file-exists? (file-exists? pcb-filename))
                     (initial-pcb? (not pcb-file-exists?))
                     (pcb-new-filename (if pcb-file-exists?
                                           (string-append schematic-basename ".new.pcb")
                                           pcb-filename)))
                (when pcb-file-exists?
                  (sch2pcb_make_pcb_element_list (string->pointer pcb-filename)))
                (unless (true? (sch2pcb_run_netlister (string->pointer pins-filename)
                                                      (string->pointer net-filename)
                                                      (string->pointer pcb-new-filename)
                                                      (sch2pcb_get_sch_basename)
                                                      (sch2pcb_get_schematics)))
                  (format (current-error-port) (G_ "Failed to run netlister\n"))
                  (exit 1))
                (when (zero? (sch2pcb_add_elements (string->pointer pcb-new-filename)))
                  (delete-file* pcb-new-filename)
                  (when initial-pcb?
                    (format #t "No elements found, so nothing to do.\n")
                    (exit 0)))
                (when (true? (sch2pcb_get_fix_elements))
                  (sch2pcb_update_element_descriptions (string->pointer pcb-filename)
                                                       (string->pointer bak-filename)))
                (sch2pcb_prune_elements (string->pointer pcb-filename)
                                        (string->pointer bak-filename))
                (report-results pcb-filename
                                pcb-new-filename
                                bak-filename
                                pins-filename
                                net-filename
                                initial-pcb?)))))))
