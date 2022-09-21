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


(use-modules (srfi srfi-1)
             (system foreign)
             (lepton ffi boolean)
             (lepton ffi sch2pcb)
             (lepton gettext)
             (lepton m4)
             (lepton srfi-37)
             (lepton version))

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


;;; Parse command line options.
(define (parse-command-line)
  (reverse
   (args-fold
    (cdr (program-arguments))
    (list
     (option '(#\v "verbose") #f #f
             (lambda (opt name arg seeds)
               (sch2pcb_increment_verbose_mode)
               seeds))
     (option '("fix-elements") #f #f
             (lambda (opt name arg seeds)
               (sch2pcb_set_fix_elements TRUE)
               seeds))
     (option '("gnetlist-arg") #t #f
             (lambda (opt name arg seeds)
               (sch2pcb_extra_gnetlist_arg_list_append (string->pointer arg))
               seeds))
     (option '(#\h #\? "help") #f #f
             (lambda (opt name arg seeds)
               (sch2pcb_usage)))
     (option '(#\r "remove-unfound") #f #f
             (lambda (opt name arg seeds)
               ;; This is default behavior.
               (sch2pcb_set_remove_unfound_elements TRUE)
               seeds))
     (option '(#\k "keep-unfound") #f #f
             (lambda (opt name arg seeds)
               (sch2pcb_set_remove_unfound_elements FALSE)
               seeds))
     (option '(#\q "quiet") #f #f
             (lambda (opt name arg seeds)
               (sch2pcb_set_quiet_mode TRUE)
               seeds))
     (option '(#\p "preserve") #f #f
             (lambda (opt name arg seeds)
               (sch2pcb_set_preserve TRUE)
               seeds))
     (option '(#\f "use-files") #f #f
             (lambda (opt name arg seeds)
               (sch2pcb_set_force_element_files TRUE)
               seeds))
     (option '(#\s "skip-m4") #f #f
             (lambda (opt name arg seeds)
               (sch2pcb_set_use_m4 FALSE)
               seeds))
     (option '(#\d "elements-dir") #t #f
             (lambda (opt name arg seeds)
               (let ((*elements-dir (sch2pcb_expand_dir (string->pointer arg))))
                 (when (> (sch2pcb_get_verbose_mode) 1)
                   (format #t "\tAdding directory to file element directory list: ~S\n"
                           (pointer->string *elements-dir)))
                 (sch2pcb_element_directory_list_prepend *elements-dir))
               seeds))
     (option '(#\o "output-name") #t #f
             (lambda (opt name arg seeds)
               (sch2pcb_set_sch_basename (string->pointer arg))
               seeds))
     (option '("schematics") #t #f
             (lambda (opt name arg seeds)
               (sch2pcb_add_multiple_schematics (string->pointer arg))
               seeds))
     (option '("m4-pcbdir") #t #f
             (lambda (opt name arg seeds)
               (sch2pcb_set_m4_pcbdir (string->pointer arg))
               seeds))
     (option '("m4-file") #t #f
             (lambda (opt name arg seeds)
               (sch2pcb_add_m4_file (string->pointer arg))
               seeds))
     (option '("gnetlist") #t #f
             (lambda (opt name arg seeds)
               ;; If the argument of the option is quoted, remove
               ;; the quotes.
               (let ((command (if (and (string-prefix? "\"" arg)
                                       (string-suffix? "\"" arg))
                                  (string-drop-right (string-drop arg 1) 1)
                                  arg)))
                 (sch2pcb_extra_gnetlist_list_append (string->pointer command)))
               seeds))
     (option '("empty-footprint") #t #f
             (lambda (opt name arg seeds)
               (sch2pcb_set_empty_footprint_name (string->pointer arg))
               seeds))
     (option '("backend-cmd") #t #f
             (lambda (opt name arg seeds)
               (sch2pcb_set_backend_mkfile_cmd (string->pointer arg))
               seeds))
     (option '("backend-net") #t #f
             (lambda (opt name arg seeds)
               (sch2pcb_set_backend_mkfile_net (string->pointer arg))
               seeds))
     (option '("backend-pcb") #t #f
             (lambda (opt name arg seeds)
               (sch2pcb_set_backend_mkfile_pcb (string->pointer arg))
               seeds))
     (option '(#\V "version") #f #f
             (lambda (opt name arg seeds)
               (display-lepton-version #:print-name #t #:copyright #t)
               (exit 0))))
    (lambda (opt name arg seeds)
      (format #t
              "lepton-sch2pcb: bad or incomplete arg: ~S\n"
              (if (char? name)
                  (string-append "-" (char-set->string (char-set name)))
                  (string-append "--" name)))
      (sch2pcb_usage))
    (lambda (op seeds)
      (if (string-suffix? ".sch" op)
          (begin
            (sch2pcb_add_schematic (string->pointer op))
            (cons op seeds))
          (begin
            (sch2pcb_load_extra_project_files)
            (sch2pcb_load_project (string->pointer op))
            seeds)))
    '())))


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

  (define non-zero? (negate zero?))
  (define pcb-file-created? (not (and (zero? (sch2pcb_get_n_added_ef))
                                      (zero? (sch2pcb_get_n_added_m4))
                                      (zero? (sch2pcb_get_n_not_found)))))

  ;; Report work done during processing.
  (unless (zero? (sch2pcb_get_verbose_mode))
    (format #t "\n"))

  (format #t "\n----------------------------------\n")
  (format #t "Done processing.  Work performed:\n")
  (when (or (non-zero? (sch2pcb_get_n_deleted))
            (non-zero? (sch2pcb_get_n_fixed))
            (true? (sch2pcb_get_need_PKG_purge))
            (non-zero? (sch2pcb_get_n_changed_value)))
    (format #t "~A is backed up as ~A.\n" pcb-filename bak-filename))
  (when (and (not (null-pointer? (sch2pcb_get_pcb_element_list)))
             (non-zero? (sch2pcb_get_n_deleted)))
    (format #t "~A elements deleted from ~A.\n"
            (sch2pcb_get_n_deleted)
            pcb-filename))

  (if (zero? (+ (sch2pcb_get_n_added_ef)
                (sch2pcb_get_n_added_m4)))
      (when (zero? (sch2pcb_get_n_not_found))
        (format #t "No elements to add so not creating ~A\n" pcb-new-filename))
      (format #t "~A file elements and ~A m4 elements added to ~A.\n"
              (sch2pcb_get_n_added_ef)
              (sch2pcb_get_n_added_m4)
              pcb-new-filename))

  (unless (zero? (sch2pcb_get_n_not_found))
    (format #t "~A not found elements added to ~A.\n"
            (sch2pcb_get_n_not_found)
            pcb-new-filename))
  (unless (zero? (sch2pcb_get_n_unknown))
    (format #t "~A components had no footprint attribute and are omitted.\n"
            (sch2pcb_get_n_unknown)))
  (unless (zero? (sch2pcb_get_n_none))
    (format #t "~A components with footprint \"none\" omitted from ~A.\n"
            (sch2pcb_get_n_none)
            pcb-new-filename))
  (unless (zero? (sch2pcb_get_n_empty))
    (format #t "~A components with empty footprint \"~A\" omitted from ~A.\n"
            (sch2pcb_get_n_empty)
            (sch2pcb_get_empty_footprint_name)
            pcb-new-filename))
  (unless (zero? (sch2pcb_get_n_changed_value))
    (format #t "~A elements had a value change in ~A.\n"
            (sch2pcb_get_n_changed_value)
            pcb-filename))
  (unless (zero? (sch2pcb_get_n_fixed))
    (format #t "~A elements fixed in ~A.\n"
            (sch2pcb_get_n_fixed)
            pcb-filename))
  (unless (zero? (sch2pcb_get_n_PKG_removed_old))
    (format #t "~A elements could not be found."
            (sch2pcb_get_n_PKG_removed_old))
    (if pcb-file-created?
        (format #t "  So ~A is incomplete.\n" pcb-filename)
        (format #t "\n")))
  (unless (zero? (sch2pcb_get_n_PKG_removed_new))
    (format #t "~A elements could not be found."
            (sch2pcb_get_n_PKG_removed_new))
    (if pcb-file-created?
        (format #t "  So ~A is incomplete.\n" pcb-new-filename)
        (format #t "\n")))
  (unless (zero? (sch2pcb_get_n_preserved))
    (format #t "~A elements not in the schematic preserved in ~A.\n"
            (sch2pcb_get_n_preserved)
            pcb-filename))

  ;; Tell user what to do next.
  (unless (zero? (sch2pcb_get_verbose_mode))
    (format #t "\n"))

  (unless (zero? (+ (sch2pcb_get_n_added_ef)
                    (sch2pcb_get_n_added_m4)))
    (if initial-pcb?
        (begin
          (format #t "\nNext step:\n")
          (format #t "1.  Run pcb on your file ~A.\n" pcb-filename)
          (format #t "    You will find all your footprints in a bundle ready for you to place\n")
          (format #t "    or disperse with \"Select -> Disperse all elements\" in PCB.\n\n")
          (format #t "2.  From within PCB, select \"File -> Load netlist file\" and select \n")
          (format #t "    ~A to load the netlist.\n\n" net-filename)
          (format #t "3.  From within PCB, enter\n\n")
          (format #t "           :ExecuteFile(~A)\n\n" pins-filename)
          (format #t "    to propagate the pin names of all footprints to the layout.\n\n"))
        (when (= (sch2pcb_get_quiet_mode) FALSE)
          (format #t "\nNext steps:\n")
          (format #t "1.  Run pcb on your file ~A.\n" pcb-filename)
          (format #t "2.  From within PCB, select \"File -> Load layout data to paste buffer\"\n")
          (format #t
                  "    and select ~A to load the new footprints into your existing layout.\n"
                  pcb-new-filename)
          (format #t "3.  From within PCB, select \"File -> Load netlist file\" and select \n")
          (format #t "    ~A to load the updated netlist.\n\n" net-filename)
          (format #t "4.  From within PCB, enter\n\n")
          (format #t "           :ExecuteFile(~A)\n\n" pins-filename)
          (format #t "    to update the pin names of all footprints.\n\n")))))


(let ((number-of-args (length (program-arguments))))
  (if (= 1 number-of-args)
      (sch2pcb_usage)
      (begin
        ;; Parse command line arguments and set up internal
        ;; variables.
        (parse-command-line)
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
                                initial-pcb?)

                (sch2pcb_set_backend_mkfile_cmd %null-pointer)
                (sch2pcb_set_backend_mkfile_net %null-pointer)
                (sch2pcb_set_backend_mkfile_pcb %null-pointer)

                (sch2pcb_set_default_m4_pcbdir %null-pointer)
                (sch2pcb_set_m4_pcbdir %null-pointer)))))))
