;;; lepton-sch2pcb -- transform schematics to PCB
;;;
;;; Copyright (C) 2022-2023 Lepton EDA Contributors
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


(use-modules (ice-9 match)
             (ice-9 rdelim)
             (srfi srfi-1)
             (srfi srfi-26)
             (system foreign)
             (lepton ffi boolean)
             (lepton ffi glib)
             (lepton ffi sch2pcb)
             (lepton file-system)
             (lepton gettext)
             (lepton m4)
             (lepton os)
             (lepton srfi-37)
             (lepton version))

(define-syntax-rule (verbose-format arg ...)
  (when (> (sch2pcb_get_verbose_mode) 0)
    (format (current-output-port) arg ...)))

(define-syntax-rule (extra-verbose-format arg ...)
  (when (> (sch2pcb_get_verbose_mode) 1)
    (format (current-output-port) arg ...)))

(define %sch2pcb (basename (car (program-arguments))))

(define %pcb-data-path (getenv "PCBDATA"))

;;; Default directory where 'pcb' stores its m4 files.  This
;;; variable is used to inform the user about initial settings.
(define %default-m4-pcb-dir
  (let ((pcb-configure-m4-directory (and (not (string-null? %pcb-m4-dir))
                                         %pcb-m4-dir)))
    (if %pcb-data-path
        ;; If PCBDATA is set, use the value.
        (string-append %pcb-data-path file-name-separator-string "m4")

        ;; Use the default value passed in from the configure
        ;; script instead of trying to hard code a value which
        ;; is very likely wrong.
        (or pcb-configure-m4-directory
            ;; Neither PCBDATA was set nor PCBM4DIR has been
            ;; configured.  Fall back to using the "m4" subdirectory
            ;; in the current directory.
            (string-append (getcwd) file-name-separator-string "m4")))))

;;; Determines if the m4 processor can be used to create pcb
;;; elements.
(define %use-m4 #t)


;;; The m4 directory where 'pcb' stores its m4 files.  This value
;;; can be overridden by a command line option or in project
;;; files.
(define %m4-pcb-dir %default-m4-pcb-dir)


;;; Default backend names that can be overridden by command line
;;; options or settings in project files.

;;; Backend that generates .cmd file (--backend-cmd).
(define %backend-cmd "pcbpins")
;;; Backend that generates .net file (--backend-net).
(define %backend-net "PCB")
;;; Backend that generates .pcb and .pcb.new files (--backend-pcb).
(define %backend-pcb "gsch2pcb")


(define %quiet-mode #f)

;;; Allow the user to specify a full path or a different name for
;;; the netlister command.
(define %netlister (or (getenv "NETLISTER") "lepton-netlist"))

(define (create-m4-override-file)
  (define m4-override-filename "gnet-gsch2pcb-tmp.scm")

  (define file-contents
    (with-output-to-string
      (lambda ()
        (format #t "(define gsch2pcb:pcb-m4-dir ~S)\n" %m4-pcb-dir)
        (unless (null? %m4-files)
          (format #t "(define gsch2pcb:m4-files ~S)\n" (string-join %m4-files)))
        (format #t
                "(define gsch2pcb:use-m4 ~A)\n"
                (if %use-m4 "#t" "#f")))))

  (let ((result
         (and (false-if-exception
               (with-output-to-file m4-override-filename
                 (lambda () (display file-contents))))
              m4-override-filename)))

    (and result
         (unless (zero? (sch2pcb_get_verbose_mode))
           (format #t "Default m4-pcbdir: ~A\n" %default-m4-pcb-dir)
           (format #t "--------\ngnet-gsch2pcb-tmp.scm override file:\n")
           (display file-contents))
         result)))

;;; List of extra backends to run netlister command for.
(define %extra-gnetlist-list '())
;;; List of additional arguments for netlister.
(define %extra-gnetlist-arg-list '())

;;; List of schematic names to process.
(define %schematics '())
;;; Variable that will be used to form output file names.
(define %schematic-basename #f)


;;; Whether using file elements should be forced over m4 PCB
;;; elements for new footprints even though m4 elements are
;;; searched for first and may have been found.
(define %force-file-elements? #f)
;;; See description of the '--fix-elements' option.
(define %fix-elements? #f)
;;; Whether unfound pcb elements have to be removed.
(define %remove-unfound-elements? #t)

;;; The number of file elements added.
(define %added-file-element-count 0)
;;; The number of m4 elements added.
(define %added-m4-element-count 0)
;;; The number of fixed elements.
(define %fixed-element-count 0)
;;; The number of not found packages.
(define %not-found-packages-count 0)
;;; The number of new packages mentioned but not found.
(define %removed-new-packages-count 0)


(define (pcb-element-get-string *element *c-getter)
  (define *s (*c-getter *element))
  (if (null-pointer? *s)
      ;; What should the function return if *s is NULL?
      "<null>"
      (pointer->string *s)))

(define (pcb-element-refdes *element)
  (pcb-element-get-string *element pcb_element_get_refdes))

(define (pcb-element-description *element)
  (pcb-element-get-string *element pcb_element_get_description))

(define (pcb-element-value *element)
  (pcb-element-get-string *element pcb_element_get_value))


(define (make-pcb-element-list pcb-filename)
  (when (and (regular-file? pcb-filename)
             (file-readable? pcb-filename))
    (with-input-from-file pcb-filename
      (lambda ()
        (let loop ((line (read-line)))
          (unless (eof-object? line)
            (let ((s (string-trim line char-set:whitespace)))
              (if (string-prefix? "PKG_" s)
                  (sch2pcb_set_need_PKG_purge TRUE)
                  (let ((*element (pcb_element_line_parse (string->pointer s))))
                    (unless (null-pointer? *element)
                      (sch2pcb_pcb_element_list_append *element))))
              (loop (read-line)))))))))


(define %element-directory-list '())


(define* (add-element-directory path #:optional prepend)
  (when (and (file-exists? path)
             (directory? path))
    (set! %element-directory-list
          (if prepend
              (cons path %element-directory-list)
              (append %element-directory-list (list path))))))

(define (append-element-directory dir)
  (add-element-directory dir))

(define (prepend-element-directory dir)
  (add-element-directory dir 'prepend))


(define (search-element-directories element-directories *element)
  (define *package-name-fix (pcb_element_get_pkg_name_fix *element))
  (define package-name-fix (and (not (null-pointer? *package-name-fix))
                                (pointer->string *package-name-fix)))
  (define *description (pcb_element_get_description *element))
  (define description (and (not (null-pointer? *description))
                           (pointer->string *description)))

  (define (description+fix->name description fix)
    (and fix
         description
         (let ((description-length (string-length description))
               (fix-length (string-length fix)))
           (if (> description-length fix-length)
               (let ((left-description-part (string-drop-right description fix-length))
                     (right-description-part (string-take-right description fix-length)))
                 (and (string-suffix? "-" left-description-part)
                      ;; This is a very weak test that fixed
                      ;; package name part matches to the right
                      ;; description part.  Maybe using
                      ;; (string-filter (char-set-delete
                      ;; char-set:full #\space #\,) str) for each
                      ;; of the strings instead of string-ref
                      ;; could be better when comparing the
                      ;; strings?
                      (char=? (string-ref right-description-part 0)
                              (string-ref fix 0))
                      ;; Reconstruct description using space
                      ;; instead of hyphen as separator.
                      (string-append (string-drop-right left-description-part 1)
                                     " "
                                     fix)))))))

  (define name (description+fix->name description package-name-fix))

  (define *name (if name (string->pointer name) %null-pointer))

  (define (search-element-path element-name)
    (let loop ((ls element-directories))
      (and (not (null? ls))
           (let ((dir-path (car ls)))
             (extra-verbose-format (G_ "\tLooking in directory: ~S\n") dir-path)
             (let ((*path (sch2pcb_find_element (string->pointer dir-path)
                                                (if element-name
                                                    (string->pointer element-name)
                                                    %null-pointer))))
               (if (null-pointer? *path)
                   (loop (cdr ls))
                   (let ((path (pointer->string *path)))
                     (g_free *path)
                     (verbose-format (G_ "\tFound: ~A\n") path)
                     path)))))))

  ;; See comment before pcb_element_pkg_to_element().
  (when package-name-fix
    (unless name
      (format #t
              "Warning: argument passing may have been confused by
         a comma in a component value:\n
         Check ~A ~A ~A
         Maybe just use a space instead of a comma?\n"
              (pointer->string (pcb_element_get_refdes *element))
              description
              (pointer->string (pcb_element_get_value *element)))))

  (let* ((element-name (or name description)))
    (if (string= element-name "unknown")
        %null-pointer
        (begin
          (verbose-format
           (G_ "\tSearching directories looking for file element: ~A\n")
               element-name)
          (search-element-path element-name)))))


;;; Process the newly created pcb file which is the output from
;;;     lepton-netlist -g gsch2pcb ...
;;;
;;; It will have elements found via the m4 interface and PKG_
;;; lines for elements not found.  Insert pcb file elements for
;;; PKG_ lines if file elements can be found.  If there was an
;;; existing pcb file, strip out any elements if they are already
;;; present so that the new pcb file will only have new elements.
(define (add-elements pcb-filename)
  (define tmp-filename (string-append pcb-filename ".tmp"))

  (define *tmp-file (sch2pcb_open_file_to_write (string->pointer tmp-filename)))

  (define (verbose-file-element-report *element is-m4-element?)
    (if is-m4-element?
        (when %force-file-elements?
          (verbose-format "~A: have m4 element ~S, but trying to replace with a file element.\n"
                          (pcb-element-refdes *element)
                          (pcb-element-description *element)))

        (verbose-format "~A: need new file element for footprint ~S (value=~A)\n"
                        (pcb-element-refdes *element)
                        (pcb-element-description *element)
                        (pcb-element-value *element))))

  (define (verbose-report-no-file-element-found path m4-element?)
    (when (and (not path)
               m4-element?
               %force-file-elements?)
      (verbose-format "\tNo file element found.\n")))

  (define (verbose-increment-added-file-element *element)
    (set! %added-file-element-count (1+ %added-file-element-count))
    (verbose-format "~A: added new file element for footprint ~S (value=~A)\n"
                    (pcb-element-refdes *element)
                    (pcb-element-description *element)
                    (pcb-element-value *element)))

  (define (m4-element->file *element *mline *tmp-file)
    (sch2pcb_buffer_to_file *mline *tmp-file)
    (set! %added-m4-element-count (1+ %added-m4-element-count))
    (verbose-format "~A: added new m4 element for footprint ~S (value=~A)\n"
                    (pcb-element-refdes *element)
                    (pcb-element-description *element)
                    (pcb-element-value *element)))

  (define (error-report-element-not-found *element)
    (format (current-error-port)
            (G_ "~A: can't find PCB element for footprint ~S (value=~A)\n")
            (pcb-element-refdes *element)
            (pcb-element-description *element)
            (pcb-element-value *element)))

  (define (error-report-element-removed *element)
    (set! %removed-new-packages-count (1+ %removed-new-packages-count))
    (format (current-error-port)
            (G_ "So device ~S will not be in the layout.\n")
            (pcb-element-refdes *element)))

  (define (unfound-element->file *element *mline *tmp-file)
    (error-report-element-not-found *element)
    (if (and %remove-unfound-elements?
             (not %fix-elements?))
        ;; If removing unfound elements is enabled while fixing
        ;; them is disabled, we just increment the counter of new
        ;; packages that won't be in the layout.
        (error-report-element-removed *element)

        ;; Otherwise we increment the number of not found packages
        ;; that will be replaced with the PKG_ placeholder, and
        ;; insert the placeholder.
        (begin
          (set! %not-found-packages-count (1+ %not-found-packages-count))
          ;; Copy PKG_ line.
          (sch2pcb_buffer_to_file *mline *tmp-file))))

  (define (process-file-element *mline
                                *tmp-file
                                *element
                                m4-element?
                                skip-next?)
    (let ((path (search-element-directories %element-directory-list *element)))
      (verbose-file-element-report *element m4-element?)
      (verbose-report-no-file-element-found path m4-element?)

      (if (and path
               (true? (sch2pcb_insert_element *tmp-file
                                              (string->pointer path)
                                              (pcb_element_get_description *element)
                                              (pcb_element_get_refdes *element)
                                              (pcb_element_get_value *element))))
          (begin
            ;; Nice, we found it.  If it is an m4 element, we have
            ;; to skip some lines below, see comments above.
            (verbose-increment-added-file-element *element)
            m4-element?)
          (begin
            (unless m4-element?
              (unfound-element->file *element *mline *tmp-file))
            skip-next?))))

  (define (process-element *mline
                           *tmp-file
                           *element
                           m4-element?
                           skip-next)
    (let ((result
           (if (or (not m4-element?)
                   (and m4-element?
                        %force-file-elements?))
               (process-file-element *mline
                                     *tmp-file
                                     *element
                                     m4-element?
                                     skip-next)
               (begin
                 ;; Here we're surely dealing with m4 elements as
                 ;; 'm4-element?' has been set to #t and no
                 ;; forcing of file elements requested.
                 (m4-element->file *element *mline *tmp-file)
                 skip-next))))

      (pcb_element_free *element)
      (verbose-format "----\n")
      result))

  (define (parse-next-line mline tline skip-next?)
    ;; First let's find out what element type we're dealing with.
    (let* ((*tline (string->pointer tline))
           (*m4-element (pcb_element_line_parse *tline))
           (m4-element? (not (null-pointer? *m4-element)))
           (*element (if m4-element?
                         ;; If Element line is present
                         ;; (*m4-element is not NULL), it was
                         ;; inserted directly from m4 code and
                         ;; thus it is an m4 element that we use.
                         *m4-element
                         ;; Otherwise, it's a line starting with
                         ;; PKG_, probably a file element?
                         (pcb_element_pkg_to_element *tline))))
      ;; Next step is to check if element processing can be
      ;; skipped as the same element has been processed before.
      (if (and (not (null-pointer? *element))
               ;; pcb_element_exists() returns PcbElement.
               (not (null-pointer? (pcb_element_exists *element
                                                       TRUE))))
          ;; OK, element has been found in the list of previously
          ;; found elements.
          (begin
            ;; Obviously, a new copy of the found element is not
            ;; needed in the element list, so it has to be freed.
            (pcb_element_free *element)
            ;; For m4 elements, the next line starts with an
            ;; opening paren "(", and we have to skip lines until
            ;; its closing paren is found.  For file elements, we
            ;; don't want to skip any lines.  In the next
            ;; iteration, this C value ('skipping') will allow us
            ;; to skip parenthesized m4 element body.
            m4-element?)

          ;; Well, the element has not been found in the current
          ;; element list, let's process it then.
          (let ((*mline (string->pointer mline)))
            (if (null-pointer? *element)
                ;; Element does not exist.  Output not trimmed
                ;; string to the temp file as is.
                (begin
                  (sch2pcb_buffer_to_file *mline *tmp-file)
                  skip-next?)
                ;; Otherwise, process it further.
                (if (true? (pcb_element_get_omit_PKG *element))
                    ;; Element exists but its omit_PKG field is
                    ;; true.  Let's skip it.
                    skip-next?
                    ;; Actually process or lookup the element.
                    (process-element *mline
                                     *tmp-file
                                     *element
                                     m4-element?
                                     skip-next?)))))))

  (define (add-elements-from-file)
    (with-input-from-file pcb-filename
      (lambda ()
        (let loop ((line (read-line))
                   (paren-level 0)
                   (skip-next? #f))
          (unless (eof-object? line)
            ;; read-line() drops trailing newlines so we add them
            ;; again here.
            (let* ((mline (string-append line "\n"))
                   (tline (string-trim mline))
                   (first-char (and (not (string-null? tline))
                                    (string-ref tline 0)))
                   ;; When we've decided that some next lines
                   ;; should be skipped, we have to count parens
                   ;; in order to skip block within toplevel
                   ;; parens.  What if the first paren is a
                   ;; closing one?  Should we reset the counter to
                   ;; zero?
                   (new-paren-level (if skip-next?
                                        (case first-char
                                          ((#\() (1+ paren-level))
                                          ((#\)) (1- paren-level))
                                          (else paren-level))
                                        paren-level))
                   (skip (if (and skip-next?
                                  (<= new-paren-level 0))
                             ;; Stop skipping mode as we found
                             ;; closing paren matching to the
                             ;; first one that was starting in
                             ;; that mode.
                             #f
                             ;; If paren level is still greater
                             ;; than zero, continue skipping.
                             ;; Otherwise, parse lines as
                             ;; usual.  The function
                             ;; parse-next-line() below will
                             ;; decide if next lines have to
                             ;; be skipped based on the result
                             ;; of parsing.
                             (or skip-next?
                                 (parse-next-line mline
                                                  tline
                                                  skip-next?)))))
              (loop (read-line) new-paren-level skip)))))))

  (define (process-files)
    (add-elements-from-file)
    (sch2pcb_close_file *tmp-file)

    (let ((total (+ %added-file-element-count
                    %added-m4-element-count
                    %not-found-packages-count)))
      (if (zero? total)
          (system* "rm" tmp-filename)
          (system* "mv" tmp-filename pcb-filename))
      total))

  (if (null-pointer? *tmp-file)
      0
      (catch #t
        process-files
        (lambda (key subr message args rest)
          (format (current-error-port) (G_ "ERROR: ~?.") message args)
          0))))


(define (prune-elements pcb-filename bak-filename)
  (define (prune-element *tmp-file line trimmed-line skip-line?)
    (let* ((*trimmed-line (string->pointer trimmed-line))
           (*element (pcb_element_line_parse *trimmed-line)))
      (sch2pcb_prune_element *element
                             *tmp-file
                             (string->pointer (string-append line "\n"))
                             *trimmed-line
                             skip-line?)))

  (let loop ((*element-list (glist->list (sch2pcb_get_pcb_element_list)
                                         identity)))
    (unless (null? *element-list)
      (let ((*element (car *element-list)))
        (if (false? (pcb_element_get_still_exists *element))
            (if (true? (sch2pcb_get_preserve))
                (begin
                  (sch2pcb_set_n_preserved (1+ (sch2pcb_get_n_preserved)))
                  (format (current-error-port)
                          "Preserving PCB element not in the schematic:    ~A (element   ~A)\n"
                          (pointer->string (pcb_element_get_refdes *element))
                          (pointer->string (pcb_element_get_description *element))))

                (sch2pcb_set_n_deleted (1+ (sch2pcb_get_n_deleted))))

            (unless (null-pointer? (pcb_element_get_changed_value *element))
              (sch2pcb_set_n_changed_value (1+ (sch2pcb_get_n_changed_value)))))
        (loop (cdr *element-list)))))

  (unless (or (null-pointer? (sch2pcb_get_pcb_element_list))
              (and (zero? (sch2pcb_get_n_deleted))
                   (false? (sch2pcb_get_need_PKG_purge))
                   (zero? (sch2pcb_get_n_changed_value))))
    (when (file-readable? pcb-filename)
      (let* ((tmp-filename (string-append pcb-filename ".tmp"))
             (*tmp-file (sch2pcb_open_file_to_write
                         (string->pointer tmp-filename))))
        (unless (null-pointer? *tmp-file)
          (begin
            (with-input-from-file pcb-filename
              (lambda ()
                (let loop ((line (read-line))
                           (skip-line? FALSE)
                           (paren-level 0))
                  (unless (eof-object? line)
                    (let* ((trimmed-line (string-trim-both line char-set:whitespace))
                           (first-char (and (not (string-null? trimmed-line))
                                            (string-ref trimmed-line 0)))
                           (new-paren-level (if (true? skip-line?)
                                                (case first-char
                                                  ((#\() (1+ paren-level))
                                                  ((#\)) (1- paren-level))
                                                  (else paren-level))
                                                paren-level))
                           (skip-next?
                            (if (true? skip-line?)
                                (if (and (< new-paren-level paren-level)
                                         (<= new-paren-level 0))
                                    FALSE
                                    TRUE)
                                (prune-element *tmp-file
                                               line
                                               trimmed-line
                                               skip-line?))))
                      (loop (read-line) skip-next? new-paren-level))))))
            (sch2pcb_close_file *tmp-file)

            (when (false? (sch2pcb_get_bak_done))
              (system* "mv" pcb-filename bak-filename)
              (sch2pcb_set_bak_done TRUE))

            (system* "mv" tmp-filename pcb-filename)))))))


(define (update-element-descriptions pcb-filename bak-filename)
  (define (update-element-description *output-file *line *trimmed-line)
    (let* ((*element (pcb_element_line_parse *trimmed-line))
           (*existing-element (if (null-pointer? *element)
                                  %null-pointer
                                  (pcb_element_exists *element FALSE))))
      (if (and (not (null-pointer? *element))
               (not (null-pointer? *existing-element))
               (not (null-pointer? (pcb_element_get_changed_description *existing-element))))
          ;; If element already exists in the element list, and it
          ;; has changed description, output its line with this
          ;; description applied.
          (let* ((res-char (list->string (list (integer->char (pcb_element_get_res_char *element)))))
                 (flags (pointer->string (pcb_element_get_flags *element)))
                 (changed-description
                  (pointer->string (pcb_element_get_changed_description *existing-element)))
                 (refdes (pointer->string (pcb_element_get_refdes *element)))
                 (value (pointer->string (pcb_element_get_value *element)))
                 (x (pointer->string (pcb_element_get_x *element)))
                 (y (pointer->string (pcb_element_get_y *element)))
                 (tail (pointer->string (pcb_element_get_tail *element)))
                 (*line (string->pointer (format #f
                                                 (if (true? (pcb_element_get_quoted_flags *element))
                                                     "Element~A~S ~S ~S ~S ~A ~A~A"
                                                     "Element~A~A ~S ~S ~S ~A ~A~A")
                                                 res-char
                                                 flags
                                                 changed-description
                                                 refdes
                                                 value
                                                 x
                                                 y
                                                 tail))))
            (sch2pcb_buffer_to_file *line *output-file)

            (format #t "~A: updating element Description: ~A -> ~A\n"
                    (pointer->string (pcb_element_get_refdes *element))
                    (pointer->string (pcb_element_get_description *element))
                    (pointer->string (pcb_element_get_changed_description *existing-element)))
            (pcb_element_set_still_exists *existing-element TRUE))
          ;; Otherwise, if the element is new, just output it into
          ;; the output file.
          (begin
            (sch2pcb_buffer_to_file *line *output-file)
            (sch2pcb_buffer_to_file (string->pointer "\n") *output-file)))

      (pcb_element_free *element)))

  (let loop ((*element-list (glist->list (sch2pcb_get_pcb_element_list)
                                         identity)))
    (unless (null? *element-list)
      (let ((*element (car *element-list)))
        (unless (null-pointer? (pcb_element_get_changed_description *element))
          (set! %fixed-element-count (1+ %fixed-element-count)))
        (loop (cdr *element-list)))))

  (if (or (null-pointer? (sch2pcb_get_pcb_element_list))
          (zero? %fixed-element-count))
      (format (current-error-port) "Could not find any elements to fix.\n")
      (let* ((tmp-filename (string-append pcb-filename ".tmp"))
             (*tmp-file (sch2pcb_open_file_to_write (string->pointer tmp-filename))))
        (when (file-readable? pcb-filename)
          (if (null-pointer? *tmp-file)
              (sch2pcb_close_file *tmp-file)
              (begin
                (with-input-from-file pcb-filename
                  (lambda ()
                    (let loop ((line (read-line)))
                      (unless (eof-object? line)
                        (let ((trimmed-line (string-trim-both line char-set:whitespace)))
                          (update-element-description *tmp-file
                                                      (string->pointer line)
                                                      (string->pointer trimmed-line))
                          (loop (read-line)))))))
                (sch2pcb_close_file *tmp-file)

                ;; Make backup for pcb file.
                (when (false? (sch2pcb_get_bak_done))
                  (system* "mv" pcb-filename bak-filename)
                  (sch2pcb_set_bak_done TRUE))

                ;; Replace pcb file with newly created file with
                ;; updated descriptions.
                (system* "mv" tmp-filename pcb-filename)))))))


;;; Run lepton-netlist to generate a netlist and a PCB board file.
;;; lepton-netlist has exit status of 0 even if it's given an
;;; invalid arg, so do some stat() hoops to decide if
;;; lepton-netlist successfully generated the PCB board file.
(define (run-netlister schematic-basename pins-filename net-filename pcb-filename)
  (define (custom-system* ls)
    (verbose-format "Running command:\n\t~A\n" (string-join ls " "))
    (let ((result (eq? EXIT_SUCCESS (status:exit-val (apply system* ls)))))
      (unless result
        (format (current-error-port) "Failed to execute external program.\n"))
      (verbose-format "\n--------\n")
      ;; return
      result))

  (define verbose-list
    (if (zero? (sch2pcb_get_verbose_mode)) '("-q") '()))

  (and (custom-system* (append (list %netlister)
                               verbose-list
                               (list "-g" %backend-cmd "-o" pins-filename)
                               %extra-gnetlist-arg-list
                               %schematics))

       (custom-system* (append (list %netlister)
                               verbose-list
                               (list "-g" %backend-net "-o" net-filename)
                               %extra-gnetlist-arg-list
                               %schematics))

       (let* ((m4-override-filename (create-m4-override-file))
              (mtime (if (file-exists? pcb-filename)
                         (stat:mtime (stat pcb-filename))
                         0))
              (optional-args (if m4-override-filename
                                 (list "-m" m4-override-filename)
                                 '()))
              (success (custom-system* (append (list %netlister)
                                               verbose-list
                                               (list "-g" %backend-pcb "-o" pcb-filename)
                                               optional-args
                                               %extra-gnetlist-arg-list
                                               %schematics))))
         (and (or success
                  ;; If the netlister command failed, report this
                  ;; and stop processing.
                  (begin
                    (if (or
                         ;; Either file does not exist,
                         (not (file-exists? pcb-filename))
                         ;; or it has not changed.
                         (= mtime (stat:mtime (stat pcb-filename))))
                        (format (current-error-port)
                                "lepton-sch2pcb: netlister command failed, `~A' not updated.\n"
                                pcb-filename)

                        ;; Report the issue anyways, even if the
                        ;; output file has been created.
                        (format (current-error-port) "lepton-sch2pcb: netlister command failed.\n"))
                    ;; Stop processing.
                    #f))
              ;; Delete no longer necessary m4 override file.
              (when m4-override-filename
                (delete-file m4-override-filename))

              (let loop ((ls %extra-gnetlist-list))
                ;; If the list is empty, we are done, return #t.
                (or (null? ls)
                    (let* ((s (car ls))
                           ;; This code emulates its C prototype
                           ;; code.  Splitting args would be
                           ;; better done with string-split().
                           (pos (string-contains s " -o "))
                           (output-filename (if pos
                                                (string-drop s (+ pos 4))
                                                (string-append schematic-basename "." s)))
                           (backend-filename (if pos
                                                 (string-take s pos)
                                                 s)))

                      (and (custom-system* (append (list %netlister)
                                                   verbose-list
                                                   (list "-g" backend-filename "-o" output-filename)
                                                   %extra-gnetlist-arg-list
                                                   %schematics))
                           (loop (cdr ls))))))))))


(define %m4-files '())

(define (add-m4-file filename)
  (when (and (regular-file? filename)
             (file-readable? filename))
    (set! %m4-files (append %m4-files (list filename)))))

(define (add-default-m4-files)
  (define (build-filename . args)
    (string-join args file-name-separator-string))

  ;; Add "pcb.inc" residing in "~/.pcb/".
  (add-m4-file (expand-env-variables (build-filename "~" ".pcb" "pcb.inc")))
  ;; Add "pcb.inc" in the current directory.
  (add-m4-file "pcb.inc"))


(define (add-schematic schematic-name)
  (define (basename-ci name)
    ;; basename() is not case-insensitive, so just drop last 4
    ;; chars (.sch).
    (string-drop-right name 4))

  (set! %schematics (append %schematics (list schematic-name)))

  (if (and (regular-file? schematic-name)
           (file-readable? schematic-name))
      (when (and (not %schematic-basename)
                 (string-suffix-ci? ".sch" schematic-name))
        (set! %schematic-basename (basename-ci schematic-name)))
      (format (current-error-port)
              "Could not add schematic: ~A\nFile is not regular or not readable.\n"
              schematic-name)))


(define (add-multiple-schematics *str)
  (for-each add-schematic
            (glist->list (sch2pcb_parse_schematics *str)
                         pointer->string
                         ;; The list must be freed.
                         'free)))


(define (string->pair str)
  (define s (string-trim-both str char-set:whitespace))
  (define break-pos (string-index s char-set:whitespace))
  (if break-pos
      (cons (string-take s break-pos)
            (string-trim (string-drop s break-pos)
                         char-set:whitespace))
      (cons s #f)))


(define (parse-config key value)
  (verbose-format "    ~A ~A\n" key value)
  (let ((*value (if value
                    (string->pointer value)
                    (string->pointer ""))))
    (match key
      ;; This is default behaviour.
      ("remove-unfound" (set! %remove-unfound-elements? #t))
      ("keep-unfound" (set! %remove-unfound-elements? #f))
      ("quiet" (set! %quiet-mode #t))
      ("preserve" (sch2pcb_set_preserve TRUE))
      ("use-files" (set! %force-file-elements? #t))
      ("skip-m4" (set! %use-m4 #f))
      ("elements-dir"
       (let ((elements-dir (expand-env-variables value)))
         (when (> (sch2pcb_get_verbose_mode) 1)
           (format #t
                   "\tAdding directory to file element directory list: ~A\n"
                   elements-dir))
         (prepend-element-directory elements-dir)))
      ("output-name" (set! %schematic-basename value))
      ("schematics" (add-multiple-schematics *value))
      ("m4-pcbdir" (set! %m4-pcb-dir value))
      ("m4-file"
       ;; For backward compatibility with previous C code, here we
       ;; have to pre-process the value, that is, to split files
       ;; by whitespaces.
       (for-each add-m4-file
                 (filter (negate string-null?)
                         (string-split value char-set:whitespace))))
      ("gnetlist" (set! %extra-gnetlist-list
                        (append %extra-gnetlist-list (list value))))
      ("empty-footprint" (sch2pcb_set_empty_footprint_name *value))
      ("backend-cmd" (set! %backend-cmd value))
      ("backend-net" (set! %backend-net value))
      ("backend-pcb" (set! %backend-pcb value))
      (_ (format (current-error-port)
                 (G_ "Unknown config key: ~S\n")
                 (pointer->string *value))))))


(define (load-project-file path)
  (define (parse-line line)
    (let* ((args (string->pair line))
           (key (car args))
           (value (cdr args)))
      (unless (parse-config key value)
        (format (current-error-port)
                (G_ "Wrong line in ~S: ~S\n")
                path
                line))))

  (define (skip-line? line)
    (or (string-null? line)
        (char-set-contains? (char-set #\# #\/ #\;)
                            (string-ref line 0))))
  (define (read-file)
    (let loop ((line (read-line)))
      (unless (eof-object? line)
        (let ((trimmed-line (string-trim-both line char-set:whitespace)))
          ;; Skip empty lines or lines consisting only of
          ;; whitespaces, and comments started with #, ;, or /.
          (unless (skip-line? trimmed-line)
            (parse-line trimmed-line))
          (loop (read-line))))))

  (if (file-readable? path)
      (begin (verbose-format (G_ "Reading project file: ~A\n") path)
             (with-input-from-file path read-file))
      (when (> (sch2pcb_get_verbose_mode) 0)
        (format (current-error-port)
                (G_ "Skip missing or unreadable file: ~A\n")
                path))))


(define (load-extra-project-files)
  (define (build-filename dir filename)
    (string-append dir file-name-separator-string filename))

  ;; TODO: rename project files ("gsch2pcb")

  (for-each load-project-file
            (map (cut build-filename <> "gsch2pcb")
                 (append (sys-config-dirs) (list (user-config-dir))))))


(define (usage)
  (format #t
          (G_
"Usage: ~A [options] {project | foo.sch [foo1.sch ...]}

Generate a PCB layout file from a set of Lepton EDA schematics.

   1) `lepton-netlist -g PCB` is run to generate foo.net from the schematics.

   2) `lepton-netlist -g gsch2pcb` is run to get PCB m4 derived elements which
   match schematic footprints.  For schematic footprints which don't match
   any PCB m4 layout elements, search a set of file element directories in
   an attempt to find matching PCB file elements.
   Output to foo.pcb if it doesn't exist.  If there is a current foo.pcb,
   output only new elements to foo.new.pcb.
   If any elements with a non-empty element name in the current foo.pcb
   have no matching schematic component, then remove those elements from
   foo.pcb and rename foo.pcb to a foo.pcb.bak sequence.

   3) `lepton-netlist -g pcbpins` is run to get a PCB actions file which will rename all
   of the pins in a .pcb file to match pin names from the schematic.

   \\project\\ is a file (not ending in .sch) containing a list of
   schematics to process and some options.  A schematics line is like:
       schematics foo1.sch foo2.sch ...
   Options in a project file are like command line args without the \\-\\:
       output-name myproject

Options (may be included in a project file):
   -d D, --elements-dir=D  Search D for PCB file elements.  These defaults
                           are searched if they exist: ./packages,
                           /usr/local/share/pcb/newlib, /usr/share/pcb/newlib,
                           (old pcb) /usr/local/lib/pcb_lib, /usr/lib/pcb_lib,
                           (old pcb) /usr/local/pcb_lib
   -o N, --output-name=N   Use output file names N.net, N.pcb, and N.new.pcb
                           instead of foo.net, ... where foo is the basename
                           of the first command line .sch file.
   -f, --use-files         Force using file elements over m4 PCB elements
                           for new footprints even though m4 elements are
                           searched for first and may have been found.
   -r, --remove-unfound    Don't include references to unfound elements in
                           the generated .pcb files.  Use if you want PCB to
                           be able to load the (incomplete) .pcb file.
                           This is the default behavior.
   -k, --keep-unfound      Keep include references to unfound elements in
                           the generated .pcb files.  Use if you want to hand
                           edit or otherwise preprocess the generated .pcb file
                           before running pcb.
   -p, --preserve          Preserve elements in PCB files which are not found
                           in the schematics.  Note that elements with an empty
                           element name (schematic refdes) are never deleted,
                           so you really shouldn't need this option.
   -q, --quiet             Don't tell the user what to do next after running lepton-sch2pcb.

   -s, --skip-m4           Skip m4 when looking for footprints.  The default is to use
                           m4 (which is what previous versions did).
       --m4-file=F.inc     Use m4 file F.inc in addition to the default m4
                           files ./pcb.inc and ~/.pcb/pcb.inc.
       --m4-pcbdir=D       Use D as the PCB m4 files install directory
                           instead of the default:
                           ~A

   --backend-cmd=backend   Backend that generates pins file (.cmd)
   --backend-net=backend   Backend that generates netlist file (.net)
   --backend-pcb=backend   Backend that generates board files (.pcb, .pcb.new)

   --gnetlist=backend      A convenience run of extra lepton-netlist -g commands.
                           Example:  lepton-netlist partslist3
                           Creates:  myproject.partslist3
   --empty-footprint=name  See the project.sample file.

Options (not recognized in a project file):
   --gnetlist-arg=arg      Allows additional arguments to be passed to lepton-netlist.
   --fix-elements          If a schematic component footprint is not equal
                           to its PCB element Description, update the
                           Description instead of replacing the element.
                           Do this the first time lepton-sch2pcb is used with
                           PCB files originally created with gschem2pcb.
   -v, --verbose           Use -v -v for additional file element debugging.
   -h, --help              Print a help message.
   -V, --version
Environment variables:
   NETLISTER               If set, this specifies the name of the netlister program
                           to execute.

Additional Resources:
  gnetlist user guide:     http://wiki.geda-project.org/geda:gnetlist_ug
  gEDA homepage:           http://www.geda-project.org
  PCB homepage:            http://pcb.geda-project.org

Report bugs at <~A>
Lepton EDA homepage: <~A>
")
          %sch2pcb
          %default-m4-pcb-dir
          (lepton-version-ref 'bugs)
          (lepton-version-ref 'url))
  (exit 0))


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
               (set! %fix-elements? #t)
               seeds))
     (option '("gnetlist-arg") #t #f
             (lambda (opt name arg seeds)
               (set! %extra-gnetlist-arg-list
                     (append %extra-gnetlist-arg-list (list arg)))
               seeds))
     (option '(#\h #\? "help") #f #f
             (lambda (opt name arg seeds)
               (usage)))
     (option '(#\r "remove-unfound") #f #f
             (lambda (opt name arg seeds)
               ;; This is default behavior.
               (set! %remove-unfound-elements? #t)
               seeds))
     (option '(#\k "keep-unfound") #f #f
             (lambda (opt name arg seeds)
               (set! %remove-unfound-elements? #f)
               seeds))
     (option '(#\q "quiet") #f #f
             (lambda (opt name arg seeds)
               (set! %quiet-mode #t)
               seeds))
     (option '(#\p "preserve") #f #f
             (lambda (opt name arg seeds)
               (sch2pcb_set_preserve TRUE)
               seeds))
     (option '(#\f "use-files") #f #f
             (lambda (opt name arg seeds)
               (set! %force-file-elements? #t)
               seeds))
     (option '(#\s "skip-m4") #f #f
             (lambda (opt name arg seeds)
               (set! %use-m4 #f)
               seeds))
     (option '(#\d "elements-dir") #t #f
             (lambda (opt name arg seeds)
               (let ((elements-dir (expand-env-variables arg)))
                 (when (> (sch2pcb_get_verbose_mode) 1)
                   (format #t
                           "\tAdding directory to file element directory list: ~S\n"
                           elements-dir))
                 (prepend-element-directory elements-dir))
               seeds))
     (option '(#\o "output-name") #t #f
             (lambda (opt name arg seeds)
               (set! %schematic-basename arg)
               seeds))
     (option '("m4-pcbdir") #t #f
             (lambda (opt name arg seeds)
               (set! %m4-pcb-dir arg)
               seeds))
     (option '("m4-file") #t #f
             (lambda (opt name arg seeds)
               (add-m4-file arg)
               seeds))
     (option '("gnetlist") #t #f
             (lambda (opt name arg seeds)
               ;; If the argument of the option is quoted, remove
               ;; the quotes.
               (let ((command (if (and (string-prefix? "\"" arg)
                                       (string-suffix? "\"" arg))
                                  (string-drop-right (string-drop arg 1) 1)
                                  arg)))
                 (set! %extra-gnetlist-list
                       (append %extra-gnetlist-list (list command))))
               seeds))
     (option '("empty-footprint") #t #f
             (lambda (opt name arg seeds)
               (sch2pcb_set_empty_footprint_name (string->pointer arg))
               seeds))
     (option '("backend-cmd") #t #f
             (lambda (opt name arg seeds)
               (set! %backend-cmd arg)
               seeds))
     (option '("backend-net") #t #f
             (lambda (opt name arg seeds)
               (set! %backend-net arg)
               seeds))
     (option '("backend-pcb") #t #f
             (lambda (opt name arg seeds)
               (set! %backend-pcb arg)
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
      (usage))
    (lambda (op seeds)
      (if (string-suffix? ".sch" op)
          (begin
            (add-schematic op)
            (cons op seeds))
          (begin
            (load-project-file op)
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
  (define pcb-file-created? (not (and (zero? %added-file-element-count)
                                      (zero? %added-m4-element-count)
                                      (zero? %not-found-packages-count))))

  ;; Report work done during processing.
  (unless (zero? (sch2pcb_get_verbose_mode))
    (format #t "\n"))

  (format #t "\n----------------------------------\n")
  (format #t "Done processing.  Work performed:\n")
  (when (or (non-zero? (sch2pcb_get_n_deleted))
            (non-zero? %fixed-element-count)
            (true? (sch2pcb_get_need_PKG_purge))
            (non-zero? (sch2pcb_get_n_changed_value)))
    (format #t "~A is backed up as ~A.\n" pcb-filename bak-filename))
  (when (and (not (null-pointer? (sch2pcb_get_pcb_element_list)))
             (non-zero? (sch2pcb_get_n_deleted)))
    (format #t "~A elements deleted from ~A.\n"
            (sch2pcb_get_n_deleted)
            pcb-filename))

  (if (zero? (+ %added-file-element-count
                %added-m4-element-count))
      (when (zero? %not-found-packages-count)
        (format #t "No elements to add so not creating ~A\n" pcb-new-filename))
      (format #t "~A file elements and ~A m4 elements added to ~A.\n"
              %added-file-element-count
              %added-m4-element-count
              pcb-new-filename))

  (unless (zero? %not-found-packages-count)
    (format #t "~A not found elements added to ~A.\n"
            %not-found-packages-count
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
  (unless (zero? %fixed-element-count)
    (format #t "~A elements fixed in ~A.\n"
            %fixed-element-count
            pcb-filename))
  (unless (zero? (sch2pcb_get_n_PKG_removed_old))
    (format #t "~A elements could not be found."
            (sch2pcb_get_n_PKG_removed_old))
    (if pcb-file-created?
        (format #t "  So ~A is incomplete.\n" pcb-filename)
        (format #t "\n")))
  (unless (zero? %removed-new-packages-count)
    (format #t "~A elements could not be found."
            %removed-new-packages-count)
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

  (unless (zero? (+ %added-file-element-count
                    %added-m4-element-count))
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
        (unless %quiet-mode
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


;;; Load system and user config files once.
(load-extra-project-files)

(let ((number-of-args (length (program-arguments))))
  (if (= 1 number-of-args)
      (usage)
      (begin
        ;; Parse command line arguments and set up internal
        ;; variables.
        (parse-command-line)
        (add-default-m4-files)
        (if (null? %schematics)
            (usage)
            (begin
              ;; Defaults for the newlib element directory search path
              ;; if not configured in the project file.
              (when (not (zero? (sch2pcb_get_verbose_mode)))
                (format #t "Processing PCBLIBPATH=~S\n" %pcb-lib-path))
              (for-each
               (lambda (x) (append-element-directory x))
               (filter-map
                (lambda (x) (false-if-exception (canonicalize-path x)))
                (cons "packages" (parse-path %pcb-lib-path))))
              (let* ((pins-filename (string-append %schematic-basename ".cmd"))
                     (net-filename (string-append %schematic-basename ".net"))
                     (pcb-filename (string-append %schematic-basename ".pcb"))
                     (bak-filename (next-backup-name (string-append %schematic-basename
                                                                    ".pcb.bak")))
                     (pcb-file-exists? (file-exists? pcb-filename))
                     (initial-pcb? (not pcb-file-exists?))
                     (pcb-new-filename (if pcb-file-exists?
                                           (string-append %schematic-basename ".new.pcb")
                                           pcb-filename)))
                (when pcb-file-exists?
                  (make-pcb-element-list pcb-filename))
                (unless (run-netlister %schematic-basename
                                       pins-filename
                                       net-filename
                                       pcb-new-filename)
                  (format (current-error-port) (G_ "Failed to run netlister\n"))
                  (exit 1))
                (when (zero? (add-elements pcb-new-filename))
                  (delete-file* pcb-new-filename)
                  (when initial-pcb?
                    (format #t "No elements found, so nothing to do.\n")
                    (exit 0)))
                (when %fix-elements?
                  (update-element-descriptions pcb-filename bak-filename))
                (prune-elements pcb-filename bak-filename)
                (report-results pcb-filename
                                pcb-new-filename
                                bak-filename
                                pins-filename
                                net-filename
                                initial-pcb?)))))))
