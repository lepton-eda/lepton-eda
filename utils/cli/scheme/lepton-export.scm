#!/usr/bin/env sh
export GUILE_LOAD_COMPILED_PATH="@ccachedir@:${GUILE_LOAD_COMPILED_PATH}"
exec @GUILE@ -s "$0" "$@"
!#

;;; Lepton EDA command-line utility
;;; Copyright (C) 2012 Peter Brett <peter@peter-b.co.uk>
;;; Copyright (C) 2014-2016 gEDA Contributors
;;; Copyright (C) 2017-2021 Lepton EDA Contributors
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

(eval-when (expand load eval)
  (unless (getenv "LIBLEPTON")
    (add-to-load-path "@LEPTON_SCHEME_MODULE_DIRECTORY@")))

(use-modules (srfi srfi-1)
             (srfi srfi-37)
             (system foreign)
             (lepton ffi))

;;; Initialize liblepton library.
(liblepton_init)

(primitive-eval '(use-modules (lepton core gettext)
                              (lepton core toplevel)
                              (lepton page)
                              (lepton rc)
                              (lepton version)))

;;; Each export format is a list of the form:
;;;   '(alias name multipage func)
;;; name is not used.
(define output-formats
  `((png "Portable Network Graphics (PNG)" #f ,lepton_export_png)
    (ps "Postscript (PS)" multipage ,export_ps)
    (eps "Encapsulated Postscript (EPS)" #f ,export_eps)
    (pdf "Portable Document Format (PDF)" multipage ,export_pdf)
    (svg "Scalable Vector Graphics (SVG)" #f ,export_svg)))

(define %settings-outfile #f)
(define (set-settings-outfile! filename)
  (set! %settings-outfile filename)
  (lepton_export_settings_set_outfile (string->pointer filename))
  %settings-outfile)

(define %settings-format #f)
(define (set-settings-format! fmt)
  (set! %settings-format fmt)
  (lepton_export_settings_set_format (string->pointer fmt))
  %settings-format)

(define %settings-font #f)
(define (set-settings-font! font-name)
  (set! %settings-font font-name)
  (lepton_export_settings_set_font (string->pointer font-name))
  %settings-font)

(define %settings-color #f)
(define (set-settings-color! colored?)
  (set! %settings-color colored?)
  (lepton_export_settings_set_color (if colored? 1 0))
  %settings-color)

(define (chdir/err dir)
  (catch #t
    (lambda () (chdir dir))
    (lambda (key subr message args rest)
      (simple-format (current-error-port)
                     (G_ "ERROR: Failed to change directory to ~S: ~A\n")
                     dir
                     (apply simple-format #f message args)))))

(define (file->page/err filename)
  (catch #t
    (lambda () (file->page filename))
    (lambda (key subr message args rest)
      (simple-format (current-error-port)
                     (G_ "ERROR: Failed to load ~S: ~A\n")
                     filename
                     (apply simple-format #f message args)))))

(define cmd
  (let ((name (basename (car (program-arguments))))
        (args (cdr (program-arguments))))
    (if (null? args)
        name
        (if (string= "export" (car args))
            "lepton-cli export"
            name))))

(define (export-usage)
  (format #t (G_ "Usage: ~A [OPTION ...] -o OUTPUT [--] FILE ...

Export Lepton EDA files in various image formats.

  -f, --format=TYPE                    output format (normally autodetected)
  -o, --output=OUTPUT                  output filename
  -p, --paper=NAME                     select paper size by name
  -P, --paper-names                    list paper size names and exit
  -s, --size=WIDTH;HEIGHT              specify exact paper size
  -k, --scale=FACTOR                   specify output scale factor
  -l, --layout=ORIENT                  page orientation
  -m, --margins=TOP;LEFT;BOTTOM;RIGHT  set page margins
  -a, --align=HALIGN;VALIGN            set alignment of drawing within page
  -d, --dpi=DPI                        pixels-per-inch for raster outputs
  -c, --color                          enable color output
  --no-color                           disable color output
  -F, --font=NAME                      set font family for printing text
  -h, --help                           display usage information and exit

Report bugs at ~A
Lepton EDA homepage: ~A
")
          cmd
          (lepton-version-ref 'bugs)
          (lepton-version-ref 'url))

  (exit 0))

(define (help-message)
  (format (current-error-port)
          (G_ "\nRun `lepton-cli export --help' for more information.\n"))
  (exit 1))

(define (parse-commandline)
  (define (bad-argument-message arg option-string)
    (format (current-error-port)
            (G_ "ERROR: Bad argument '%1$s' to %2$s option.\n")
            arg
            option-string))

  (define (ditch-paper-size)
    (lepton_export_settings_reset_paper_size))

  ;; Parse command-line arguments.
  (args-fold
   ;; If the utility has been run from lepton-cli, the first
   ;; argument is equal to "export".  Therefore we just drop it
   ;; and process the rest args.
   (let ((args (cdr (program-arguments))))
     (if (and (not (null? args))
              (string= (car args) "export"))
         (begin (set-program-arguments (cons (car (program-arguments))
                                             (cdr args)))
                (cdr args))
         args))
   (list
    (option '("no-color") #f #f
            (lambda (opt name arg seeds)
              (set-settings-color! #f)
              seeds))
    (option '(#\a "align") #t #f
            (lambda (opt name arg seeds)
              (unless (export_parse_align (string->pointer arg))
                (bad-argument-message arg "-a,--align")
                (help-message))
              seeds))
    (option '(#\c "color") #f #f
            (lambda (opt name arg seeds)
              (set-settings-color! #t)
              seeds))
    (option '(#\d "dpi") #t #f
            (lambda (opt name arg seeds)
              (let ((%settings-dpi (string->number arg)))
                (if (or (not %settings-dpi)
                        (<= %settings-dpi 0))
                    (begin
                      (bad-argument-message arg "-d,--dpi")
                      (help-message))
                    (lepton_export_settings_set_dpi %settings-dpi)))
              seeds))
    (option '(#\f "format") #t #f
            (lambda (opt name arg seeds)
              (set-settings-format! arg)
              seeds))
    (option '(#\F "font") #t #f
            (lambda (opt name arg seeds)
              (set-settings-font! arg)
              seeds))
    (option '(#\k "scale") #t #f
            (lambda (opt name arg seeds)
              (if (export_parse_scale (string->pointer arg))
                  ;; Since a specific scale was provided, ditch
                  ;; the paper size setting.
                  (ditch-paper-size)
                  (begin
                    (bad-argument-message arg "-k,--scale")
                    (help-message)))
              seeds))
    (option '(#\l "layout") #t #f
            (lambda (opt name arg seeds)
              (unless (export_parse_layout (string->pointer arg))
                (bad-argument-message arg "-l,--layout")
                (help-message))
              seeds))
    (option '(#\m "margins") #t #f
            (lambda (opt name arg seeds)
              (unless (export_parse_margins (string->pointer arg))
                (bad-argument-message arg "-m,--margins")
                (help-message))
              seeds))
    (option '(#\o "output") #t #f
            (lambda (opt name arg seeds)
              (set-settings-outfile! arg)
              seeds))
    (option '(#\p "paper") #t #f
            (lambda (opt name arg seeds)
              (unless (export_parse_paper (string->pointer arg))
                (bad-argument-message arg "-p,--paper")
                (help-message))
              seeds))
    (option '(#\P "paper-names") #f #f
            (lambda (opt name arg seeds)
              (lepton_export_list_paper_size_names)
              seeds))
    (option '(#\s "size") #t #f
            (lambda (opt name arg seeds)
              (if (export_parse_size (string->pointer arg))
                  ;; Since a specific size was provided, ditch the paper size setting
                  (ditch-paper-size)
                  (begin
                    (bad-argument-message arg "-s,--size")
                    (help-message)))
              seeds))

    (option '(#\h "help") #f #f
            (lambda (opt name arg seeds)
              (export-usage))))
   (lambda (opt name arg seeds)
     (format #t
             (G_ "ERROR: Unknown option ~A.\n")
             (if (char? name)
                 (string-append "-" (char-set->string (char-set name)))
                 (string-append "--" name)))
     (export-usage))
   (lambda (op seeds)
     (cons op seeds))
   '()))

(define (export-command-line)
  ;; Parse command-line arguments.
  (let ((schematics (reverse (parse-commandline))))
    ;; Check that some schematic files to print were provided
    (when (null? schematics)
       (format (current-error-port)
               (G_ "ERROR: You must specify at least one input filename.\n"))
       (help-message))

    (unless %settings-outfile
      (format (current-error-port)
              (G_ "ERROR: You must specify an output filename.\n"))
      (help-message))

    ;; Return schematic file names.
    schematics))

(define (ext s)
  (let ((index (string-rindex s #\.)))
    (and index
         (string-drop s (1+ index)))))

(define (export)
  (define original-cwd (getcwd))

  ;; Enable rendering of placeholders. Otherwise the user won't
  ;; see what's wrong.
  (set_render_placeholders)

  ;; Now load rc files, if necessary
  (unless (getenv "LEPTON_INHIBIT_RC_FILES")
    (parse-rc "lepton-cli export" "gafrc"))

  ;; Parse configuration files.
  (export_config)

  ;; Parse command-line arguments.
  (let ((schematics (export-command-line)))

    ;; If no format was specified, try and guess from output
    ;; filename.
    (unless %settings-format
      (unless (ext %settings-outfile)
        (format (current-error-port)
                (G_ "ERROR: Cannot infer output format from filename ~S.\n")
                %settings-outfile)
        (exit 1)))

    ;; Try and find an exporter function
    (let* ((output-format
            (string->symbol (string-downcase (or %settings-format
                                                 (ext %settings-outfile)))))
           (exporter (assq-ref output-formats output-format))
           (multipage? (and exporter (second exporter)))
           (export-func (and exporter (third exporter))))

      (unless exporter
        (if (not %settings-format)
            (begin
              (format (current-error-port)
                      (G_ "ERROR: Cannot find supported format for filename ~S.\n")
                      %settings-outfile)
              (exit 1))
            (begin
              (format (current-error-port)
                      (G_ "ERROR: Unsupported output format ~S.\n")
                      %settings-format)
              (help-message))))

      ;; If more than one schematic/symbol file was specified, check that
      ;; exporter supports multipage output.
      (when (and (> (length schematics) 1)
                 (not multipage?))
        (format (current-error-port)
                (G_ "ERROR: Selected output format does not support multipage output\n"))
        (exit 1))


      ;; Load schematic files
      (for-each
       (lambda (file)
         (file->page/err file)
         (chdir/err original-cwd))
       schematics)

      ;; Render
      (export-func)

      (exit 0))))

(set_guile_compiled_path)

;;; Main function for `lepton-cli export'
(%with-toplevel
 (%make-toplevel)
 export)
