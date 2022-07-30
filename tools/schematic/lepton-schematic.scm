;;; Lepton EDA attribute editor
;;; Copyright (C) 1998-2016 gEDA Contributors
;;; Copyright (C) 2017-2022 Lepton EDA Contributors
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
             (srfi srfi-1)
             (system foreign)
             (lepton color-map)
             (lepton config)
             (lepton eval)
             (lepton ffi)
             (lepton file-system)
             (lepton log)
             (lepton m4)
             (lepton os)
             (lepton object foreign)
             (lepton page foreign)
             (lepton srfi-37)
             (lepton version)
             (schematic gettext)
             (schematic ffi)
             (schematic ffi gtk)
             (schematic gui keymap)
             (schematic window))

;;; Initialize liblepton library.
(liblepton_init)
(unless (getenv "LEPTON_INHIBIT_RC_FILES")
  (register-data-dirs))


;;; Localization.
(bindtextdomain %schematic-gettext-domain %lepton-localedir)
(textdomain %schematic-gettext-domain)
(bind-textdomain-codeset %schematic-gettext-domain "UTF-8")
(setlocale LC_ALL "")
(setlocale LC_NUMERIC "C")


;;; Precompilation.
(define (precompile-mode)
  (getenv "LEPTON_SCM_PRECOMPILE"))

(define (precompile-prepare)
  (setenv "GUILE_AUTO_COMPILE" "0"))

;;; Add Lepton compiled path to Guile compiled paths env var.
(define (set-guile-compiled-path)
  (set! %load-compiled-path (cons "@LEPTON_CCACHE_DIR@"
                                  %load-compiled-path)))
(define (register-guile-funcs)
  (g_init_window (scm->pointer %lepton-window)))

(define (precompile-run)
  (let ((script (getenv "LEPTON_SCM_PRECOMPILE_SCRIPT")))
    (if script
        (begin (register-guile-funcs)
               ;; Actually load the script.
               (primitive-load script)
               0)
        1)))


(define add-post-load-expr! #f)
(define eval-post-load-expr! #f)

;;; Contains a Scheme expression arising from command-line
;;; arguments.  This is evaluated after loading lepton-schematic
;;; and any schematic files specified on the command-line.
(let ((post-load-expr '()))
  (set! add-post-load-expr!
        (lambda (expr script?)
          (set! post-load-expr
                (cons (list (if script? 'load 'eval-string) expr)
                      post-load-expr))))
  (set! eval-post-load-expr!
        (lambda ()
          (eval-protected
           (cons 'begin (reverse post-load-expr))))))


;;; Print brief help message describing lepton-schematic usage and
;;; command-line options, and exit with exit status 0.
(define (usage)
  (format #t
          (G_ "Usage: ~A [OPTION ...] [--] [FILE ...]

Interactively edit Lepton EDA schematics or symbols.
If one or more FILEs are specified, open them for
editing; otherwise, create a new, empty schematic.

Options:
  -q, --quiet              Quiet mode.
  -v, --verbose            Verbose mode.
  -L DIR                   Add DIR to Scheme search path.
  -c EXPR, --command=EXPR  Scheme expression to run at startup.
  -s FILE                  Scheme script to run at startup.
  -V, --version            Show version information.
  -h, --help               Help; this message.
  --                       Treat all remaining arguments as filenames.

Report bugs at ~S
Lepton EDA homepage: ~S\n")
          (basename (car (program-arguments)))
          (lepton-version-ref 'bugs)
          (lepton-version-ref 'url))
  (exit 0))


;;; Parse lepton-schematic command-line options, displaying usage
;;; message or version information as required.
(define (parse-commandline)
  "Parse command line options.  Return the list of non-option
arguments which should represent the list of schematics to open."
  (reverse
   (args-fold
    (cdr (program-arguments))
    (list
     (option '(#\q "quiet") #f #f
             (lambda (opt name arg seeds)
               (set_quiet_mode)
               seeds))
     (option '(#\v "verbose") #f #f
             (lambda (opt name arg seeds)
               (set_verbose_mode)
               seeds))
     (option '(#\L) #t #f
             (lambda (opt name arg seeds)
               (add-to-load-path arg)
               seeds))
     (option '(#\s) #t #f
             (lambda (opt name arg seeds)
               (add-post-load-expr! arg #t)
               seeds))
     (option '(#\c "command") #t #f
             (lambda (opt name arg seeds)
               (add-post-load-expr! arg #f)
               seeds))
     (option '(#\h #\? "help") #f #f
             (lambda (opt name arg seeds)
               (usage)))
     (option '(#\V "version") #f #f
             (lambda (opt name arg seeds)
               (display-lepton-version #:print-name #t #:copyright #t)
               (exit 0))))
    (lambda (opt name arg seeds)
      (format #t
              (G_ "ERROR: Unknown option ~A.
Run `~A --help' for more information.\n")
              (if (char? name)
                  (string-append "-" (char-set->string (char-set name)))
                  (string-append "--" name))
              (basename (car (program-arguments))))
      (exit 1))
    (lambda (op seeds) (cons op seeds))
    '())))


;;; Load GTK2 resource files.
;;; Search system and user configuration directories for
;;; lepton-gtkrc files and load them in sequence.
(define (parse-gtkrc)
  ;; Note: the function gtk_rc_parse() is deprecated in GTK3 so
  ;; the code below is disabled in the GTK3 port.
  (unless %m4-use-gtk3
    (let loop ((dirs (append (sys-config-dirs)
                             (list (user-config-dir)))))
      (or (null? dirs)
          (let ((filename (string-append (car dirs)
                                         file-name-separator-string
                                         "lepton-gtkrc")))
            (when (file-readable? filename)
              (gtk_rc_parse (string->pointer filename)))
            (loop (cdr dirs)))))))




;;; Setup default icon for GTK windows
;;; Sets the default window icon by name, to be found in the
;;; current icon theme.
(define (set-window-default-icon)
  (define %theme-icon-name "lepton-schematic")

  (gtk_window_set_default_icon_name (string->pointer %theme-icon-name)))

;;; Setup icon search paths.
;;; Add the icons installed by the program to the search path for
;;; the default icon theme, so that they can be automatically
;;; found by GTK.
(define (init-window-icons)
  ;; FIXME: this shouldn't be necessary, Lepton should just
  ;; install its icons in the system hicolor icon theme and
  ;; they'll be picked up automatically.
  (let loop ((sys-dirs (sys-data-dirs)))
    (or (null? sys-dirs)
        (let ((icon-dir (string-append (car sys-dirs)
                                       file-name-separator-string
                                       "icons")))
          (gtk_icon_theme_append_search_path (gtk_icon_theme_get_default)
                                             icon-dir)
          (loop (cdr sys-dirs))))))


(define (open-log-window window)
  (let ((cfg (path-config-context (getcwd))))
    (when (string= (config-string cfg "schematic" "log-window")
                   "startup")
      (x_widgets_show_log window))))


(define (get-absolute-filenames filename-list cwd)
  (define (get-absolute-filename filename)
    (if (absolute-file-name? filename)
        ;; Path is already absolute so no need to do any concat of
        ;; cwd.
        filename
        ;; Get absolute path.  At this point the filename might be
        ;; unnormalized, like /path/to/foo/../bar/baz.sch.  Bad
        ;; filenames will be normalized in f_open (called by
        ;; x_window_open_page). This works for Linux and MINGW32.
        (string-append cwd
                       file-name-separator-string
                       filename)))

  (map get-absolute-filename filename-list))

(define (main app file-list)
  ;; Create a new window and associated LeptonToplevel object.
  (define window (make-schematic-window app
                                        (lepton_toplevel_new)))
  ;; Current directory.
  (define cwd (getcwd))

  (define (open-page *filename)
    (x_window_open_page window *filename))

  (define (string-ls->pointer-ls ls)
    (map string->pointer ls))

  ;; Open up log window on startup if requested in config.
  (open-log-window window)

  (let* ((filenames (get-absolute-filenames file-list cwd))
         (*filenames (if (null? filenames)
                         (list %null-pointer)
                         (string-ls->pointer-ls filenames)))
         (*pages (map open-page *filenames))
         (*current-page (last *pages)))

    ;; Update the window to show the current page:
    (x_window_set_current_page window *current-page))

  ;; Return the new window.
  window)


;;; Init logging.
(init-log "schematic")
(display-lepton-version #:print-name #t #:log #t)

;;; Precompilation.
;;; If precompilation is requested, run it and exit.
(when (precompile-mode)
  (precompile-prepare)
  (exit (precompile-run)))

;;; Set up paths for Lepton's compiled Scheme modules.
(set-guile-compiled-path)

;;; Initialize GTK.
(gtk_init %null-pointer %null-pointer)

;;; Init global buffers.
(o_buffer_init)

;;; Register guile (scheme) functions
(register-guile-funcs)

;;; Initialise color map (need to do this before reading rc
;;; files).
(x_color_init)
(o_undo_init)

;;; Parse custom GTK resource files.  Used only for GTK2.
(parse-gtkrc)

;;; Set default icon theme and make sure we can find our own
;;; icons.
(set-window-default-icon)
(init-window-icons)

;;; Enable rendering of placeholders.
(set_render_placeholders)

;;; Init libstroke.
(x_stroke_init)

(define (activate app user-data)
  (let* ((schematics (parse-commandline))
         ;; Foreign pointer to w_current.
         (window (main app schematics)))

    ;; Evaluate post load expression in the dynamic context of the
    ;; new window.
    (with-window window (eval-post-load-expr!))))

;;; Run main GTK loop.
(if %m4-use-gtk3
    (lepton_schematic_run (procedure->pointer void activate '(* *)))
    (begin
      (activate %null-pointer %null-pointer)
      (lepton_schematic_run %null-pointer)))
