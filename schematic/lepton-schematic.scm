#!/usr/bin/env sh
exec @GUILE@ -s "$0" "$@"
!#

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

(eval-when (expand load eval)
  (unless (getenv "LIBLEPTON")
    (add-to-load-path "@LEPTON_SCHEME_MODULE_DIRECTORY@")
    (set! %load-compiled-path (cons "@ccachedir@" %load-compiled-path))))

(use-modules (ice-9 match)
             (srfi srfi-1)
             (system foreign)
             (lepton color-map)
             (lepton config)
             (lepton eval)
             (lepton ffi)
             (lepton file-system)
             (lepton log)
             (lepton os)
             (lepton srfi-37)
             (lepton version)
             (schematic core gettext)
             (schematic ffi)
             (schematic ffi gtk)
             (schematic gui keymap)
             (schematic menu))

;;; Initialize liblepton library.
(liblepton_init)
(unless (getenv "LEPTON_INHIBIT_RC_FILES")
  (register-data-dirs))
(edascm_init)


;;; Localization.
(bindtextdomain %schematic-gettext-domain "@localedir@")
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
  (set! %load-compiled-path (cons "@ccachedir@"
                                  %load-compiled-path)))
(define (register-guile-funcs)
  (g_init_window))

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


;;; Load GTK resource files.
;;; Search system and user configuration directories for
;;; lepton-gtkrc files and load them in sequence.
(define (parse-gtkrc)
  (let loop ((dirs (append (sys-config-dirs)
                           (list (user-config-dir)))))
    (or (null? dirs)
        (let ((filename (string-append (car dirs)
                                       file-name-separator-string
                                       "lepton-gtkrc")))
          (when (file-readable? filename)
            (gtk_rc_parse filename))
          (loop (cdr dirs))))))




;;; Setup default icon for GTK windows
;;; Sets the default window icon by name, to be found in the
;;; current icon theme.
(define (set-window-default-icon)
  (define %theme-icon-name "lepton-schematic")

  (gtk_window_set_default_icon_name %theme-icon-name))

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

;;; Creates a new window in lepton-schematic.
(define (make-schematic-window)
  (define new-window (x_window_setup (x_window_new)))

  (x_window_create_main new-window
                        (make-main-menu new-window)
                        *process-key-event))

(define (main file-list)
  ;; Create a new window and associated LeptonToplevel object.
  (define window (make-schematic-window))
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

;;; Parse custom GTK resource files.
(parse-gtkrc)

;;; Set default icon theme and make sure we can find our own
;;; icons.
(set-window-default-icon)
(init-window-icons)

;;; Enable rendering of placeholders.
(set_render_placeholders)

;;; Init libstroke.
(x_stroke_init)

(let* ((schematics (parse-commandline))
       ;; Foreign pointer to w_current.
       (window (main schematics)))

  ;; %lepton-window is a fluid defined in C code, namely in
  ;; g_window.c.  When a new lepton-schematic window is created,
  ;; the fluid is initialized to the window's own pointer to
  ;; itself.  Thus, any Scheme callback procedure called inside
  ;; the window may use just the value of the fluid to reference
  ;; its window, thus avoiding the need of any additional
  ;; arguments.  Here we reference the fluid to enter into the
  ;; dynamic state of the new window created above and eval
  ;; post-load expressions (if any) within it.  'window' is
  ;; already a Scheme pointer object (wrapping a C pointer), so we
  ;; don't have to use pointer->scm to make the fluid happy.
  (with-fluid* %lepton-window window
               eval-post-load-expr!))

;;; Run main GTK loop.
(gtk_main)
