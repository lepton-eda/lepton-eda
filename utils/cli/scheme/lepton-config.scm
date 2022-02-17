#!/usr/bin/env sh
exec @GUILE@ -s "$0" "$@"
!#

;;; Lepton EDA command-line utility
;;; Copyright (C) 2012 Peter Brett <peter@peter-b.co.uk>
;;; Copyright (C) 2015 gEDA Contributors
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
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

(eval-when (expand load eval)
  (unless (getenv "LIBLEPTON")
    (add-to-load-path "@LEPTON_SCHEME_DIR@")
    (set! %load-compiled-path (cons "@ccachedir@" %load-compiled-path))))

(use-modules (ice-9 match)
             (srfi srfi-1)
             (lepton config)
             (lepton core gettext)
             (lepton ffi)
             (lepton file-system)
             (lepton srfi-37)
             (lepton version))

;;; Initialize liblepton library.
(liblepton_init)
(unless (getenv "LEPTON_INHIBIT_RC_FILES")
  (register-data-dirs))
(edascm_init)


(define cmd (basename (car (program-arguments))))
(define cmd-args (cdr (program-arguments)))

(define (config-usage)
  (format #t (G_ "Usage: ~A [OPTION] [GROUP KEY [VALUE]]

View and modify Lepton EDA configuration.

  -p [PATH], --project[=PATH]
                 select project configuration
  -u, --user     select user configuration
  -s, --system   select system configuration
  -c, --cache    select cache configuration
  -h, --help     display usage information and exit

If GROUP and KEY are specified, retrieves the value of that
configuration parameter.  If a VALUE was specified, sets the value of
the parameter.  The -p, -u and -s options can be used to select the
configuration store affected (by default, the project configuration
store for the current directory).  Any argument following to the
option -p is considered to be a path, not a configuration group.
If no GROUP and KEY were provided, outputs the filename of the
selected configuration store.

Report bugs at ~S
Lepton EDA homepage: ~S
")
          cmd
          (lepton-version-ref 'bugs)
          (lepton-version-ref 'url))

  (exit 0))

(define (run-help-prompt)
  (format (current-error-port)
          (G_ "\nRun `~A --help' for more information.\n")
          cmd)
  (exit 1))

(define (multi-store-error)
  (format (current-error-port)
          (G_ "ERROR: You may only specify a single configuration store.\n"))
  (run-help-prompt))

(define %project-store-path #f)

(define (project-store-path)
  %project-store-path)

(define (set-project-store-path! path)
  (set! %project-store-path path))

(define %current-config-context #f)

(define (current-config-context)
  %current-config-context)

(define (set-current-config-context! cfg)
  (set! %current-config-context cfg))

;;; FIXME: it's from (netlist config), should be (lepton config)
;;; instead.
(define (try-load-config cfg)
  (catch 'system-error
    (lambda () ;; (when (file-exists? (config-filename cfg))
      ;;   (config-load! cfg))
      (config-load! cfg)
       )
    (lambda (key subr message args rest)
      (format (current-error-port)
              "WARNING: Could not load '~A': ~?.\n"
              (config-filename cfg)
              message
              args))))

(define (try-retrieve-config-string cfg group key)
  (catch 'config-error
    (lambda () (config-string cfg group key))
    (lambda (key subr message args rest)
      (format (current-error-port)
              "ERROR: ~?.\n"
              message
              args)
      (exit 1))))

(define (try-save-config cfg)
  (catch 'system-error
    (lambda () (config-save! cfg))
    (lambda (key subr message args rest)
      (format (current-error-port)
              "ERROR: ~?\n"
              message
              args)
      (exit 1))))

(define (error-wrong-directory dirname)
  (format (current-error-port)
          (G_ "ERROR: Directory ~S does not exist.\n")
          dirname)
  (exit 1))

(define (parse-commandline*)
  ;; Parse command-line arguments
  (args-fold
   cmd-args
   (list
    (option '(#\p "project") #f #t
            (lambda (opt name arg seeds)
              (if (or (current-config-context)
                      (project-store-path))
                  (multi-store-error)
                  (if arg
                      (if (and (file-exists? arg) (directory? arg))
                          (set-project-store-path! arg)
                          (error-wrong-directory arg))
                      (set-project-store-path! ".")))
              seeds))
    (option '(#\s "system") #f #f
            (lambda (opt name arg seeds)
              (if (or (current-config-context)
                      (project-store-path))
                  (multi-store-error)
                  (set-current-config-context! (system-config-context)))
              seeds))
    (option '(#\u "user") #f #f
            (lambda (opt name arg seeds)
              (if (or (current-config-context)
                      (project-store-path))
                  (multi-store-error)
                  (set-current-config-context! (user-config-context)))
              seeds))
    (option '(#\c "cache") #f #f
            (lambda (opt name arg seeds)
              (if (or (current-config-context)
                      (project-store-path))
                  (multi-store-error)
                  (set-current-config-context! (cache-config-context)))
              seeds))
    (option '(#\h "help") #f #f
            (lambda (opt name arg seeds)
              (config-usage))))
   (lambda (opt name arg seeds)
     (format #t
             (G_ "ERROR: Unknown option ~A.\n")
             (if (char? name)
                 (string-append "-" (char-set->string (char-set name)))
                 (string-append "--" name)))
     (run-help-prompt))
   (lambda (op seeds)
     (cons op seeds))
   '()))

(define (parse-commandline)
  (reverse (parse-commandline*)))

(define (get-current-config-context)
  (or (current-config-context)
      ;; If no configuration is available yet, grab the project
      ;; configuration.
      (path-config-context (or (project-store-path) "."))))

(define (print-config-file-name cfg)
  (format #t "~A\n" (config-filename cfg))
  (exit 0))

;;; Attempt to load configuration with all its parents.
(define (load-config-with-parents cfg)
  (when cfg
    (when (and (not (config-loaded? cfg))
               (config-filename cfg))
      (try-load-config cfg))
    (load-config-with-parents (config-parent cfg))))

(define (set-group-key-and-save-config cfg group key value)
  (load-config-with-parents cfg)
  (set-config! cfg group key value)
  (try-save-config cfg))

(define (print-config-string cfg group key)
  (load-config-with-parents cfg)
  (format #t "~A\n" (try-retrieve-config-string cfg group key)))

(define (error-wrong-command-args)
  (format (current-error-port)
          (G_ "ERROR: You must specify both configuration group and key."))
  (run-help-prompt))

(define (error-redundant-args)
  (format (current-error-port)
          (G_ "ERROR: Wrong number of command-line arguments."))
  (run-help-prompt))

(let ((args (parse-commandline))
      (cfg (get-current-config-context)))
  (match args
    (()
     ;; If no arguments were specified, output the configuration
     ;; file location.
     (print-config-file-name cfg))

    ((group key)
     ;; If no value was specified, output the parameter value.
     (print-config-string cfg group key))

    ((group key value)
     ;; If a value was specified, set the value and save the
     ;; configuration.
     (set-group-key-and-save-config cfg group key value))

    ((group key value r0 . rx)
     ;; Redundant argument was specified.
     (error-redundant-args))

    (_ (error-wrong-command-args))))
