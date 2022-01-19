#!/usr/bin/env sh
exec @GUILE@ -e main -s "$0" "$@"
!#

;;; Lepton EDA command-line utility
;;; Copyright (C) 2012-2013 Peter Brett <peter@peter-b.co.uk>
;;; Copyright (C) 2012-2014 gEDA Contributors
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
    (add-to-load-path "@LEPTON_SCHEME_MODULE_DIRECTORY@")
    (set! %load-compiled-path (cons "@ccachedir@" %load-compiled-path))))

(use-modules (srfi srfi-37)
             (ice-9 eval-string)
             (ice-9 readline)
             (lepton ffi)
             (lepton toplevel))

;;; Initialize liblepton library.
(liblepton_init)
(unless (getenv "LEPTON_INHIBIT_RC_FILES")
  (register-data-dirs))
(edascm_init)

(primitive-eval '(use-modules (lepton core gettext)
                              (lepton core toplevel)
                              (lepton rc)
                              (lepton repl)
                              (lepton version)))

(define cmd (basename (car (program-arguments))))
(define cmd-args (cdr (program-arguments)))

(define (shell-usage)
  (format #t (G_ "Usage: ~A [OPTION ...]

Shell for interactive processing of Lepton EDA data using Scheme.

  -s FILE        load Scheme source code from FILE, and exit
  -c EXPR        evaluate Scheme expression EXPR, and exit
  --             stop scanning arguments; run interactively

The above switches stop argument processing, and pass all
remaining arguments as the value of (command-line).

  -L DIRECTORY   add DIRECTORY to the front of the Scheme load path
  -l FILE        load Scheme source code from FILE
  -h, --help     display usage information and exit

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


(define (primitive-load-file filename)
  (catch 'system-error
    (lambda () (primitive-load filename))
    (lambda (key subr message args rest)
      (format (current-error-port)
              (G_ "ERROR: Could not load file ~S: ~?\n")
              filename
              message
              args)
      (exit 1))))

;;; Parse command line arguments.
(define (cli-shell)
  (catch 'misc-error
    (lambda ()
      (args-fold
       cmd-args
       (list
        (option '(#\h "help") #f #f
                (lambda (opt name arg seeds)
                  (shell-usage)))
        (option '(#\c) #t #f
                (lambda (opt name arg seeds)
                  (cons (delay (begin
                                 (eval-string arg)
                                 (exit 0)))
                        seeds)))
        (option '(#\l) #t #f
                (lambda (opt name arg seeds)
                  (cons (delay (primitive-load-file arg))
                        seeds)))
        (option '(#\L) #t #f
                (lambda (opt name arg seeds)
                  (cons (delay (add-to-load-path arg))
                        seeds)))
        (option '(#\s) #t #f
                (lambda (opt name arg seeds)
                  (cons (delay (begin
                                 (primitive-load-file arg)
                                 (exit 0)))
                        seeds))))
       (lambda (opt name arg seeds)
         (format (current-error-port)
                 (G_ "ERROR: unrecognized option ~S.\n")
                 (if (char? name)
                     (string-append "-" (char-set->string (char-set name)))
                     (string-append "--" name)))
         (run-help-prompt))
       (lambda (op seeds)
         (cons op seeds))
       '()))
    (lambda (key subr message args rest)
      (format (current-error-port) (G_ "ERROR: ~?\n") message args)
      (run-help-prompt))))

(define %cli-gettext-domain "lepton-cli")

(define (main args)
  ;; Localization.
  (bindtextdomain %cli-gettext-domain "@localedir@")
  (textdomain %cli-gettext-domain)
  (bind-textdomain-codeset %cli-gettext-domain "UTF-8")
  (setlocale LC_ALL "")
  (setlocale LC_NUMERIC "C")

  (%with-toplevel
   (%make-toplevel)
   (lambda ()
     (unless (getenv "LEPTON_INHIBIT_RC_FILES")
       (parse-rc "lepton-shell" "gafrc"))
     (let ((seed-ls (reverse (cli-shell))))
       (for-each
        (lambda (seed)
          (if (promise? seed)
              (force seed)
              (begin
                (primitive-load-file seed)
                (exit 0))))
        seed-ls)
       (activate-readline)
       (lepton-repl)))))
