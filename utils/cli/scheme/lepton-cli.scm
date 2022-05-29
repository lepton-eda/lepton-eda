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

(use-modules (lepton ffi)
             (lepton gettext)
             (lepton srfi-37)
             (lepton version))

;;; Initialize liblepton library.
(liblepton_init)
(unless (getenv "LEPTON_INHIBIT_RC_FILES")
  (register-data-dirs))
(edascm_init)


(define %cli (basename (car (program-arguments))))
(define %rest-args (cdr (program-arguments)))
(define %commands
  '("shell" "config" "export"))

(define (run-help-prompt)
  (format (current-error-port)
          (G_ "\nRun `~A --help' for more information.\n")
          %cli)
  (exit 1))

(define (check-command command)
  (unless (member command %commands)
    (format (current-error-port)
            (G_ "ERROR: Unrecognised command ~S.\n")
            command)
    (run-help-prompt)))


;;; Print brief help message describing lepton-cli usage and
;;; command-line options, and exit with exit status 0.
(define (usage)
  (format #t
          (G_ "Usage: ~A [OPTION...] COMMAND [ARGS ...]

Lepton EDA command-line utility.

General options:
  --no-rcfiles   inhibit loading of 'gafrc' files
  -h, --help     display usage information and exit
  -V, --version  display version information and exit

Commonly-used commands (type `lepton-cli <cmd> --help' for usage):
  shell          Scheme REPL for interactive Lepton EDA data processing
  config         Edit Lepton EDA configuration
  export         Export Lepton EDA files in various image formats.

Report bugs at <~A>
Lepton EDA homepage: <~A>
")
          %cli
          (lepton-version-ref 'bugs)
          (lepton-version-ref 'url))
  (exit 0))

(define (wrong-command? arguments)
  (or (null? arguments)
      (string= "--" (car arguments))))

(define (wrong-command-error)
  (format (current-error-port)
          (G_ "ERROR: You must specify a command to run."))
  (run-help-prompt))

;;; Parse command line options.
(define (parse-commandline)
  (if (wrong-command? %rest-args)
      (wrong-command-error)
      (args-fold
       %rest-args
       (list
        (option '(#\h #\? "help") #f #f
                (lambda (opt name arg seeds)
                  (usage)))
        (option '(#\V "version") #f #f
                (lambda (opt name arg seeds)
                  (display-lepton-version #:print-name #t #:copyright #t)
                  (exit 0)))
        (option '("no-rcfiles") #f #f
                (lambda (opt name arg seeds)
                  (putenv "LEPTON_INHIBIT_RC_FILES=1") ; for FreeBSD
                  ;; Reduce the number of rest arguments.
                  (set! %rest-args (cdr %rest-args))
                  seeds)))
       (lambda (opt name arg seeds)
         (format #t
                 (G_ "ERROR: Unknown option ~A.\n")
                 (if (char? name)
                     (string-append "-" (char-set->string (char-set name)))
                     (string-append "--" name)))
         (run-help-prompt))
       (lambda (op seeds)
         (check-command op)
         (let ((prog-name
                (if (string= op "shell")
                    (or (getenv "LEPTON_SHELL")
                        (string-append %lepton-bindir
                                       file-name-separator-string
                                       "lepton-shell"))
                    (if (string= op "config")
                        (or (getenv "LEPTON_CONFIG")
                            (string-append %lepton-bindir
                                           file-name-separator-string
                                           "lepton-config"))
                        (or (getenv "LEPTON_EXPORT")
                            (string-append %lepton-bindir
                                           file-name-separator-string
                                           "lepton-export"))))))
           (apply execle
                  prog-name
                  (environ)
                  ;; Conventionally, the first arg for execl*
                  ;; functions is the same as program name.
                  (cons prog-name (cdr %rest-args)))))
       '())))

(define %cli-gettext-domain "lepton-cli")

;;; Localization.
(bindtextdomain %cli-gettext-domain %lepton-localedir)
(textdomain %cli-gettext-domain)
(bind-textdomain-codeset %cli-gettext-domain "UTF-8")
(setlocale LC_ALL "")
(setlocale LC_NUMERIC "C")

(parse-commandline)
;;; If we're here, that means something went wrong.
(wrong-command-error)
