;;; Lepton EDA attribute editor
;;; Copyright (C) 2003-2010 Stuart D. Brorson.
;;; Copyright (C) 2005-2016 gEDA Contributors
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

(use-modules (ice-9 getopt-long)
             (ice-9 receive)
             (srfi srfi-1)
             (system foreign)
             (lepton ffi lib)
             (lepton ffi)
             (lepton file-system)
             (lepton log)
             (lepton page)
             (lepton rc)
             (lepton toplevel)
             (lepton version))

;;; Initialize liblepton library.
(liblepton_init)
(unless (getenv "LEPTON_INHIBIT_RC_FILES")
  (register-data-dirs))
(edascm_init)

(define libleptonattrib (dynamic-link %libleptonattrib))

(define gtk_init
  (pointer->procedure
   void
   (dynamic-func "gtk_init" libgtk)
   (list '* '*)))

(define set_verbose_mode
  (pointer->procedure
   void
   (dynamic-func "set_verbose_mode" libleptonattrib)
   '()))

(define x_fileselect_open
  (pointer->procedure
   '*
   (dynamic-func "x_fileselect_open" libleptonattrib)
   '()))

(define lepton_attrib_window
  (pointer->procedure
   int
   (dynamic-func "lepton_attrib_window" libleptonattrib)
   '(*)))

;;; Localization.
(define %textdomain "libleptonattrib")
(bindtextdomain %textdomain %lepton-localedir)
(textdomain %textdomain)
(bind-textdomain-codeset %textdomain "UTF-8")
(setlocale LC_ALL "")
(setlocale LC_NUMERIC "C")

(define (G_ msg) (gettext msg %textdomain))

(define (usage)
  (format #t
          (G_ "Usage: ~A [OPTIONS] FILE ...

lepton-attrib: Lepton EDA attribute editor.
Presents schematic attributes in easy-to-edit spreadsheet format.

Options:
  -v, --verbose          Verbose mode on
  -V, --version          Show version information
  -h, --help             This help menu

Report bugs at ~S
Lepton EDA homepage: ~S
")
          (basename (car (program-arguments)))
          (lepton-version-ref 'bugs)
          (lepton-version-ref 'url))

  (exit 0))


(define (report-unreadable filename)
  (format (current-error-port)
          "Could not open file ~S.\n"
          filename))

(define (gslist->list gslist)
  (let loop ((gsls gslist)
             (ls '()))
    (if (null-pointer? gsls)
        ls
        (let* ((elem (parse-c-struct gsls (list '* '*)))
               (str (pointer->string (first elem)))
               (gsls (second elem)))
          (loop gsls (cons str ls))))))


(define (process-gafrc* name)
  (process-gafrc "lepton-attrib" name))


;;; Init logging.
(init-log "attrib")
(display-lepton-version #:print-name #t #:log #t)


(let* ((option-spec '((help (single-char #\h))
                      (verbose (single-char #\v))
                      (version (single-char #\V))))

       (options (getopt-long (program-arguments) option-spec))
       (help (option-ref options 'help #f))
       (version (option-ref options 'version #f))
       (files (option-ref options '() '()))
       (verbose? (option-ref options 'verbose #f)))

  (when help (usage))
  ;; Output version to stdout and exit, if requested.
  (when version
    (display-lepton-version #:print-name #t #:copyright #t)
    (exit 0))
  (when verbose? (set_verbose_mode))

  (receive (readable-files unreadable-files)
      (partition file-readable? files)
    (if (null? unreadable-files)
        ;; Main procedure.
        (begin
          ;; Initialize GTK.
          (gtk_init %null-pointer %null-pointer)
          (let ((files (if (null? readable-files)
                           ;; No files specified on the command
                           ;; line, pop up the File open dialog.
                           (gslist->list (x_fileselect_open))
                           readable-files)))
            (if (null? files)
                (exit 0)
                (with-toplevel (make-toplevel)
                 (lambda ()
                   (for-each process-gafrc* files)
                   ;; Open all files.
                   (for-each file->page files)
                   ;; Run attribute editor.
                   (exit (lepton_attrib_window (toplevel->pointer (current-toplevel)))))))))

        ;; There are non-existing or unreadable files.  Report and
        ;; exit.
        (begin
          (for-each report-unreadable unreadable-files)
          (exit 1)))))
