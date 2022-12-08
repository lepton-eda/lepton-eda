;;; Lepton EDA netlister
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


(define-module (netlist backend)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 i18n)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)

  #:use-module (lepton gettext)
  #:use-module (lepton log)

  #:use-module (netlist error)
  #:use-module (netlist mode)

  #:export (load-backend
            lookup-backends
            run-backend
            make-backend))

(define-record-type <backend>
  (backend path name)
  backend?
  (path backend-path set-backend-path!)
  (name backend-name set-backend-name!))


(define %backend-prefix "gnet-")
(define %backend-suffix ".scm")
(define %backend-prefix-length (string-length %backend-prefix))
(define %backend-suffix-length (string-length %backend-suffix))


(define (error-backend-wrong-file-name name)
  (netlist-error 1 (G_ "Can't load backend file ~S.\n~
                        Backend files are expected to have names like \"gnet-NAME.scm\"\n~
                        and contain entry point function NAME (where NAME is the backend's name).\n")
                 name))


(define (error-backend-not-found-by-name backend )
  (netlist-error 1 (G_ "Could not find backend `~A' in load path.\n~
                        Run `~A --list-backends' for a full list of available backends.\n")
                 backend (car (program-arguments))))


(define (legacy-backend-filename? filename)
  "Return #t if FILENAME is a backend filename, otherwise return
#f."
  (and (string-prefix? %backend-prefix filename)
       (string-suffix? %backend-suffix filename)))


(define (backend-filename->proc-name filename)
  "Transforms FILENAME to a backend name which is also the name of
the procedure the backend runs.  For legacy backends, the name is
formed by dropping the prefix \"gnet-\" and the extenstion
\".scm\".  Returns the resulting string or #f if FILENAME does not
meet the specified requirements."
  (let ((base (basename filename)))
    (and (legacy-backend-filename? base)
         (string-drop-right (string-drop base %backend-prefix-length)
                            %backend-suffix-length))))


(define (search-backend name)
  "Searches for backend by its NAME."
  (%search-load-path (format #f "gnet-~A.scm" name)))


(define (backend-name-by-path path)
  (or (backend-filename->proc-name path)
      (error-backend-wrong-file-name path)))


(define (backend-path-by-name name)
  (or (search-backend name)
      (error-backend-not-found-by-name name)))


(define* (make-backend #:key (path #f) (name #f))
  (cond
   ((and path name) (backend path name))
   ;; Path set by '-f' has priority over name set by '-g'.
   (path (backend path (backend-name-by-path path)))
   (name (backend (backend-path-by-name name) name))
   (else #f)))


(define (run-backend backend output-filename)
  "Runs backend's function BACKEND with redirection of its
standard output to OUTPUT-FILENAME.  If OUTPUT-FILENAME is #f, no
redirection is carried out."
  (let ((backend-proc (primitive-eval (string->symbol (backend-name backend)))))
    (if output-filename
        ;; output-filename is defined, output to it.
        (with-output-to-file output-filename
          (lambda () (backend-proc output-filename)))
        ;; output-filename is #f, output to stdout.
        (backend-proc output-filename))))


(define (lookup-backends)
  "Searches %load-path for available lepton-netlist backends and
returns the resulting list of filenames.  A file is considered to
be a backend if its basename begins with \"gnet-\" and ends with
\".scm\"."
  (define (path-backends path)
    (or (scandir path legacy-backend-filename?)
        (begin
          (log! 'warning (G_ "Can't open directory ~S.\n") path)
          '())))

  (define backend-files
    (append-map path-backends (delete-duplicates %load-path)))

  (define backend-names
    (map backend-filename->proc-name backend-files))

  (display (string-join
            (sort! backend-names string-locale<?)
            "\n"
            'suffix)))


(define (query-backend-mode)
  "Queries and sets the current netlisting mode.  Backends can
request what netlist mode should be used by providing the function
request-netlist-mode() that should return the desired mode symbol.
The procedure netlist-mode() can be used to find out currently
available netlisting modes."
  (define (error-backend-mode mode)
    (netlist-error
     1
     (G_ "Netlist mode requested by backend is not supported: ~A\n")
     mode))

  (define proc-name 'request-netlist-mode)
  (define proc-binding
    (false-if-exception
     (module-symbol-binding (current-module) proc-name)))

  ;; If the procedure binding exists, and it is really a
  ;; procedure, eval it to get the new netlisting mode.
  (when (procedure? proc-binding)
    (let ((mode (proc-binding)))
      (if (netlist-mode? mode)
          (set-netlist-mode! mode)
          (error-backend-mode mode)))))


(define (load-backend backend)
  (catch #t
    (lambda ()
      (primitive-load (backend-path backend))
      (query-backend-mode))
    (lambda (key subr message args rest)
      (format (current-error-port) (G_ "ERROR: ~?\n") message args)
      (netlist-error 1 (G_ "Failed to load backend file.\n")))))
