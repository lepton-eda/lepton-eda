;;; Lepton EDA netlister
;;; Copyright (C) 2017-2023 Lepton EDA Contributors
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
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)

  #:use-module (lepton file-system)
  #:use-module (lepton gettext)
  #:use-module (lepton log)

  #:use-module (netlist error)
  #:use-module (netlist mode)

  #:export (lookup-legacy-backends
            lookup-module-backends
            run-backend
            make-legacy-backend
            make-module-backend))

(define-record-type <backend>
  (backend path name runner legacy)
  backend?
  (path backend-path set-backend-path!)
  (name backend-name set-backend-name!)
  (runner backend-runner set-backend-runner!)
  (legacy backend-legacy set-backend-legacy!))


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

(define (error-backend-proc-not-found func-name backend-path)
  (netlist-error 1 (G_ "Could not find function ~S in backend ~S.\n")
                 func-name backend-path))


(define (legacy-backend-filename? filename)
  "Return #t if FILENAME is a backend filename, otherwise return
#f."
  (and (string-prefix? %backend-prefix filename)
       (string-suffix? %backend-suffix filename)))


(define (module-backend-filename? filename)
  (and (string-suffix? %backend-suffix filename)
       (not (legacy-backend-filename? (basename filename)))))


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


(define (load-scheme-script filename)
  (define (load-and-log name)
    (log! 'message (G_ "Loading ~S") name)
    (primitive-load name))

  ;; If the file exists in the current directory, or its name is
  ;; absolute, just load it.
  (if (file-readable? filename)
      (load-and-log filename)
      ;; Otherwise, try to find it in %load-path.
      (let ((file (%search-load-path filename)))
        (if file
            (load-and-log file)
            (log! 'warning (G_ "Could not find file ~S in %load-path.") filename)))))


;;; Loads the list of Scheme scripts LS reporting ERROR-MSG if
;;; something went wrong.  In the latter case, the program exits
;;; with exit status 1.
(define* (load-scheme-scripts ls #:optional (post-load? #f))
  (define pre-load-error
    (G_ "Failed to load Scheme file before loading backend.\n"))
  (define post-load-error
    (G_ "Failed to load Scheme file after loading backend.\n"))
  (catch #t
    (lambda ()
      (for-each load-scheme-script ls))
    (lambda (key subr message args rest)
      (format (current-error-port) (G_ "ERROR: ~?\n") message args)
      (netlist-error 1 (if post-load? post-load-error pre-load-error)))))


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


(define (load-backend-file filename)
  (catch #t
    (lambda ()
      (primitive-load filename)
      (query-backend-mode))
    (lambda (key subr message args rest)
      (format (current-error-port) (G_ "ERROR: ~?\n") message args)
      (netlist-error 1 (G_ "Failed to load backend file.\n")))))


(define* (make-legacy-backend #:key
                              (path #f)
                              (name #f)
                              (pre-load '())
                              (post-load '()))
  ;; Path set by '-f' has priority over name set by '-g'.
  (define filename (or path (backend-path-by-name name)))
  (define func-name (or name (backend-name-by-path path)))

  ;; Load Scheme FILE before loading backend (-l FILE).
  (load-scheme-scripts pre-load)

  (load-backend-file filename)

  ;; Load Scheme FILE after loading backend (-m FILE).
  (load-scheme-scripts post-load 'post-load)

  (let ((proc (primitive-eval (string->symbol func-name))))
    (if proc
        (backend path name proc #t)
        (error-backend-proc-not-found func-name filename))))


;;; Checks that the module file exists, exports the NAME binding,
;;; and the binding is a procedure.
(define* (make-module-backend #:key
                              (name #f)
                              (pre-load '())
                              (post-load '()))
  (define (warn-module-backend-not-found)
    (begin
      (log! 'message
            (G_ "Could not find ~S in the module ~S.\n~
                 Fall back to looking up for legacy backend ~S.")
            name
            module-sym
            name))
    #f)
  (define backend-sym (string->symbol name))
  (define module-sym (list 'backend backend-sym))
  ;; Ensure that module file exists.
  (define module (resolve-module module-sym #:ensure #f))
  ;; Resolve interface of the module to process only its exported
  ;; bindings.
  (define interface (and module (resolve-interface module-sym)))
  ;; Test if the variable NAME exists in the module.
  (define var (and interface (module-variable interface backend-sym)))
  ;; Get the value of the variable.
  (define proc (and var (module-ref interface backend-sym)))

  ;; Check that the variable is a procedure.
  (if (procedure? proc)
      (backend (module-filename module) name proc #f)
      ;; Returns #f.
      (warn-module-backend-not-found)))


(define (run-backend backend output-filename)
  "Runs backend's function BACKEND with redirection of its
standard output to OUTPUT-FILENAME.  If OUTPUT-FILENAME is #f, no
redirection is carried out."
  (define backend-proc (backend-runner backend))
  (define thunk
    (if (backend-legacy backend)
        ;; Legacy backends require one argument.
        (lambda () (backend-proc output-filename))
        ;; Module backends do not require arguments.
        backend-proc))

  (if output-filename
      ;; output-filename is defined, output to it.
      (with-output-to-file output-filename thunk)
      ;; output-filename is #f, output to stdout.
      (thunk)))


(define (lookup-module-backends)
  (define (build-filename path filename)
    (string-append path file-name-separator-string filename))

  (define (path->backend-path path)
    (false-if-exception
     (canonicalize-path (build-filename path "backend"))))

  (define backend-dirs (filter-map path->backend-path %load-path))

  (define (regular-file-in-dir? path filename)
    (and (regular-file? (build-filename path filename)) filename))

  (define (scm-files path)
    (filter-map (cut regular-file-in-dir? path <>) (scandir path)))

  (define backend-basenames
    (filter module-backend-filename? (append-map scm-files backend-dirs)))

  (define (backend-name filename)
    (basename filename %backend-suffix))

  ;; Forms backend name from file basename BASE and tests if a
  ;; module backend can be created for the name.  Returns the name
  ;; on success, or #f otherwise.
  (define (backend-name/test base)
    (let ((name (backend-name base)))
      (and name
           (make-module-backend #:name name)
           name)))

  (filter-map backend-name/test backend-basenames))


(define (lookup-legacy-backends)
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

  (map backend-filename->proc-name backend-files))
