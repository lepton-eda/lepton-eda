;;; Lepton EDA library - Scheme API
;;; Copyright (C) 2011-2016 gEDA Contributors
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

;;; Source library is a list of directories to search for source
;;; files by their basenames. The directories are searched for
;;; files recursively. First found file with a given basename is
;;; returned.
;;; If any given directory is not readable, error is returned.
;;; If there are several files with the same given basename,
;;; a warning is output that some of those files won't be used.


;;; Lepton component library procedures.

(define-module (lepton library component)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (system foreign)

  #:use-module (lepton ffi)
  #:use-module (lepton file-system)
  #:use-module (lepton gettext)
  #:use-module (lepton log)
  #:use-module (lepton os)

  #:export (component-library
            component-library-search
            component-library-command
            component-library-funcs
            component-libraries
            reset-component-library
            absolute-component-name)

  #:export-syntax (make-symbol-library
                   symbol-library?
                   symbol-library-name set-symbol-library-name!
                   symbol-library-path set-symbol-library-path!))

(define-record-type <symbol-library>
  (make-symbol-library name path)
  symbol-library?
  (name symbol-library-name set-symbol-library-name!)
  (path symbol-library-path set-symbol-library-path!))


(define %component-libraries '())
(define (component-libraries)
  %component-libraries)


(define (add-component-library! path name)
  "Adds the component library with specified PATH and NAME to the
list of component libraries.  NAME is a descriptive name for
library directory."
  (define (library-path-exists? path)
    (let loop ((libs %component-libraries))
      (and (not (null? libs))
           (or (string= path (symbol-library-path (car libs)))
               (loop (cdr libs))))))

  (define (normalize-path path)
    (if (absolute-file-name? path)
        path
        (string-append (getcwd)
                       file-name-separator-string
                       path)))

  ;; take care of any shell variables
  (let ((expanded-path (normalize-path (expand-env-variables path))))

    (if (directory? expanded-path)
        (if (library-path-exists? expanded-path)
            (log! 'message (G_ "Library at ~S has been already added.")
                  expanded-path)
            ;; If anything is OK, just add the new library.
            (begin
              (set! %component-libraries
                    (cons (make-symbol-library name expanded-path)
                          %component-libraries))
              (s_clib_add_directory (string->pointer expanded-path)
                                    (string->pointer name))))
        ;; Report that path is invalid.
        (log! 'warning
              (G_ "Invalid path ~S passed to component-library.\n")
              expanded-path))))


(define (component-library-symbol-names path)
  (define symbol-files
    (match-lambda
      ;; Pick up only flat files.
      ((name stat)
       (and (string-suffix-ci? ".sym" name) name))
      ;; Filter anything other.
      (_ #f)))

  (define directory-symbols
    (match-lambda
      ;; The path must be directory.
      ((name stat children ...)
       (filter-map symbol-files children))
      ;; Return empty list if something went wrong.
      (_ '())))

  (define (enter? name stat) #t)

  ;; Use the 'stat' procedure here to allow traversing of symbolic
  ;; links.
  (let ((tree (file-system-tree path enter? stat)))
    (if tree
        (directory-symbols tree)
        '())))


(define (lookup-in-component-library path symbol-name)
  (let loop ((symbol-names (component-library-symbol-names path)))
    (and (not (null? symbol-names))
         (or (string= symbol-name (car symbol-names))
             (loop (cdr symbol-names))))))


(define (lookup-in-component-libraries symbol-name)
  (let loop ((libs (component-libraries)))
    (and (not (null? libs))
         (let ((lib (car libs)))
           (if (lookup-in-component-library (symbol-library-path lib)
                                            symbol-name)
               lib
               (loop (cdr libs)))))))


(define (absolute-component-name component-basename)
  (let ((lib (lookup-in-component-libraries component-basename)))
    (and lib
         (string-append (symbol-library-path lib)
                        file-name-separator-string
                        component-basename))))


(define* (component-library path #:optional name)
  "Adds a component library from PATH optionally named NAME.  If
name is omitted, PATH is used as name instead.  Environment
variables in PATH are expanded."
  ;; Expand environment variables here, too.  They are expanded
  ;; when the procedure is called in component-library-search, but
  ;; in other cases it is not so.
  (let* ((path (expand-env-variables path))
         (name (or name path)))
    (add-component-library! path name)))


(define (component-library-command list-command get-command name)
  "The function can be used in RC files to add component libraries
generated by scripts.  It creates a component library source
called NAME (the third argument) driven by two user commands:
LIST-COMMAND (the first argument) and GET-COMMAND (the second
argument). The list command should return a list of component
names in the source.  The get command should return symbol
contents by specified component name.  Both commands should output
their results to stdout.  Returns #t on success, otherwise returns
#f."
  ;; Take care of any shell variables.
  ;; ! \bug this may be a security risk!
  (let ((real-list-command (expand-env-variables list-command))
        (real-get-command (expand-env-variables get-command)))

    (not (null-pointer? (s_clib_add_command (string->pointer real-list-command)
                                            (string->pointer real-get-command)
                                            (string->pointer name))))))


(define  (component-library-funcs list-function get-function name)
  "The function can be used in Scheme RC files to add a set of
Guile procedures for listing and generating symbols.  It creates a
component library source called NAME (the third argument) driven
by two user Scheme procedures: LIST-FUNCTION (the first argument)
and GET-FUNCTION (the second argument). The list function should
return a Scheme list of component names in the source.  The get
function should return symbol contents by specified component name
as a Scheme string in gEDA format or #f if the component name is
unknown.  Returns #t on success, otherwise returns #f."
  (not (null-pointer?
        (s_clib_add_scm (scm->pointer list-function)
                        (scm->pointer get-function)
                        (string->pointer name)))))


(define (reset-component-library)
  "Reset component library and initialise it to an empty list."
  (set! %component-libraries '())
  (s_clib_init))



(define (make-node-name rootdir dir prefix)
  (define (same-dirs? a b)
    (string=
     (string-trim-right a file-name-separator?)
     (string-trim-right b file-name-separator?)))

  (define (string-drop-rootdir dir)
    (string-trim (string-drop dir (string-length rootdir))
                 file-name-separator?))

  (if (string-null? prefix)
      (if (same-dirs? dir rootdir)
          (basename dir)
          (string-drop-rootdir dir))
      (if (same-dirs? dir rootdir)
          prefix
          (string-append prefix
                         (string-drop-rootdir dir)))))

(define* (component-library-search rootdir  #:optional (prefix ""))
  "Add all symbol libraries found below ROOTDIR to be searched for
components, naming them with an optional PREFIX."
  ;; Recursively removes file name separator suffices in DIR-NAME.
  (define (remove-/-suffices dir-name)
    (if (string-suffix? file-name-separator-string dir-name)
        (remove-/-suffices (string-drop-right dir-name
                                              (string-length file-name-separator-string)))
        dir-name))

  (let ((dht (make-hash-table 31))
        (rootdir (remove-/-suffices (expand-env-variables rootdir))))

    ;; Build symbol directory list.
    (ftw rootdir
         (lambda (filename statinfo flags)
           (cond
            ((eq? 'invalid-stat flags)
             (log! 'critical "Invalid path ~S." filename))
            ((or (eq? 'directory-not-readable flags)
                 (eq? 'symlink flags))
             (format #t "Warning: Cannot access ~S.\n" filename))
            (else
             (and (eq? 'regular flags)
                  (string-suffix-ci? ".sym" filename)
                  (hashq-set! dht
                              (string->symbol (dirname filename))
                              #t))))
           #t))

    ;; Fill component library tree.
    (for-each
     (lambda (dir)
       (let ((name (make-node-name rootdir dir prefix)))
         (component-library dir name)))
     (sort-list! (hash-map->list (lambda (key val)
                                   (symbol->string key))
                                 dht)
                 string>?))))
