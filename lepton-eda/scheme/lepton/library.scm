;;; Lepton EDA library - Scheme API
;;; Copyright (C) 2016 gEDA Contributors
;;; Copyright (C) 2019-2022 Lepton EDA Contributors
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

(define-module (lepton library)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (lepton file-system)
  #:use-module (lepton gettext)
  #:use-module (lepton library component)
  #:use-module (lepton log)
  #:use-module (lepton os)

  #:export (%default-source-library
            ;; deprecated
            source-library
            source-library-search
            reset-source-library
            ;; new
            source-library?
            source-library-contents
            set-source-library-contents!
            ;; temporary
            get-source-library-file)

  #:re-export (component-library
               component-library-search
               component-library-command
               component-library-funcs
               reset-component-library))


(define-record-type <source-library>
  (make-source-library contents)
  source-library?
  (contents library-contents set-library-contents!))

;;; Default source library. Used for support of legacy source
;;; library procedures.
(define %default-source-library (make-source-library '()))

(define (source-library-contents lib)
  "Returns the contents of given source library."
  (library-contents lib))

(define (set-source-library-contents! lib contents)
  "Sets the contents of given source library into the given value."
  (set-library-contents! lib contents))

;;; Prepends PATH to the contents of the source library LIB.
(define (source-library-prepend! lib path)
  (unless (member path (library-contents lib))
    (set-library-contents! lib
                           (cons path (library-contents lib)))))

#|
;;; Appends PATH to the contents of the source library LIB.
(define (source-library-append! lib path)
  (set-library-contents! lib
                         (append (library-contents lib)
                                 (list path))))
|#

(define (source-library path)
  "Prepends the contents of given path to the default source
library. Returns %default-source-library.

This procedure is legacy and should be avoided in new code. Use
set-library-contents! instead."
  (unless (string? path)
    (scm-error 'wrong-type-arg
               "source-library"
               "Wrong type argument in position 1 (expecting string): ~A"
               (list path)
               #f))

  ;; Take care of any shell variables.
  (let ((path (expand-env-variables path)))
    (if (file-readable? path)
        (source-library-prepend! %default-source-library
                                 (if (absolute-file-name? path)
                                     path
                                     (string-append (getcwd)
                                                    file-name-separator-string
                                                    path)))
        (log! 'critical
              (G_ "Invalid path ~S or source not readable.\n")
              path))
    %default-source-library))


(define (reset-source-library)
  "Resets source library, that is, sets its contents to '().
Returns %default-source-library.

This procedure is legacy and should be avoided in new code. Use
set-library-contents! instead."
  (set-library-contents! %default-source-library '())
  %default-source-library)


(define (source-library-search path)
  "Recursively prepends the contents of given path to the default
source library.  Returns %default-source-library.

This procedure is legacy and should be avoided in new code. Use
set-library-contents! instead."
  (define (vcs-name? name)
    (member name '(".git" ".svn" "CVS")))

  ;; Transforms the tree of directories into a plain list of paths,
  ;; filtering out plain files and some VCS related directories.
  (define filter-tree
    (match-lambda
      ;; Flat files and empty directories. Skip them.
      ((name stat) #f)
      ;; Directories.
      ((name stat children ...)
       ;; Skip VCS dirs.
       (and (not (vcs-name? name))
            ;; Process other dirs recursively.
            (cons name (filter-map filter-tree children))))))

  (define (build-filename dir file)
    (string-append dir file-name-separator-string file))

  (define (directory-tree->list name children)
    (cons name
          (append-map
           (lambda (child)
             (if (null? child)
                 '()
                 (directory-tree->list (build-filename name (car child))
                                       (cdr child))))
           children)))

  (define (path->directory-list path)
    (let ((tree (file-system-tree path)))
      ;; If the tree has been created successfully, replace its
      ;; first element with expanded path, and make the plain
      ;; directory list.
      (if tree
          (directory-tree->list path (cdr (filter-tree tree)))
          (begin
            (log! 'critical
                  (G_ "Invalid path ~S or source not readable.\n")
                  path)
            ;; Return empty list in this case.
            '()))))

  (define (trim-trailing-/ s)
    (define sep-len (string-length file-name-separator-string))
    (let loop ((s s))
      (if (string-suffix? file-name-separator-string s)
          (loop (string-drop-right s sep-len))
          s)))

  (define (normalize-path path)
    (trim-trailing-/ (expand-env-variables path)))

  (unless (string? path)
    (scm-error 'wrong-type-arg
               "source-library-search"
               "Wrong type argument in position 1 (expecting string): ~A"
               (list path)
               #f))

  (for-each source-library
            (path->directory-list (normalize-path path)))
  ;; Return value.
  %default-source-library)


;;; This is a temporary procedure for hierarchy traversing support
;;; and its use should be avoided.
(define (get-source-library-file base-name)
  "Searches in default source library for a file with given
basename and returns its full name. Returns #f if no file has
been found."
  (define get-files
    (match-lambda
      ((name stat)                      ; flat file
       name)
      (_ #f)))
  (define get-dir-files
    (match-lambda ((name stat children ...)
                   (filter-map get-files children))))

  (unless (string? base-name)
    (scm-error 'wrong-type-arg
               "get-source-library-file"
               "Wrong type argument in position 1 (expecting string): ~A"
               (list base-name)
               #f))

  (let loop ((paths (source-library-contents
                     %default-source-library)))
    (and (not (null? paths))
         (if (member base-name (get-dir-files (file-system-tree (car paths))))
             (let ((full-name (string-append (car paths)
                                             file-name-separator-string
                                             base-name)))
               (if (file-readable? full-name)
                   full-name
                   (begin
                     (log! 'critical (G_ "File ~S is not readable.\n")
                           full-name)
                     (loop (cdr paths)))))
             (loop (cdr paths))))))
