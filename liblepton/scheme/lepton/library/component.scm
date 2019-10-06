;;; Lepton EDA library - Scheme API
;;; Copyright (C) 2011-2016 gEDA Contributors
;;; Copyright (C) 2017-2019 Lepton EDA Contributors
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
;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA

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
  #:use-module (geda log)
  #:use-module (geda os)
  #:use-module (lepton core rc)

  #:export (component-library
            component-library-search
            component-library-command
            component-library-funcs
            reset-component-library))

(define %component-libraries '())
(define (component-libraries)
  %component-libraries)

(define (add-component-library! path name)
  (set! %component-libraries
        (assoc-set! %component-libraries path name))
  (%component-library path name))

(define* (component-library path #:optional name)
  (let ((lib (assoc-ref %component-libraries path))
        (name (or name path)))
    (if lib
        (log! 'message "Skip already added path ~S." path)
        (add-component-library! path name))))

(define component-library-command %component-library-command)
(define component-library-funcs %component-library-funcs)
(define (reset-component-library)
  (set! %component-libraries '())
  (%reset-component-library))

(define* (component-library-search rootdir  #:optional (prefix ""))
  "Add all symbol libraries found below ROOTDIR to be searched for
components, naming them with an optional PREFIX."

  (let ((dht (make-hash-table 31))
        (rootdir (expand-env-variables rootdir)))

    ;; Build symbol directory list.
    (ftw rootdir
         (lambda (filename statinfo flags)
           (cond
            ((eq? 'invalid-stat flags)
             (error "Invalid path ~S." filename))
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
       (let ((name (substring dir (string-length rootdir))))
         (component-library dir (string-append prefix name))))
     (sort-list! (hash-map->list (lambda (key val)
                                   (symbol->string key))
                                 dht)
                 string>?))))
