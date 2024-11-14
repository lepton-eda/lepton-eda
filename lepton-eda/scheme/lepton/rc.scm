;;; Lepton EDA library - Scheme API
;;; Copyright (C) 2007-2016 gEDA Contributors
;;; Copyright (C) 2017-2025 Lepton EDA Contributors
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

;;; Legacy Scheme RC interface.

(define-module (lepton rc)
  #:use-module (srfi srfi-1)
  #:use-module (system foreign)

  #:use-module (lepton eval)
  #:use-module (lepton ffi)
  #:use-module (lepton file-system)
  #:use-module (lepton gettext)
  #:use-module (lepton log)
  #:use-module (lepton os)
  #:use-module (lepton toplevel foreign)
  #:use-module (lepton toplevel)

  #:export (build-path
            geda-data-path
            geda-rc-path
            path-sep
            load-scheme-dir
            load-rc-from-sys-config-dirs
            parse-rc
            process-gafrc))

(define path-sep file-name-separator-string)

(define geda-data-path (last (sys-data-dirs)))
(define geda-rc-path (last (sys-config-dirs)))

(define (build-path first . rest)
  "Build path from one or more path components, separating them by
system file name separator string."
  (string-join (cons first rest) file-name-separator-string))

(define (load-scheme-dir scheme-dir)
  "Evaluate any scheme files found in the given directory SCHEME-DIR."
  (if (and (file-exists? scheme-dir)
           (directory? scheme-dir)
           (access? scheme-dir R_OK))
      (let ((dir (opendir scheme-dir)))
        (do ((entry (readdir dir) (readdir dir)))
            ((eof-object? entry))
          (let ((path (build-path scheme-dir entry)))
            (if (and (regular-file? path)
                     (string-suffix? ".scm" path)
                     (access? path R_OK))
                (eval-protected `(primitive-load ,path))
                #f
                )))
        (closedir dir))
      #f))

(define (load-rc-from-sys-config-dirs basename)
  "Load rc file BASENAME from the system configuration
path (rather than the regular Scheme load path)."
  (let ((rc-file (search-path (sys-config-dirs) basename '("" ".scm"))))
    ;; Use primitive-load to suppress autocompilation
    (if rc-file (primitive-load rc-file))))


;;; General RC file parsing function.
;;;
;;; Calls the default error handler. If any error other than
;;; ENOENT occurs while loading or running a Scheme initialisation
;;; file, prints an informative message and calls exit(1).
;;;
;;; \bug liblepton shouldn't call exit() - this function calls
;;;      g_rc_parse__process_error(), which does.
;;;
;;; \warning Since this function may not return, it should only be
;;; used on application startup or when there is no chance of data
;;; loss from an unexpected exit().
(define (parse-rc program-name rc-name)
  "Parses RC file RC-NAME in the namespace of PROGRAM-NAME.
RC-NAME should be a basename of RC file, such as, for example,
\"gafrc\"."
  (g_rc_parse_handler (toplevel->pointer (current-toplevel))
                      (string->pointer rc-name)
                      %null-pointer
                      (procedure->pointer void g_rc_parse__process_error '(* *))
                      (string->pointer program-name)))


;;; List of processed rc directories.
(define %rc-dirs (make-hash-table))


;;; Backward compatibility stuff.
(define (process-gafrc program schematic)
  "Process \"gafrc\" file for PROGRAM in the directory SCHEMATIC
resides in."
  (let ((cwd (getcwd)))
    (unless (hash-ref %rc-dirs cwd)
      (let ((dir (dirname schematic)))
        (if (directory? dir)
            (begin
              (chdir dir)
              (parse-rc program "gafrc")
              (hash-set! %rc-dirs cwd cwd)
              (chdir cwd))
            (log! 'message
                  (G_ "Could not process \"gafrc\" in the directory ~S: the directory does not exist.")
                  dir))))))
