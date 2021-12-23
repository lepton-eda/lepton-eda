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
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)

  #:use-module (lepton eval)
  #:use-module (lepton ffi boolean)
  #:use-module (lepton ffi glib)
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


(define (build-filename dir name)
  (string-append dir file-name-separator-string name))


;;; Attempts to load and run the system Scheme initialisation file
;;; with basename *RCNAME in *TOPLEVEL, reporting errors via
;;; **ERR.  The string "system-" is prefixed to *RCNAME.  If
;;; *RCNAME is NULL, the default value of "gafrc" is used.
;;; Returns TRUE on success, FALSE on failure.
(define (parse-system-rc *toplevel *rcname **err)
  (define sysname (string-append "system-"
                                 (if (null-pointer? *rcname)
                                     "gafrc"
                                     (pointer->string *rcname))))

  (let loop ((dirs (sys-config-dirs)))
    (if (null? dirs)
        TRUE
        (let ((rcfile (build-filename (car dirs) sysname)))
          (if (and (file-exists? rcfile)
                   (regular-file? rcfile))
              (g_rc_parse_file *toplevel
                               (string->pointer rcfile)
                               (eda_config_get_system_context)
                               **err)
              (loop (cdr dirs)))))))


(define (load-rc-from-sys-config-dirs basename)
  "Load rc file BASENAME from the system configuration
path (rather than the regular Scheme load path)."
  (let ((rc-file (search-path (sys-config-dirs) basename '("" ".scm"))))
    ;; Use primitive-load to suppress autocompilation
    (if rc-file (primitive-load rc-file))))


;;; General RC file parsing function.
;;;
;;; Attempt to load and run system, user and local (current
;;; working directory) Scheme initialisation files in *TOPLEVEL,
;;; first with the default "gafrc" basename and then with the
;;; basename RCNAME.
;;;
;;; If an error occurs, the function calls HANDLER with the
;;; provided PROGRAM-NAME and a GError.
;;;
;;; The default error handler is currently the function
;;; g_rc_parse__process_error().  If any error other than ENOENT
;;; occurs while loading or running a Scheme initialisation file,
;;; it prints an informative message and calls exit(1).
;;;
;;; Bug: liblepton shouldn't call exit() - the function
;;;      g_rc_parse__process_error() does.
;;;
;;; Warning: Since this function may not return, it should only be
;;; used on application startup or when there is no chance of data
;;; loss from an unexpected exit().
(define* (parse-rc program-name
                   rc-name
                   #:key
                   (handler g_rc_parse__process_error)
                   (*toplevel (toplevel->pointer (current-toplevel))))
  "Parses RC file RC-NAME in the namespace of PROGRAM-NAME.
RC-NAME should be a basename of RC file, such as, for example,
\"gafrc\"."
  (define *rcname (string->pointer rc-name))
  (define *user-data (string->pointer program-name))
  (define (handler-dispatch *error)
    (unless (or (null-pointer? *error)
                (null-pointer? (dereference-pointer *error)))
      (handler *error *user-data)
      (g_clear_error *error)))

  (let ((*error (bytevector->pointer (make-bytevector (sizeof '*) 0))))

    ;; Load cache configuration:
    (g_rc_load_cache_config *toplevel *error)
    (handler-dispatch *error)

    ;; Load RC files in order.
    ;; First gafrc files.
    (parse-system-rc *toplevel %null-pointer *error)
    (handler-dispatch *error)
    (g_rc_parse_user *toplevel %null-pointer *error)
    (handler-dispatch *error)
    (g_rc_parse_local *toplevel %null-pointer %null-pointer *error)
    (handler-dispatch *error)
    ;; Next application-specific rcname.
    (unless (null-pointer? *rcname)
      (parse-system-rc *toplevel *rcname *error)
      (handler-dispatch *error)
      (g_rc_parse_user *toplevel *rcname *error)
      (handler-dispatch *error)
      (g_rc_parse_local *toplevel *rcname %null-pointer *error)
      (handler-dispatch *error))))


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
