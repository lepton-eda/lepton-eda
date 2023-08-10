;;; Lepton EDA schematic -> pcb conversion utility
;;;
;;; Copyright (C) 2022-2025 Lepton EDA Contributors
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

(define-module (sch2pcb lookup)
  #:use-module (system foreign)

  #:use-module (lepton ffi boolean)
  #:use-module (lepton ffi sch2pcb)
  #:use-module (lepton file-system)
  #:use-module (sch2pcb format)

  #:export (lookup-footprint))


(define (lookup-footprint path name)
  "Searches for a Pcb element (footprint) file by NAME in PATH.
If an element is found, returns a pointer to its C string name,
otherwise returns %null-pointer."
  (define (opendir-protected path)
    (catch #t
      (lambda () (opendir path))
      (lambda (key subr message args rest)
        (format-error (string-append "Could not open directory ~S: "
                                     (format #f "~?" message args))
                      (list path))
        #f)))

  ;; readdir() reads filenames from directory stream until #<eof>
  ;; object is found.  We don't process the dirs "." or ".." as it
  ;; leads to infinite recursive invocations.
  (define (readdir* dir)
    (let ((path (readdir dir)))
      (and (not (eof-object? path))
           (if (or (string=? path ".")
                   (string=? path ".."))
               (readdir* dir)
               path))))

  (define (found-filename? name element-name)
    (let ((found? (or (string= name element-name)
                      (string= (string-append element-name ".fp")
                               name))))
      (extra-verbose-format "\t           : ~A\t~A\n"
                            name
                            (if found? "Yes\n" "No\n"))
      found?))

  (define (process-directory dir-path element-name dir)
    (let loop ((name (readdir* dir)))
      (and name
           (or (let ((path (string-append dir-path
                                          file-name-separator-string
                                          name)))
                 (if (directory? path)
                     ;; If we got a directory name, then recurse down into it.
                     (process-path path element-name)

                     ;; Otherwise assume it is a file and see if
                     ;; it is the one we want.
                     (and (found-filename? name element-name)
                          path)))

               (loop (readdir* dir))))))

  (define (process-path path name)
    (let ((dir (opendir-protected path)))
      (and dir
           (begin
             (extra-verbose-format "\t  Searching: ~S for ~S\n"
                                   path
                                   name)
             (let ((result (process-directory path name dir)))
               (closedir dir)
               result)))))

  (process-path path name))
