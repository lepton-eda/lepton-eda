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

  (define (opendir path)
    (sch2pcb_find_element_open_dir (string->pointer path)))

  (define (readdir *dir)
    (let ((name (sch2pcb_find_element_read_name *dir)))
      (and (not (null-pointer? name))
           (pointer->string name))))

  (define (find-element path element-name name process-func dir?)
    (if dir?
        ;; If we got a directory name, then recurse down into it.
        (let ((*next-dir (opendir path)))
          (and (not (null-pointer? *next-dir))
               (begin
                 (extra-verbose-format "\t  Searching: ~S for ~S\n"
                                       path
                                       element-name)
                 (let ((found (process-func path
                                            element-name
                                            *next-dir)))
                   (sch2pcb_find_element_close_dir *next-dir)
                   found))))

        ;; Otherwise assume it is a file and see if it is the one
        ;; we want.
        (begin
          (extra-verbose-format "\t           : ~A\t" name)
          (let ((found
                 (if (string= name element-name)
                     path
                     (and (string= (string-append element-name ".fp")
                                   name)
                          path))))
            (extra-verbose-format (if found
                                      "Yes\n"
                                      "No\n"))
            found))))

  (define (process-directory dir-path element-name *dir)
    (let loop ((name (readdir *dir)))
      (and name
           (let* ((path (string-append dir-path
                                       file-name-separator-string
                                       name))
                  (found (find-element path
                                       element-name
                                       name
                                       process-directory
                                       (directory? path))))
             (or found
                 (loop (readdir *dir)))))))

  (let ((*dir (opendir path)))
    (and (not (null-pointer? *dir))
         (begin
           (extra-verbose-format "\t  Searching: ~S for ~S\n"
                                 path
                                 name)
           (let ((result (process-directory path name *dir)))
             (sch2pcb_find_element_close_dir *dir)
             result)))))
