;;; Lepton EDA library - Scheme API
;;; Copyright (C) 2007-2016 gEDA Contributors
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

;;; File system related procedures.

(define-module (lepton file-system)

  #:export (regular-file?
            directory?
            file-readable?))

(define (regular-file? path)
  "Returns #t if the given path exists and is a regular file,
otherwise #f.  Symlinks to regular files are considered regular
files as well."
  (and (file-exists? path)
       ;; Use stat() here instead of lstat() to let symlinks to
       ;; regular files be treated as plain regular files.
       (eqv? (stat:type (stat path)) 'regular)))

(define (directory? path)
  "Returns #t if the given path exists and is a directory file, otherwise #f."
  (and (file-exists? path)
       (eqv? (stat:type (stat path)) 'directory)))

(define (file-readable? filename)
  "Returns #t if file FILENAME is readable with current user's
permissions, otherwise returns #f."
  (let ((uid (getuid))
        (gid (getgid))
        (st (false-if-exception (stat filename))))
    (and st
         (let* ((perms (stat:perms st))
                (perms-bit-set? (lambda (mask)
                                  (not (= 0 (logand mask perms))))))
           (or (zero? uid)
               (and (= uid (stat:uid st))
                    (perms-bit-set? #o400))
               (and (= gid (stat:gid st))
                    (perms-bit-set? #o040))
               (perms-bit-set? #o004))))))
