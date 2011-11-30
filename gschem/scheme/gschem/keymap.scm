;; gEDA - GPL Electronic Design Automation
;; gschem - gEDA Schematic Capture - Scheme API
;; Copyright (C) 2011 Peter Brett <peter@peter-b.co.uk>
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA
;;

(define-module (gschem keymap)
  #:use-module (gschem core keymap))

;; -------------------- Key combinations --------------------

(define-public key? %key?)

(define-public key->string %key->string)

(define-public key->display-string %key->display-string)

(define-public (string->key str)
  (or (%string->key str)
      (scm-error 'key-format #f
                 "~S is not a valid key combination." (list str) #f)))

;; -------------------- Key sequences --------------------

(define-public (keys? obj)
  (and (vector? obj)
       (> (vector-length obj) 0)
       (call/cc
        (lambda (return)
          (array-for-each
           (lambda (x) (or (key? x) (return #f)))
           obj)))))

(define-public (keys->string keys)
  (string-join (map key->string (vector->list keys)) " "))

(define-public (string->keys str)
  (list->vector (map string->key
                     (filter! (lambda (x) (not (string-null? x)))
                              (string-split str #\space)))))

(define-public (keys->display-string keys)
  (string-join (map key->display-string (vector->list keys)) " "))
