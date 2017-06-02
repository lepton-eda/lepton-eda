;;; Lepton EDA netlister
;;; Copyright (C) 2017 Lepton EDA Contributors
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
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;;; MA 02111-1301 USA.

(define-module (gnetlist net)
  #:use-module (gnetlist config)
  #:export (create-netattrib
            create-netname
            netattrib-netname
            netattrib-pinnum-get-connected-string
            netattrib-connected-string-get-pinnum
            netattrib-check-connected-string))

(define (create-netattrib basename hierarchy-tag)
  (define mangle? (gnetlist-config-ref 'mangle-net))
  (define reverse-order?  (gnetlist-config-ref 'reverse-net-order))
  (define separator (gnetlist-config-ref 'net-separator))

  (if (and hierarchy-tag mangle? basename)
      (if reverse-order?
          (string-append basename (or separator "") hierarchy-tag)
          (string-append hierarchy-tag (or separator "") basename))
      basename))

(define (create-netname basename hierarchy-tag)
  (define mangle? (gnetlist-config-ref 'mangle-netname))
  (define reverse-order?  (gnetlist-config-ref 'reverse-netname-order))
  (define separator (gnetlist-config-ref 'netname-separator))

  (if (and hierarchy-tag mangle? basename)
      (if reverse-order?
          (string-append basename (or separator "") hierarchy-tag)
          (string-append hierarchy-tag (or separator "") basename))
      basename))

(define (netattrib-netname s)
  (and s
       (let ((colon-position (string-index s #\:)))
         (if colon-position
             (string-take s colon-position)
             (begin
               (log! 'critical (_ "Invalid attribute (missing ':'): net=~A" s))
               #f)))))

(define %pin-net-prefix "__netattrib_power_pin ")

(define (netattrib-pinnum-get-connected-string pinnum)
  (string-append %pin-net-prefix pinnum))

(define (netattrib-connected-string-get-pinnum s)
  (and (string-prefix? %pin-net-prefix s)
       (string-drop s (string-length %pin-net-prefix))))

(define (netattrib-check-connected-string s)
  (when (netattrib-connected-string-get-pinnum s)
    (log! 'error
          (_ "Name ~S is reserved for internal use.")
          %pin-net-prefix)))
