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
  #:use-module (srfi srfi-1)
  #:use-module (geda attrib)
  #:use-module (geda log)
  #:use-module (geda object)
  #:use-module (gnetlist config)
  #:use-module (gnetlist core gettext)

  #:export (create-netattrib
            create-netname
            netattrib-netname
            netattrib-pinnum-get-connected-string
            netattrib-connected-string-get-pinnum
            netattrib-check-connected-string
            netattrib-search-net))

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

(define (blame-missing-colon net-attrib-value)
  (log! 'critical
        (_ "Invalid attribute (missing ':'): net=~A")
        net-attrib-value)
  #f)

(define (netattrib-netname s)
  (and s
       (let ((colon-position (string-index s #\:)))
         (if colon-position
             (string-take s colon-position)
             (blame-missing-colon s)))))

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

(define %delimiters (string->char-set ",; "))

(define (netattrib-search-net object wanted-pin)
  (define (search-pin value)
    (let ((colon-position (string-index value #\:)))
      (if colon-position
          (let ((net-name (netattrib-netname value))
                (pin-part (string-drop value (1+ colon-position))))
            (let loop ((pin-list (string-split pin-part %delimiters)))
              (and (not (null? pin-list))
                   (if (string=? (car pin-list) wanted-pin)
                       net-name
                       (loop (cdr pin-list))))))
          (blame-missing-colon value))))

  (define (search-in-values ls)
    (and (not (null? ls))
         (or (search-pin (car ls))
             (search-in-values (cdr ls)))))

  (define (net-values attribs)
    (define (net-value attrib)
      (and (string=? (attrib-name attrib) "net")
           (attrib-value attrib)))
    (filter-map net-value attribs))

  (and object
       (component? object)
       ;; first look inside the component (for backward compatibility)
       (let ((inherited (search-in-values (net-values (inherited-attribs object)))))
             (or
              ;; now look outside the component
              (search-in-values (net-values (object-attribs object)))
              inherited))))
