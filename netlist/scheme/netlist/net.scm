;;; Lepton EDA netlister
;;; Copyright (C) 2017-2018 Lepton EDA Contributors
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

(define-module (netlist net)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (geda attrib)
  #:use-module (geda log)
  #:use-module (geda object)
  #:use-module (netlist config)
  #:use-module (netlist core gettext)
  #:use-module (netlist hierarchy)
  #:use-module (symbol check net-attrib)
  #:use-module (symbol check duplicate)

  #:export (create-netattrib
            create-netname
            netattrib-search-net
            check-net-maps
            attrib-value-by-name))

(define (hierarchy-tag->string hierarchy-tag separator reverse-order?)
  (match hierarchy-tag
    ;; Hierarchy tag is already reversed, so we have to reverse it
    ;; again if no reverse-order? is set.
    ((? list? tag) (string-join (if reverse-order? tag (reverse tag))
                                (or separator "")))
    (tag tag)))

(define (create-netattrib basename hierarchy-tag)
  (define mangle? (gnetlist-config-ref 'mangle-net))
  (define reverse-order?  (gnetlist-config-ref 'reverse-net-order))
  (define separator (gnetlist-config-ref 'net-separator))

  (let ((hierarchy-tag (hierarchy-tag->string hierarchy-tag
                                              separator
                                              reverse-order?)))
   (if (and hierarchy-tag mangle? basename)
       (if reverse-order?
           (string-append basename (or separator "") hierarchy-tag)
           (string-append hierarchy-tag (or separator "") basename))
       basename)))

(define (create-netname basename hierarchy-tag)
  (define mangle? (gnetlist-config-ref 'mangle-netname))
  (define reverse-order?  (gnetlist-config-ref 'reverse-netname-order))
  (define separator (gnetlist-config-ref 'netname-separator))

  (let ((hierarchy-tag (hierarchy-tag->string hierarchy-tag
                                              separator
                                              reverse-order?)))
   (if (and hierarchy-tag mangle? basename)
       (if reverse-order?
           (string-append basename (or separator "") hierarchy-tag)
           (string-append hierarchy-tag (or separator "") basename))
       basename)))

(define (attrib-value-by-name object name)
  (define (has-appropriate-name? attrib)
    (string=? (attrib-name attrib) name))

  (define (first* ls)
    (and (not (null? ls)) (car ls)))

  (first* (map attrib-value
               (filter has-appropriate-name? (object-attribs object)))))


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

;;; Checks for duplicate pinnumbers in NET-MAPS.
(define (check-duplicates/net-maps-override net-maps)
  (define (net-map=? a b)
    (string=? (net-map-pinnumber a) (net-map-pinnumber b)))

  (define (net-map<? a b)
    (string<? (net-map-pinnumber a) (net-map-pinnumber b)))

  (define (blame-duplicate a b)
    (log! 'message
          (_ "Attached net ~A:~A overrides inherited net ~A:~A")
          (net-map-netname a)
          (net-map-pinnumber a)
          (net-map-netname b)
          (net-map-pinnumber b)))

  ;; Here lists may contain no more than two objects, one from
  ;; attached net-map and one from inherited one, since the
  ;; net-maps already contain unique elements. Attached net-maps
  ;; must go first to make this work properly. See
  ;; check-net-maps() below, which ensures this.
  (define (blame-if-list ls)
    (if (list? ls)
        (begin (blame-duplicate (first ls) (second ls))
               (car ls))
        ls))

  (map blame-if-list
       (list->duplicate-list net-maps net-map<? net-map=?)))

(define (check-net-maps object)
  "Makes net-maps for inherited and attached net= attributes of
OBJECT, checks them for duplicates and transforms into one net-map
list with unique elements."
  (define (net-attrib? attrib)
    (string=? "net" (attrib-name attrib)))

  (let ((attached-net-maps (make-net-maps (filter net-attrib? (object-attribs object))))
        (inherited-net-maps (make-net-maps (filter net-attrib? (inherited-attribs object)))))
    (check-duplicates/net-maps-override (append attached-net-maps inherited-net-maps))))
