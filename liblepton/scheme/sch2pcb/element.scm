;;; Lepton EDA Schematic to PCB conversion
;;; Scheme API
;;; Copyright (C) 2023-2024 Lepton EDA Contributors
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

(define-module (sch2pcb element)
  #:use-module (system foreign)

  #:use-module (lepton ffi boolean)
  #:use-module (lepton ffi sch2pcb)
  #:use-module (sch2pcb format)

  #:export (free-element
            pcb-element-description
            set-pcb-element-description!
            pcb-element-omit-pkg
            set-pcb-element-omit-pkg!
            pcb-element-pkg-name-fix
            set-pcb-element-pkg-name-fix!
            pcb-element-refdes
            set-pcb-element-refdes!
            pcb-element-value
            set-pcb-element-value!
            pkg-line->element))


(define (pcb-element-get-string *element *c-getter)
  (define *s (*c-getter *element))
  (if (null-pointer? *s)
      ;; What should the function return if *s is NULL?
      "<null>"
      (pointer->string *s)))

(define (pcb-element-set-string! *element *c-setter s)
  (define *s (if s (string->pointer s) %null-pointer))
  (*c-setter *element *s))

(define (pcb-element-description *element)
  (pcb-element-get-string *element pcb_element_get_description))

(define (set-pcb-element-description! *element description)
  (pcb-element-set-string! *element
                           pcb_element_set_description
                           description))

(define (pcb-element-refdes *element)
  (pcb-element-get-string *element pcb_element_get_refdes))

(define (set-pcb-element-refdes! *element refdes)
  (pcb-element-set-string! *element
                           pcb_element_set_refdes
                           refdes))

(define (pcb-element-value *element)
  (pcb-element-get-string *element pcb_element_get_value))

(define (set-pcb-element-value! *element value)
  (pcb-element-set-string! *element
                           pcb_element_set_value
                           value))

(define (pcb-element-omit-pkg *element)
  (true? (pcb_element_get_omit_PKG *element)))

(define (set-pcb-element-omit-pkg! *element omit-pkg?)
  (pcb_element_set_omit_PKG *element
                            (if omit-pkg? TRUE FALSE)))

(define (pcb-element-pkg-name-fix *element)
  (pcb-element-get-string *element pcb_element_get_pkg_name_fix))

(define (set-pcb-element-pkg-name-fix! *element name-fix)
  (pcb-element-set-string! *element
                           pcb_element_set_pkg_name_fix
                           name-fix))


(define (fix-spaces str)
  (define (fix c)
    (if (or (eq? c #\space) (eq? c #\tab)) #\_ c))

  (string-map fix str))


;;; The netlister backend gnet-gsch2pcb.scm generates PKG_ lines:
;;;
;;;   PKG_footprint(footprint{-fp0-fp1},refdes,value{,fp0,fp1})
;;;
;;; where fp1 and fp2 (if they exist) are the extra footprint
;;; components when specifying footprints like "DIL 14 300".  This
;;; is needed for m4 macros.
;;;
;;; A complication is if the footprint references a file element
;;; with spaces embedded in the name.  The netlister backend will
;;; interpret these as fp0, fp1, ... args and the footprint will
;;; in this case incorrectly have '-' inserted where the spaces
;;; should be.  So, if there are additional args, reconstruct the
;;; portion of the name given by the args with spaces for later
;;; use.  Eg. if the footprint is "100 Pin jack", we will have
;;;
;;;   PKG_100-Pin-jack(100-Pin-jack,refdes,value,Pin,jack)
;;;
;;; So put "Pin jack" into pkg_name_fix so if this element is
;;; searched as a file element we can munge the description to
;;; what it should be, e.g.:
;;;
;;;      100-Pin-jack -> 100 Pin jack
;;;
(define (pkg-line->element line)
  ;; Drop the part of string S starting with the first found
  ;; right paren.
  (define (trim-after-right-paren s)
    (let ((right-paren-index (string-index s #\))))
      (if right-paren-index
          (string-take s right-paren-index)
          s)))

  ;; If the component value has a comma, e.g. "1k, 1%", the
  ;; netlister generated PKG line will be
  ;;
  ;;   PKG_XXX(`R0w8',`R100',`1k, 1%'),
  ;;
  ;; but after processed by m4, the input to lepton-sch2pcb will
  ;; be
  ;;
  ;;   PKG_XXX(R0w8,R100,1k, 1%).
  ;;
  ;; So the quoting info has been lost when processing for file
  ;; elements.  So here try to detect and fix this.  But I can't
  ;; handle the situation where the description has a '-' and the
  ;; value has a comma because gnet-gsch2pcb.scm munges the
  ;; description with '-' when there are extra args.
  ;;
  (define (args->element args)
    (let* ((*element (pcb_element_new))
           (description (fix-spaces (list-ref args 0)))
           (refdes (fix-spaces (list-ref args 1)))
           (value (trim-after-right-paren (fix-spaces (list-ref args 2))))
           (extra-args-count (- (length args) 3))
           (dashes-count (string-count description #\-))
           ;; Assume there was a comma in the value, for
           ;; instance "1K, 1%".
           (value-has-comma? (= extra-args-count (1+ dashes-count)))
           (revamped-value
            (if value-has-comma?
                (trim-after-right-paren
                 (string-append value
                                ","
                                (fix-spaces (list-ref args 3))))
                value)))
      (set-pcb-element-description! *element description)
      (set-pcb-element-refdes! *element refdes)
      (set-pcb-element-value! *element revamped-value)
      (let* ((n (if value-has-comma? 4 3))
             (fix-args (list-tail args n)))
        (unless (null? fix-args)
          (set-pcb-element-pkg-name-fix!
           *element
           ;; This seems to be superfluous here.
           (trim-after-right-paren (string-join fix-args))))
        *element)))

  (if (string-prefix? "PKG_" line)
      (let ((left-paren-index (string-index line #\()))
        (if left-paren-index
            ;; Get the contents of the string after the left paren
            ;; and check how many arguments it contains by
            ;; splitting it up by commas.
            (let* ((args-line (string-drop line (1+ left-paren-index)))
                   (args (string-split args-line #\,)))
              (if (< (length args) 3)
                  (begin
                    (format-warning "Bad package line: ~A\n" line)
                    %null-pointer)
                  (args->element args)))
            %null-pointer))
      %null-pointer))


(define (free-element *element)
  (pcb_element_free *element))
