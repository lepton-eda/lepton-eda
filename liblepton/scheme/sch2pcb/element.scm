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


(define (pkg-line->element line)
  (define (args->element args)
    (let ((*element (pcb_element_new))
          (description (fix-spaces (list-ref args 0)))
          (refdes (fix-spaces (list-ref args 1)))
          (value (fix-spaces (list-ref args 2))))
      (set-pcb-element-description! *element description)
      (set-pcb-element-refdes! *element refdes)
      (let* ((right-paren-index (string-index value #\)))
             (new-value (if right-paren-index
                            (string-take value right-paren-index)
                            value)))
        (set-pcb-element-value! *element new-value)
        (let* ((extra-args-count (- (length args) 3))
               (dashes-count (string-count
                              (pcb-element-description *element) #\-))
               ;; Assume there was a comma in the value, for
               ;; instance "1K, 1%".
               (value-has-comma? (= extra-args-count (1+ dashes-count))))
          (when value-has-comma?
            (let ((revamped-value (string-append (pcb-element-value *element)
                                                 ","
                                                 (fix-spaces (list-ref args 3)))))
              (set-pcb-element-value!
               *element
               (let ((right-paren-index (string-index revamped-value #\))))
                 ;; Drop anything at right starting with a closing
                 ;; paren.
                 (if right-paren-index
                     (string-take revamped-value right-paren-index)
                     revamped-value)))))
          (let ((n (if value-has-comma? 4 3)))
            (pcb_element_pkg_to_element *element
                                        (string->pointer line)
                                        n))))))

  (if (not (string-prefix? "PKG_" line))
      %null-pointer
      (let ((left-paren-index (string-index line #\()))
        (if (not left-paren-index)
            %null-pointer
            ;; Get the contents of the string after the left paren
            ;; and check how many arguments it contains by
            ;; splitting it up by commas.
            (let* ((args-line (string-drop line (1+ left-paren-index)))
                   (args (string-split args-line #\,)))
              (if (< (length args) 3)
                  (begin
                    (format-warning "Bad package line: ~A\n" line)
                    %null-pointer)
                  (args->element args)))))))


(define (free-element *element)
  (pcb_element_free *element))
