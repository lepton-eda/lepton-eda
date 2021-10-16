;;; Lepton EDA library - Scheme API
;;; Copyright (C) 2010 Peter Brett
;;; Copyright (C) 2010-2014 gEDA Contributors
;;; Copyright (C) 2017-2021 Lepton EDA Contributors
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
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
;;

;; This file contains deprecated Scheme API features, which should not
;; be used in new code.

(define-module (geda deprecated)
 #:use-module (rnrs bytevectors)
 #:use-module (system foreign)

 ;; Import C procedures
 #:use-module (lepton core gettext)
 #:use-module (lepton core rc)

 #:use-module (lepton attrib)
 #:use-module (lepton ffi)
 #:use-module (lepton log)
 #:use-module (lepton object)
 #:use-module (lepton object type)
 #:use-module (lepton os)
 #:use-module (lepton page)
 #:use-module (lepton rc)

 ;; Re-export procedures and variables from (lepton rc).
 #:re-export (build-path
              geda-data-path
              geda-rc-path
              load-scheme-dir
              load-rc-from-sys-config-dirs)

 #:export (deprecated-module-log-warning!
           scheme-directory))

(define* (deprecated-module-log-warning! #:optional (new-modname #f))
  (log! 'warning
        (G_ "The module ~S is deprecated. Please don't use it any more.~A")
        (module-name (current-module))
        (if new-modname
          (format #f
                  (G_ "\n  It has been replaced by the ~A module.")
                  new-modname)
          "")))

(define (lepton-var->char var-name)
  (integer->char
   (bytevector-uint-ref (pointer->bytevector (dynamic-pointer var-name
                                                              liblepton)
                                             (sizeof uint8))
                        0
                        (native-endianness)
                        (sizeof uint8))))

(define-public OBJ_LINE (lepton-var->char "_OBJ_LINE"))
(define-public OBJ_PATH (lepton-var->char "_OBJ_PATH"))
(define-public OBJ_BOX (lepton-var->char "_OBJ_BOX"))
(define-public OBJ_PICTURE (lepton-var->char "_OBJ_PICTURE"))
(define-public OBJ_CIRCLE (lepton-var->char "_OBJ_CIRCLE"))
(define-public OBJ_NET (lepton-var->char "_OBJ_NET"))
(define-public OBJ_BUS (lepton-var->char "_OBJ_BUS"))
(define-public OBJ_COMPLEX (lepton-var->char "_OBJ_COMPONENT"))
(define-public OBJ_TEXT (lepton-var->char "_OBJ_TEXT"))
(define-public OBJ_PIN (lepton-var->char "_OBJ_PIN"))
(define-public OBJ_ARC (lepton-var->char "_OBJ_ARC"))
(define-public OBJ_PLACEHOLDER (lepton-var->char "_OBJ_PLACEHOLDER"))


(define-public (get-line-width object)
  "Returns the line width used to draw OBJECT. Deprecated because it
doesn't respect type restrictions, unlike the object-stroke
function in (lepton object)."
  (define pointer (geda-object->pointer* object 1))

  (lepton_object_get_stroke_width pointer))

(define-public get-attribute-name-value parse-attrib)

(define-public get-attribute-angle text-angle)

(define-public (get-object-attributes object)
  (reverse! (object-attribs object)))

(define-public (get-attrib-value-by-attrib-name object name)
  (reverse!
   (filter!
    string?
    (map (lambda (x)
           (let ((name-value (false-if-exception (parse-attrib x))))
             (if (and name-value (string=? name (car name-value)))
                 (cdr name-value)
                 #f)))
         (object-attribs object)))))


(define-public (get-object-type object)
  (case (object-type object)
    ((arc) OBJ_ARC)
    ((box) OBJ_BOX)
    ((bus) OBJ_BUS)
    ((circle) OBJ_CIRCLE)
    ((complex) OBJ_COMPLEX)
    ((line) OBJ_LINE)
    ((net) OBJ_NET)
    ((path) OBJ_PATH)
    ((picture) OBJ_PICTURE)
    ((pin) OBJ_PIN)
    ((text) OBJ_TEXT)
    (else (error "Unknown object type ~A" (object-type object)))))

(define-public get-page-filename page-filename)

(define-public set-page-filename set-page-filename!)

(define-public (get-attribute-bounds object)
  ;; object-bounds returns ((left . top) . (right . bottom)).
  ;; Put in form ((left . right) . (top . bottom))
  (let* ((bounds (object-bounds object))
         (top (cdr (car bounds)))
         (right (car (cdr bounds))))
    (set-cdr! (car bounds) right)
    (set-car! (cdr bounds) top)
    bounds))

(define-public (calcule-new-attrib-bounds attrib align angle x y)
  (define align-table
    '(("Lower Left" . lower-left)
      ("Middle Left" . middle-left)
      ("Upper Left" . upper-left)
      ("Lower Middle" . lower-center)
      ("Middle Middle" . middle-center)
      ("Upper Middle" . upper-center)
      ("Lower Right" . lower-right)
      ("Middle Right" . middle-right)
      ("Upper Right" . upper-right)))

  (let* ((t (copy-object attrib))
         (params (text-info t))
         (location (car params)))

    (apply set-text! t
           (cons (if (= x -1) (car location) x) ; location
                 (if (= y -1) (cdr location) y))
           (or (assoc-ref align-table align) (text-align t)) ; alignment
           (if (= angle -1) (list-ref params 2) angle) ; angle
           (list-tail params 3)) ; other params

    ;; object-bounds returns ((left . top) . (right . bottom)).
    ;; Put in form ((left . right) . (bottom . top))
    (let* ((bounds (object-bounds t))
           (top (cdr (car bounds)))
           (bottom (cdr (cdr bounds)))
           (right (car (cdr bounds))))
      (set-cdr! (car bounds) right)
      (set-car! (cdr bounds) bottom)
      (set-cdr! (cdr bounds) top)
      bounds)))

(define (scheme-directory dir)
  (add-to-load-path (expand-env-variables dir)))

(define (enabled? x)
  (string= "enabled" x))
