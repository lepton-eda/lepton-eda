;; Lepton EDA Schematic Capture
;; Scheme API
;; Copyright (C) 2011 Peter Brett
;; Copyright (C) 2017-2022 Lepton EDA Contributors
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

(define-module (schematic attrib)
  #:use-module (system foreign)

  #:use-module (lepton config)
  #:use-module (lepton log)
  #:use-module (schematic core attrib)
  #:use-module (schematic core gettext)
  #:use-module (lepton ffi)

  #:export (attribute-name
            init-schematic-attribs!))

;; add-attrib! target name value visible attribute-mode
;;
;; Create a new attribute, either attached to a target object in the
;; current page, or floating in the current page if target is #f.  The
;; name and value for the attribute must be strings, and if visible is
;; #f, the attribute will be invisible.  The attribute-mode controls
;; which parts of the attribute will be visible, and must be one of
;; the following symbols:
;;
;;   name
;;   value
;;   both
;;
;; See also active-page in the (schematic window) module.
(define-public add-attrib! %add-attrib!)


(define (attribute-name name)
  "Adds attribute NAME to the list of attributes shown in the
\"Add attribute\" and \"Edit attribute\" dialogs.  Returns #t if
the attribute was added successfully, otherwise returns #f."
  (define proc-name "attribute-name: ")

  (define (warning-empty-string)
    (string-append "WARNING: "
                   proc-name
                   (G_ "The argument cannot be an empty string.")))

  (define (warning-already-added)
    (string-append "WARNING: "
                   proc-name
                   (G_ "Attribute has been already added: ")
                   (format #f "~S" name)))

  (define (warning-failed-to-add)
    (string-append "WARNING: "
                   proc-name
                   (G_ "Failed to add attribute: ")
                   (format #f "~S" name)))

  (define (error-not-string)
    (string-append "ERROR: "
                   proc-name
                   (G_ "The argument must be a string.")))

  (define (warn-if-error err msg)
    (or (not err)
        (begin (log! 'warning "~A" msg) #f)))

  (catch 'wrong-type-arg
    (lambda ()
      (and (warn-if-error (string-null? name)
                          (warning-empty-string))

           (let ((pname (string->pointer name)))
             (and (warn-if-error (zero? (s_attrib_uniq pname))
                                 (warning-already-added))
                  (warn-if-error (zero? (s_attrib_add_entry pname))
                                 (warning-failed-to-add))))))
    (lambda (key . args)
      (begin (log! 'critical "~A" (error-not-string))
             #f))))

;;; Attribute list used in "Edit attribute" and "Multiattrib"
;;; dialogs.  Initially, it is set to #f which means it is not
;;; initialized.
(define %schematic-attribs #f)

;;; Get attributes from config.
(define (schematic-attribs-from-config)
  (let* ((group "schematic.attrib")
         (sym-attribs-key "symbol-attribs")
         (pin-attribs-key "pin-attribs")
         (cfg (path-config-context (getcwd))))
    (if (config-has-group? cfg group)
        (append (if (config-has-key? cfg group sym-attribs-key)
                    (config-string-list cfg group sym-attribs-key)
                    '())
                (if (config-has-key? cfg group pin-attribs-key)
                    (config-string-list cfg group pin-attribs-key)
                    '())))))

(define (init-schematic-attribs!)
  "Initializes the list of attributes used in attribute dialogs
from path configuration context."
  (when (not %schematic-attribs)
    ;; Init %schematic-attribs once.
    (set! %schematic-attribs
          (filter attribute-name (schematic-attribs-from-config))))
  %schematic-attribs)
