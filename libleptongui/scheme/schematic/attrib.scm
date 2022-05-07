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
  #:use-module (lepton ffi)
  #:use-module (lepton log)
  #:use-module (lepton object)
  #:use-module (lepton object foreign)
  #:use-module (lepton object text)
  #:use-module (lepton page)

  #:use-module (schematic core gettext)
  #:use-module (schematic ffi)
  #:use-module (schematic window)

  #:export (add-attrib!
            attribute-name
            init-schematic-attribs!))

(define-syntax-rule (check-attrib-target val pos)
  (let ((proc-name (frame-procedure-name (stack-ref (make-stack #t) 1))))
    (if (object? val)
        (unless (eq? (object-page val) (active-page))
          (scm-error 'object-state
                     proc-name
                     (G_ "Object ~A is not included in the current lepton-schematic page.")
                     (list val)
                     '()))
        (unless (or (not val)
                    (page? val))
          (scm-error 'wrong-type-arg
                     proc-name
                     "Wrong type argument in position ~A (expecting page, object, or #f): ~A"
                     (list pos val)
                     #f)))))

;;; FIXME: This function does not verify that NAME is actually a
;;; valid attribute name.
;;; TODO: It would be nice to support pages other than the current
;;; active page.
(define (add-attrib! target name value visible show)
  "Creates and returns a new attribute object, either attached to
an object or floating, with given NAME and VALUE which should be
strings.  If VISIBLE is #f, the new attribute will be invisible;
otherwise it will be visible.  SHOW determines which parts of an
attribute-formatted string should be shown, and should be one of
the symbols 'name, 'value or 'both.  If TARGET is specified and is
a Lepton object, the new attribute will be attached to it. If the
object is not in lepton-schematic's active page, an 'object-state
error will be raised.  If TARGET is #f, the new attribute will be
floating in lepton-schematic's current active page.  The initial
value of the attribute will be set then to (0 . 0).  See also
active-page() in the (schematic window) module."
  (check-attrib-target target 1)
  (check-string name 2)
  (check-string value 3)
  (check-boolean visible 4)
  (check-text-show show 5)

  (let ((*object (if (object? target)
                     (geda-object->pointer target)
                     %null-pointer))
        (visibility (text-visibility->integer visible))
        (show? (symbol->text-attribute-show-mode show))
        (*str (string->pointer (string-append name "=" value))))
    (pointer->geda-object (o_attrib_add_attrib (current-window)
                                               *str
                                               visibility
                                               show?
                                               *object
                                               TRUE  ; let's be
                                               0     ; specific
                                               0))))


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
