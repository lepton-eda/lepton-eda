;; gEDA - GPL Electronic Design Automation
;; gschem - gEDA Schematic Capture - Scheme API
;; Copyright (C) 2010-2011 Peter Brett
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

;; This file contains deprecated Scheme API features, which should not
;; be used in new code.

(define-module (gschem deprecated)

  #:use-module (geda page)
  #:use-module (geda object)
  #:use-module (geda attrib)
  #:use-module (gschem window)
  #:use-module (gschem hook)
  #:use-module (gschem selection)
  #:use-module (gschem attrib))

;; add-attribute-to-object object name value visible show
;;
;; Add an attribute "name=value" to object.  If visible is #f, the new
;; attribute will be invisible.  show should be a list containing one
;; or both of the strings "name and "value" (if neither is specified,
;; both are assumed).
;;
;; See also add-attrib! in the (gschem attrib) module.
(define-public (add-attribute-to-object object name value visible show)
  (add-attrib! object name value visible
               (let ((n (member "name" show))
                     (v (member "value" show)))
                 (cond
                  ((and n v) 'both)
                  (n 'name)
                  (v 'value)
                  (else 'both))))) ;; Default

;; set-attribute-value! attrib value
;;
;; Set the value part of the text object attrib.
(define-public (set-attribute-value! attrib value)
  (let ((params (text-info attrib))
        (name-value (attrib-parse attrib)))
    (list-set! params 3 (simple-format "~A=~A" (car name-value) value))
    (apply set-text! attrib params)))

;; get-objects-in-page page
;;
;; Get the contents of page, in reverse order.
(define-public (get-objects-in-page page)
  (reverse! (page-contents page)))

;; get-current-page
;;
;; Return the page which currently has focus in gschem.
(define-public get-current-page active-page)

;; get-object-pins object
;;
;; Return the pin objects from a component's contents, in reverse
;; order, or the empty list if object is not a component.
(define-public (get-object-pins object)
  (if (component? object)
      (reverse! (filter! pin? (component-contents object)))
      '()))

;; get-pin-ends pin
;;
;; Return the coordinates of the endpoints of a pin, in the format:
;;
;;   ((x1 . y1) x2 . y2)
(define-public (get-pin-ends pin)
  (let ((params (line-info pin)))
    (cons (list-ref params 0) (list-ref params 1))))

;; get-selected-filename
;;
;; Returns the filename associated with the active page in the current
;; gschem window.
(define-public (get-selected-filename)
  (page-filename (active-page)))

;;;; Old-style hooks

;; Adds a function to src-hook.  The function is called with a single
;; argument, lst, which should be a list of objects.  For each member
;; of lst which matches filter?, the function calls tgt-hook with that
;; object as the argument.
(define (add-hook!/filter src-hook tgt-hook filter?)
  (add-hook! src-hook
    (lambda (lst)
      (if (not (hook-empty? tgt-hook))
          (for-each
           (lambda (obj)
             (if (filter? obj)
                 (run-hook tgt-hook obj)))
           lst)))))

;; Adds a function to src-hook. The function is called with a single
;; argument, lst, which should be a list of objects.  For each member
;; of lst which matches filter?, the function calls tgt-hook with a
;; list of all attributes (attached and inherited) of that object.
(define (add-hook!/full-attribs src-hook tgt-hook filter?)
  (add-hook! src-hook
    (lambda (lst)
      (if (not (hook-empty? tgt-hook))
          (for-each
           (lambda (obj)
             (if (filter? obj)
                 (run-hook tgt-hook
                           (append! (object-attribs obj)
                                    (inherited-attribs obj)))))
           lst)))))

;; add-component-hook:
;;
;; Called when a new component is added to the page (not copied etc).
;; Argument is a list of all attributes (inherited & promoted) of the
;; component.  Differs from the classic behaviour in that it is *not*
;; also called once for each promoted attribute with the empty list as
;; the argument.
(define-public add-component-hook (make-hook 1))
(add-hook!/full-attribs add-objects-hook add-component-hook component?)

;; add-component-object-hook:
;;
;; Called when a new component is added to the page (not copied etc).
;; Called once with the component itself as the argument, and once for
;; each promoted attribute, with the attribute as the argument.
(define-public add-component-object-hook (make-hook 1))
(add-hook! add-objects-hook
 (lambda (lst)
   (if (not (hook-empty? add-component-object-hook))
       (for-each
        (lambda (obj)
          (define (run x) (run-hook add-component-object-hook x))
          (if (component? obj)
              (begin (run obj) (for-each run (object-attribs obj)))))
        lst))))

;; add-attribute-hook
;;
;; Called each time an attribute is added to something.  Argument is
;; the thing that had an attribute added.  The behaviour here emulates
;; the classic behaviour as closely as possible -- it doesn't run the
;; hook on explicit attribute attachment operations (via
;; "Attributes->Attach"), but does run when an individual attribute is
;; created and simultaneously attached to something.
(define-public add-attribute-hook (make-hook 1))
(add-hook! add-objects-hook
  (lambda (lst)
    (if (and (not (hook-empty? add-attribute-hook)) (= 1 (length lst)))
        (let* ((attrib (car lst))
               (target (attrib-attachment attrib)))
          (if (and (attribute? attrib) target)
              (run-hook add-attribute-hook target))))))

;; add-pin-hook
;;
;; Called each time a pin is added to the schematic.  Argument is the
;; pin itself.
(define-public add-pin-hook (make-hook 1))
(add-hook!/filter add-objects-hook add-pin-hook pin?)

;; mirror-component-object-hook
;;
;; Called for each component in the selection when a mirror operation
;; is carried out.  The argument is the component itself.
(define-public mirror-component-object-hook (make-hook 1))
(add-hook!/filter mirror-objects-hook mirror-component-object-hook component?)

;; mirror-pin-hook
;;
;; Same as mirror-component-object-hook, but for pins.
(define-public mirror-pin-hook (make-hook 1))
(add-hook!/filter mirror-objects-hook mirror-pin-hook pin?)

;; rotate-component-object-hook
;;
;; Called for each component in the selection when a rotate operation
;; is carried out (including using the middle mouse button during a
;; move operation, but excluding rotations during component
;; placement).  The argument is the component itself.
(define-public rotate-component-object-hook (make-hook 1))
(add-hook!/filter rotate-objects-hook rotate-component-object-hook component?)

;; rotate-pin-hook
;;
;; Same as rotate-component-object-hook, but for pins.
(define-public rotate-pin-hook (make-hook 1))
(add-hook!/filter rotate-objects-hook rotate-pin-hook pin?)

;; copy-component-hook:
;;
;; Called each time a component is copied into the schematic.
;; Argument is a list off all attributes (inherited & promoted) of the
;; component.  Differs from classic behaviour in that it is called on
;; pasting from buffers and the clipboard, in addition to "Edit->Copy
;; Mode" and "Edit->Multiple Copy Mode".
(define-public copy-component-hook (make-hook 1))
(add-hook!/full-attribs paste-objects-hook copy-component-hook component?)

;; move-component-hook:
;;
;; Called each time a component is moved in the schematic.
;; Argument is as copy-component-hook.
(define-public move-component-hook (make-hook 1))
(add-hook!/full-attribs move-objects-hook move-component-hook component?)

;; deselect-component-hook:
;;
;; Called each time a component is removed from the selection,
;; except if the selection is cleared entirely.  Argument is
;; as select-component-hook.
(define-public deselect-component-hook (make-hook 1))
(add-hook!/full-attribs deselect-objects-hook deselect-component-hook
                        component?)

;; deselect-net-hook:
;;
;; Called each time a net segment (n.b. *not* bus segment) is added to
;; the selection. Argument is a list of all attributes of the
;; net.
(define-public deselect-net-hook (make-hook 1))
(add-hook!/full-attribs deselect-objects-hook deselect-net-hook net?)

;; deselect-all-hook:
;;
;; Called with the empty list as the argument each time the
;; selection is emptied, even if the selection is already
;; empty.
(define-public deselect-all-hook (make-hook 1))
(add-hook! deselect-objects-hook
  (lambda (arg)
    (if (and (not (null? deselect-all-hook))
             (null? (page-selection (active-page))))
        (run-hook deselect-all-hook '()))))

;; select-component-hook:
;;
;; Called each time a component is added to the selection.
;; Argument is a list of all attributes (inherited & promoted)
;; of the component.
(define-public select-component-hook (make-hook 1))
(add-hook!/full-attribs select-objects-hook select-component-hook
                        component?)

;; select-net-hook:
;;
;; Called each time a net segment (n.b. *not* bus segment) is
;; added to the selection.  Argument is the empty list.
(define-public select-net-hook (make-hook 1))
(add-hook!/full-attribs select-objects-hook select-net-hook net?)
