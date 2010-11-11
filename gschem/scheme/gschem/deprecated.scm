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
  #:use-module (gschem hook))

(define-public (set-attribute-value! attrib value)
  (let ((params (text-info attrib))
        (name-value (attrib-parse attrib)))
    (list-set! params 3 (simple-format "~A=~A" (car name-value) value))
    (apply set-text! attrib params)))

(define-public (get-objects-in-page page)
  (reverse! (page-contents page)))

(define-public get-current-page active-page)

(define-public (get-object-pins object)
  (if (component? object)
      (reverse! (filter! pin? (component-contents object)))
      '()))

(define-public (get-pin-ends pin)
  (let ((params (line-info pin)))
    (cons (list-ref params 0) (list-ref params 1))))

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
