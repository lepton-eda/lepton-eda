;; gEDA - GPL Electronic Design Automation
;; gschem extension for saving symbols
;; Copyright (C) 2011 Peter Brett <peter@peter-b.co.uk>
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

(use-modules (srfi srfi-1))
(use-modules (geda object))
(use-modules (geda page))
(use-modules (geda attrib))
(use-modules (gschem window))
(use-modules (gschem selection))

(define (save-selected-symbol)

  ;; Demote an attribute into a symbol.  This first searches page for
  ;; an unattached attribute with the same name as attrib.  If found,
  ;; the value of the found attribute is replaced by the the value of
  ;; attrib; otherwise, attrib is copied into page.
  (define (demote-attribute! page attrib)
    (let* ((name (attrib-name attrib))
           (current (find
                     (lambda (x) (and (attribute? x)
                                      (equal? name (attrib-name x))))
                     (page-contents page))))

      (if current
          ;; We found a matching attribute, so set the value and
          ;; visibility. This makes sure that attributes that were
          ;; promoted and made invisible are made visible again.
          (begin
            (set-attrib-value! current (attrib-value attrib))
            (set-text-visibility! current (text-visible? attrib)))
          ;; We didn't find a match, so copy the attribute into the
          ;; page.
          (page-append! page (copy-object attrib)))))

  ;; This is the main worker function. It creates a new page,
  ;; initially populated by the contents of object, which must be a
  ;; component.  Attributes attached to the object are copied into the
  ;; generated page, overriding existing attributes with that name.
  (define (component->page object)
    ;; Initial page filename is
    ;; "<schematic filename>@<symbol basename>"
    (let ((P (make-page (string-join
                         (list
                          (page-filename (object-page object))
                          (component-basename object))
                         "@"))))

      ;; Copy over the contents of the component.
      (for-each
       (lambda (x)
         (if (attrib-attachment x)
             ;; Object is attached as an attribute; skip it, we'll
             ;; deal with it when we copy the object that it's
             ;; attached to.
             #t
             ;; Otherwise, copy the object over, and any attached
             ;; attributes.
             (let ((new-x (copy-object x)))
               (page-append! P new-x)
               (for-each
                (lambda (a)
                  (let ((new-a (copy-object a)))
                    (page-append! P new-a)
                    (attach-attrib! new-x new-a)))
                (object-attribs x)))))

       (component-contents object))

      ;; Demote attributes
      (for-each (lambda (x) (demote-attribute! P x))
                (object-attribs object))

      ;; Return the created page
      P))

  ;; Returns the selected component.  Displays a message box if the
  ;; components selected != 1.
  (define (get-selected-component)
    (let* ((components (filter! component? (page-selection (active-page))))
           (n (length components)))
      (cond
       ((= n 0)
        (begin
          (gschem-msg "You must select a component.")
          #f))
       ((> n 1)
        (begin
          (gschem-msg "You must select only one component.")
          #f))
       (else (car components)))))

  ;; Do cool stuff
  (let ((component (get-selected-component)))
    (and component
         (begin
           (set-active-page!
            (component->page component))
           (view-zoom-extents)
           (file-save-as)
           #t))))
