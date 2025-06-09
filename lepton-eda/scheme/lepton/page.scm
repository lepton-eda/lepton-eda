;;; Lepton EDA library - Scheme API
;;; Copyright (C) 2010 Peter Brett <peter@peter-b.co.uk>
;;; Copyright (C) 2010-2017 gEDA Contributors
;;; Copyright (C) 2017-2022 Lepton EDA Contributors
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


(define-module (lepton page)
  #:use-module (ice-9 format)
  #:use-module (ice-9 optargs)
  #:use-module ((ice-9 rdelim)
                #:select (read-string)
                #:prefix rdelim:)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)

  #:use-module (lepton ffi boolean)
  #:use-module (lepton ffi check-args)
  #:use-module (lepton ffi glib)
  #:use-module (lepton ffi)
  #:use-module (lepton gerror)
  #:use-module (lepton object type)
  #:use-module (lepton object foreign)
  #:use-module (lepton os)
  #:use-module (lepton page foreign)
  #:use-module (lepton toplevel foreign)
  #:use-module (lepton toplevel)

  #:export (page?
            active-pages
            close-page!
            file->page
            make-page
            object-page
            page-append!
            page-remove!
            page-contents
            page-dirty?
            set-page-dirty!
            page-filename
            set-page-filename!
            page->string
            string->page))

(define (page? page)
  "Returns #t if PAGE is a <page> instance, otherwise returns
#f."
  (is-page? page))


(define (object-page object)
  "Returns a page that OBJECT belongs to.  If OBJECT does not
belong to a page, returns #f."
  (define object-pointer (check-object object 1))

  (let ((page-pointer (lepton_object_get_page object-pointer)))
    (and (not (null-pointer? page-pointer))
         (pointer->page page-pointer))))


(define (active-pages)
  "Returns a list of currently-opened pages."
  (glist->list
   (lepton_page_list_get_glist
    (lepton_toplevel_get_pages (toplevel->pointer (current-toplevel))))
   pointer->page))


(define (make-page filename)
  "Creates and returns a new page associated with FILENAME.  Note
that this does not check that a file exists with that name, or
attempt to load any data from it.  Raises a 'wrong-type-arg error
if the current <toplevel> (as returned by the current-toplevel()
procedure) is not defined or set to a wrong value."
  (check-string filename 1)

  (when (string-null? filename)
        (scm-error 'misc-error
                   'make-page
                   "Filename cannot be an empty string."
                   '()
                   #f))

  (let ((toplevel (current-toplevel)))
    (if (toplevel? toplevel)
        (pointer->page (lepton_page_new (toplevel->pointer toplevel)
                                        (string->pointer filename)))
        (scm-error 'wrong-type-arg
                   'make-page
                   "Current <toplevel> value is not defined or wrong: ~A"
                   (list toplevel)
                   #f))))


(define (close-page! page)
  "Destroys PAGE, freeing all of its resources.  Attempting to use
PAGE after calling this function will cause an error."
  (define pointer (check-page page 1))

  (lepton_page_delete (toplevel->pointer (current-toplevel))
                      pointer))


(define (page-filename page)
  "Returns the filename associated with PAGE as a string."
  (define pointer (check-page page 1))

  (pointer->string (lepton_page_get_filename pointer)))


(define (set-page-filename! page filename)
  "Sets the filename associated with PAGE.  Returns PAGE."
  (define pointer (check-page page 1))
  (check-string filename 2)

  (lepton_page_set_filename pointer (string->pointer filename))
  page)


(define (page-contents page)
  "Returns the contents of PAGE as a list of objects."
  (define pointer (check-page page 1))

  (glist->list (lepton_page_objects pointer) pointer->object))


(define (page-dirty? page)
  "Returns #t if PAGE has been flagged as having been modified,
otherwise returns #f."
  (define pointer (check-page page 1))

  (true? (lepton_page_get_changed pointer)))


(define* (set-page-dirty! page #:optional (state #t))
  "Clears the flag of changed state of PAGE if dirty STATE is #f.
Otherwise, flags PAGE as having been modified.  Returns PAGE."
  (define pointer (check-page page 1))
  (check-boolean state 2)

  (lepton_page_set_changed pointer (if state TRUE FALSE))
  page)


(define (page->string page)
  "Returns a string representation of the contents of PAGE."
  (define pointer (check-page page 1))
  (pointer->string
   (lepton_object_list_to_buffer (lepton_page_objects pointer))))


(define (string->page filename str)
  "Creates a new page from a string representation. Returns the
page with filename FILENAME created by parsing STR.  Raises a
'string-format error if STR contains invalid gEDA file format
syntax."
  (define (gerror-error *error)
    (let ((*err (dereference-pointer *error)))
      (unless (null-pointer? *err)
        (let ((message (gerror-message *err)))
          (g_clear_error *error)
          (scm-error 'string-format
                     'string->page
                     "Parse error: ~s"
                     (list message)
                     '())))))

  (check-string filename 1)
  (check-string str 2)

  (let* ((*error (bytevector->pointer (make-bytevector (sizeof '*) 0)))
         (pointer (lepton_page_new (toplevel->pointer (current-toplevel))
                                   (string->pointer filename)))
         (objects (o_read_buffer pointer
                                 %null-pointer
                                 (string->pointer str)
                                 -1
                                 (lepton_page_get_filename pointer)
                                 *error)))
    (gerror-error *error)

    (lepton_page_append_list pointer objects)

    (pointer->page pointer)))


;;; Adds OBJECT to PAGE.  If OBJECT is already attached to a page
;;; or to a component object, raises an 'object-state error.
;;; Returns modified PAGE.
(define (%page-append! page object)
  (define page-pointer (check-page page 1))
  (define object-pointer (check-object object 2))

  ;; Check that the object isn't already attached to something.
  (let ((object-page-pointer (lepton_object_get_page object-pointer)))
    (when (or (and (not (null-pointer? object-page-pointer))
                   (not (equal? object-page-pointer page-pointer)))
              (not (null-pointer? (lepton_object_get_parent object-pointer))))
      (scm-error 'object-state
                 'page-append!
                 "Object ~A is already attached to something."
                 (list object)
                 '()))

    (if (equal? object-page-pointer page-pointer)
        page

        (begin
          (lepton_object_emit_pre_change_notify object-pointer)
          (lepton_page_append page-pointer object-pointer)
          (lepton_object_emit_change_notify object-pointer)
          (lepton_page_set_changed page-pointer 1)

          page))))

(define (page-append! page . objects)
  " Appends zero or more OBJECTS to the contents of PAGE in the
order given.  If any of the OBJECTS is already part of a page
other than PAGE, or is part of a component object, raises an
'object-state error.  Any of the OBJECTS that are already in the
PAGE are ignored.  Returns PAGE."
  (for-each (lambda (x) (%page-append! page x)) objects)
  page)


;;; Removes OBJECT from PAGE.  If OBJECT is attached to a page
;;; other than PAGE, or to a component object, raises an
;;; 'object-state error.  If OBJECT is not attached to a page,
;;; does nothing.  Returns PAGE.
(define (%page-remove! page object)
  (define page-pointer (check-page page 1))
  (define object-pointer (check-object object 2))

  ;; Check that the object is not attached to something else.
  (let ((object-page-pointer (lepton_object_get_page object-pointer)))
    (when (or (and (not (null-pointer? object-page-pointer))
                   (not (equal? object-page-pointer page-pointer)))
              (not (null-pointer? (lepton_object_get_parent object-pointer))))
      (scm-error 'object-state
                 'page-remove!
                 "Object ~A is attached to a component or different page."
                 (list object)
                 '()))

    ;; Check that object is not attached as an attribute.
    (unless (null-pointer? (lepton_object_get_attached_to object-pointer))
      (scm-error 'object-state
                 'page-remove!
                 "Object ~A is attached as an attribute."
                 (list object)
                 '()))

    ;; Check that object doesn't have attributes.
    (unless (null-pointer? (lepton_object_get_attribs object-pointer))
      (scm-error 'object-state
                 'page-remove!
                 "Object ~A has attributes."
                 (list object)
                 '()))

    (if (null-pointer? object-page-pointer)
        page

        (begin
          (lepton_object_emit_pre_change_notify object-pointer)
          (lepton_page_remove page-pointer object-pointer)
          (lepton_page_set_changed page-pointer 1)
          ;; If the object is currently selected unselect it.
          (o_selection_remove (lepton_page_get_selection_list page-pointer)
                              object-pointer)
          (lepton_object_emit_change_notify object-pointer)
          page))))


(define (page-remove! page . objects)
  "Removes zero or more OBJECTS from the contents of PAGE.  Any
OBJECTS that are not part of a page or component object are
ignored.  An 'object-state error will be thrown if any of the
OBJECTS satisfies any of the following conditions:
- part of a page other than PAGE;
- part of component object;
- has attached attributes;
- is attached as an attribute.
Returns PAGE."
  (for-each (lambda (x) (%page-remove! page x)) objects)
  page)

;;; Reads file FILENAME and outputs a page with the same name.
(define (file-contents->page filename)
  (with-input-from-file filename
    (lambda ()
      (string->page filename (rdelim:read-string)))))


;;; Returns an opened page from PAGES by FILENAME. If no
;;; corresponding page found, returns #f.
(define (page-by-filename filename pages)
  (and (not (null? pages))
       (let ((page (car pages)))
         (if (string= filename (page-filename page))
             page
             (page-by-filename filename (cdr pages))))))


(define* (file->page filename #:optional new-page?)
  "Given FILENAME, returns an opened page for it, or a new page if
none exists. Optional argument NEW-PAGE? can be used to force
creation of a new page for given filename."
  (let ((filename (expand-env-variables filename)))
    (if new-page?
        (file-contents->page filename)
        (or (page-by-filename filename (active-pages))
            (file-contents->page filename)))))
