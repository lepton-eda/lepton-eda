;;; Lepton EDA library - Scheme API
;;; Copyright (C) 2010 Peter Brett <peter@peter-b.co.uk>
;;; Copyright (C) 2010-2017 gEDA Contributors
;;; Copyright (C) 2017-2021 Lepton EDA Contributors
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
  #:use-module (ice-9 optargs)
  #:use-module ((ice-9 rdelim)
                #:select (read-string)
                #:prefix rdelim:)
  #:use-module (system foreign)

  ;; Import C procedures
  #:use-module (lepton core gettext)
  #:use-module (lepton core page)
  #:use-module (lepton ffi)
  #:use-module (lepton page foreign)

  #:use-module (lepton object type)

  #:use-module (lepton os)

  #:export (close-page!
            file->page
            make-page
            object-page
            page?
            page-dirty?
            page-filename
            set-page-filename!))

(define (page? page)
  "Returns #t if PAGE is a #<geda-page> instance, otherwise
returns #f."
  (true? (edascm_is_page (scm->pointer page))))


(define (object-page object)
  "Returns a page that OBJECT belongs to.  If OBJECT does not
belong to a page, returns #f."
  (define object-pointer (geda-object->pointer* object 1))

  (let ((page-pointer (lepton_object_get_page object-pointer)))
    (and (not (null-pointer? page-pointer))
         (pointer->geda-page page-pointer))))


(define-public active-pages %active-pages)


(define (make-page filename)
  "Creates and returns a new page associated with FILENAME.  Note
that this does not check that a file exists with that name, or
attempt to load any data from it."
  (check-string filename 1)
  (pointer->geda-page (s_page_new (edascm_c_current_toplevel)
                                  (string->pointer filename))))


(define (close-page! page)
  "Destroys PAGE, freeing all of its resources.  Attempting to use
PAGE after calling this function will cause an error."
  (define pointer (geda-page->pointer* page 1))

  (s_page_delete (edascm_c_current_toplevel)
                 pointer))


(define (page-filename page)
  "Returns the filename associated with PAGE as a string."
  (define pointer (geda-page->pointer* page 1))

  (pointer->string (s_page_get_filename pointer)))


(define (set-page-filename! page filename)
  "Sets the filename associated with PAGE.  Returns PAGE."
  (define pointer (geda-page->pointer* page 1))
  (check-string filename 2)

  (s_page_set_filename pointer (string->pointer filename))
  page)


(define-public page-contents %page-contents)


(define (page-dirty? page)
  "Returns #t if PAGE has been flagged as having been modified,
otherwise returns #f."
  (define pointer (geda-page->pointer* page 1))

  (true? (lepton_page_get_changed pointer)))


(define-public page->string %page->string)
(define-public string->page %string->page)

(define-public (page-append! P . objects)
  (for-each (lambda (x) (%page-append! P x)) objects)
  P)

(define-public (page-remove! P . objects)
  (for-each (lambda (x) (%page-remove! P x)) objects)
  P)

(define*-public (set-page-dirty! page #:optional (state #t))
  (%set-page-dirty! page state))


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
