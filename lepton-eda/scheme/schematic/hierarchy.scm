;;; Lepton EDA Schematic Capture
;;; Scheme API
;;; Copyright (C) 2023-2026 Lepton EDA Contributors
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


(define-module (schematic hierarchy)
  #:use-module (system foreign)

  #:use-module (lepton ffi check-args)
  #:use-module (lepton ffi glib)
  #:use-module (lepton ffi)
  #:use-module (lepton gettext)
  #:use-module (lepton library)
  #:use-module (lepton log)
  #:use-module (lepton page foreign)

  #:use-module (schematic ffi)
  #:use-module (schematic file)
  #:use-module (schematic window foreign)

  #:export (open-subschematic
            open-symbol))

;;; The function searches for an associated source file referred
;;; by FILENAME and loads it if it is not already in the list of
;;; the currently open pages.  The function returns the found or
;;; loaded source page, or %NULL-POINTER if it could not find the
;;; source file.  Any issue is reported to the log.
(define (*open-subschematic window filename parent-page page-control)
  (define *window (check-window window 1))
  (define *filename (and (check-string filename 2)
                         (string->pointer filename)))
  (define *parent-page (check-page parent-page 3))
  (define *toplevel (schematic_window_get_toplevel *window))

  (define (get-forebear *page *found-page)
    ;; Check whether this page is in the parents list.
    (let loop ((*forebear *page))
      (if (and (not (null-pointer? *forebear))
               (not (= (lepton_page_get_pid *found-page)
                       (lepton_page_get_pid *forebear)))
               (>= (lepton_page_get_up *forebear) 0))
          (loop (lepton_toplevel_search_page_by_id (lepton_toplevel_get_pages *toplevel)
                                                   (lepton_page_get_up *forebear)))
          *forebear)))

  (check-integer page-control 4)

  (let ((source-filename (get-source-library-file filename)))
    (if source-filename
        (let* ((normalized-filename (canonicalize-path source-filename))
               (*found-page (lepton_toplevel_search_page *toplevel
                                                         (string->pointer normalized-filename))))
          (if (null-pointer? *found-page)
              ;; Subschematic has not been found, let's create a new page.
              (let ((page (open-schematic source-filename)))
                (and page
                     (let ((*page (page->pointer page)))
                       (if (zero? page-control)
                           (begin
                             (schematic_hierarchy_increment_page_control_counter)
                             (lepton_page_set_page_control
                              *page
                              (schematic_hierarchy_get_page_control_counter)))
                           (lepton_page_set_page_control *page page-control))

                       (lepton_page_set_up
                        *page
                        (lepton_page_get_pid *parent-page))
                       *page)))

              ;; Page has been found.
              (let ((*forebear-page (get-forebear *parent-page *found-page)))
                (if (and (not (null-pointer? *forebear-page))
                         (= (lepton_page_get_pid *found-page)
                            (lepton_page_get_pid *forebear-page)))
                    (scm-error 'hierarchy-loop
                               #f
                               (G_ "Hierarchy contains a circular dependency.")
                               '()
                               #f)
                    (begin
                      (lepton_toplevel_goto_page *toplevel *found-page)
                      (unless (zero? page-control)
                        (lepton_page_set_page_control *found-page page-control))
                      (lepton_page_set_up *found-page (lepton_page_get_pid *parent-page))
                      ;; return
                      *found-page)))))
        (scm-error 'missing-resource
                   #f
                   (G_ "Schematic ~S not found in source library.")
                   (list filename)
                   #f))))


(define (open-subschematic window
                           filename
                           parent-page
                           page-control
                           scheme-error-handler)
  (catch #t
    (lambda ()
      (*open-subschematic window filename parent-page page-control))
    ;; (lambda (key subr message args rest) ...)
    scheme-error-handler))


(define (open-symbol window filename parent-page)
  (define *window (check-window window 1))
  (define *parent (check-page parent-page 3))
  (define *symbol
    (s_clib_get_symbol_by_name (string->pointer filename)))
  (define source-path
    (and (not (null-pointer? *symbol))
         (let ((*clib-filename (s_clib_symbol_get_filename *symbol)))
           (if (null-pointer? *clib-filename)
               (begin
                 (log! 'message (G_ "Symbol is not a real file. Symbol cannot be loaded."))
                 #f)
               (let ((clib-filename (pointer->string *clib-filename)))
                 (g_free *clib-filename)
                 clib-filename)))))

  (define *symbol-page
    (and source-path
         (let* ((*toplevel (schematic_window_get_toplevel *window))
                (*filename (string->pointer (canonicalize-path source-path)))
                (*page (lepton_toplevel_search_page *toplevel *filename)))
           (if (null-pointer? *page)
               ;; Create a new page.
               (let ((page (open-schematic source-path)))
                 (and page
                      (let ((*page (page->pointer page)))
                        (schematic_hierarchy_increment_page_control_counter)
                        (lepton_page_set_page_control
                         *page
                         (schematic_hierarchy_get_page_control_counter))
                        *page)))

               ;; Return existing page.
               *page))))

  (when *symbol-page
    ;; Change link to parent page since we can come here from any
    ;; parent and must come back to the same page.
    (lepton_page_set_up *symbol-page (lepton_page_get_pid *parent)))

  *symbol-page)
