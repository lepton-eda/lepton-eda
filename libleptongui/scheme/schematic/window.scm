;; Lepton EDA Schematic Capture
;; Scheme API
;; Copyright (C) 2010-2011 Peter Brett <peter@peter-b.co.uk>
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

(define-module (schematic window)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)

  #:use-module (lepton ffi)
  #:use-module (lepton page foreign)

  ; Import C procedures
  #:use-module (schematic core window)

  #:use-module (schematic ffi)

  #:export (active-page
            set-active-page!
            pointer-position)

  ;; Overrides the close-page! procedure in the (lepton page)
  ;; module.
  #:replace (close-page!))


(define (active-page)
  "Returns the page which is active in the current
lepton-schematic window.  If there is no active page, returns #f."
  (let ((*page (lepton_toplevel_get_page_current
                (edascm_c_current_toplevel))))
    (and (not (null-pointer? *page))
         (pointer->geda-page *page))))


(define (set-active-page! page)
  "Sets the page which is active in the current lepton-schematic
window to PAGE.  Returns PAGE."
  (define *page (geda-page->pointer* page 1))
  (x_window_set_current_page (current-window) *page)
  page)


(define (close-page! page)
  "Closes PAGE."
  (define *page (geda-page->pointer* page 1))
  (define *window (current-window))
  ;; Currently active page.
  (define *active_page
    (schematic_window_get_active_page *window))

  (if (eq? page (active-page))
      (x_window_close_page *window *page)
      ;; If the page is not active, make it active and close, then
      ;; switch back to the previously active page.
      (begin
        (x_window_set_current_page *window *page)
        (x_window_close_page *window
                             (schematic_window_get_active_page *window))
        (x_window_set_current_page *window *active_page)))

  ;; Return value is unspecified.
  (if #f #f))


(define (pointer-position)
  "Returns the current mouse pointer position, expressed in world
coordinates in the form (X . Y).  If the pointer is outside the
schematic drawing area, returns #f."
  (define x (make-bytevector (sizeof int)))
  (define y (make-bytevector (sizeof int)))

  (let ((result (true? (x_event_get_pointer_position
                        (current-window)
                        FALSE
                        (bytevector->pointer x)
                        (bytevector->pointer y)))))
    (and result
         (cons (bytevector-sint-ref x 0 (native-endianness) (sizeof int))
               (bytevector-sint-ref y 0 (native-endianness) (sizeof int))))))


(define-public current-window %current-window)

(define-public (snap-point point)
  (%snap-point (car point) (cdr point)))
