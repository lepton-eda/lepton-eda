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
  #:use-module (ice-9 format)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)

  #:use-module (lepton ffi)
  #:use-module (lepton gettext)
  #:use-module (lepton log)
  #:use-module (lepton page foreign)
  #:use-module (lepton page)
  #:use-module (lepton toplevel)

  #:use-module (schematic ffi)

  #:export (%lepton-window
            current-window
            with-window
            active-page
            set-active-page!
            pointer-position
            snap-point)

  ;; Overrides the close-page! procedure in the (lepton page)
  ;; module.
  #:replace (close-page!))


;;; This is a fluid that is initialized with pointer to a new
;;; lepton-schematic window when it is created.  Any Scheme
;;; callback procedure called inside the window may use the value
;;; of the fluid to reference its window, thus avoiding the need
;;; of any additional arguments.  In any window, the fluid points
;;; exactly to it.
(define %lepton-window (make-fluid))

;;; Define a wrapped pointer type.
(define-wrapped-pointer-type <schematic-window>
  schematic-window?
  wrap-schematic-window
  unwrap-schematic-window
  ;; Printer.
  (lambda (window port)
    (format port "#<schematic-window-0x~x>"
            (pointer-address (unwrap-schematic-window window)))))


;;; Execute forms in the dynamic context of WINDOW and its
;;; toplevel.  We have to dynwind LeptonToplevel here as well
;;; since there are functions that depend on it and should know
;;; what its current value is.
(define-syntax-rule (with-window window form form* ...)
  (with-fluids ((%lepton-window window)
                (%lepton-toplevel
                 (gschem_toplevel_get_toplevel window)))
    form form* ...))


(define (current-window)
  "Return the value of the toplevel window structure fluid in the
current dynamic context.  Signals an error if there is no valid
window fluid or the fluid value is NULL.  Never returns NULL."
  (let ((window (fluid-ref %lepton-window)))

    (when (null-pointer? window)
      (error (G_ "Found NULL lepton-schematic window.")))
    window))


(define (active-page)
  "Returns the page which is active in the current
lepton-schematic window.  If there is no active page, returns #f."
  (let ((*page (lepton_toplevel_get_page_current
                (toplevel->pointer (current-toplevel)))))
    (and (not (null-pointer? *page))
         (pointer->page *page))))


(define (set-active-page! page)
  "Sets the page which is active in the current lepton-schematic
window to PAGE.  Returns PAGE."
  (define *page (check-page page 1))
  (x_window_set_current_page (current-window) *page)
  page)


(define (close-page! page)
  "Closes PAGE."
  (define *page (check-page page 1))
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


(define (snap-point point)
  "Snaps POINT in the form (X . Y) to the snap grid, returning the
snapped point position as a pair in the same form.  This always
snaps the given point to the grid, disregarding the current user
snap settings."
  ;; Mimic snap_grid() when snapping is not off.
  (define (snap-grid coord)
    (lepton_coord_snap coord
                       (gschem_options_get_snap_size
                        (schematic_window_get_options (current-window)))))

  (check-coord point 1)

  (cons (snap-grid (car point)) (snap-grid (cdr point))))
