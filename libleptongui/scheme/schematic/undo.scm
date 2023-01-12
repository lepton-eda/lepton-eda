;; Lepton EDA Schematic Capture
;; Scheme API
;; Copyright (C) 2017 dmn <graahnul.grom@gmail.com>
;; Copyright (C) 2017-2023 Lepton EDA Contributors
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

( define-module  ( schematic undo )
  #:use-module (system foreign)

  #:use-module (lepton ffi boolean)
  #:use-module (lepton ffi undo)
  #:use-module (lepton ffi)
  #:use-module (lepton config)
  #:use-module (lepton log)

  #:use-module (schematic action-mode)
  #:use-module (schematic ffi)
  #:use-module (schematic gettext)
  #:use-module (schematic window foreign)
  #:use-module (schematic window global)

    ; public:
    ;
  #:export (undo-save-state
            undo!
            redo!
            ;; Toolbar callbacks.
            callback-edit-undo
            callback-edit-redo)

) ; define-module


;;; Variables defined in defines.h for C code.
(define UNDO_ALL 0)
(define UNDO_VIEWPORT_ONLY 1)
;;; Variables defined in gschem_defines.h.
(define UNDO_DISK 0)
(define UNDO_MEMORY 1)


(define (undo-save-state)
  "Saves current state onto the undo stack.  Returns #t on
success, #f on failure."
  (define *window (*current-window))

  (let ((*view (gschem_toplevel_get_current_page_view *window)))
    (and (not (null-pointer? *view))
         (let ((*page (gschem_page_view_get_page *view)))
           (and (not (null-pointer? *page))
                (o_undo_savestate *window *page UNDO_ALL)
                #t)))))


(define (undo-callback *window redo?)
  (define undo-enabled?
    (config-boolean (path-config-context (getcwd))
                    "schematic.undo"
                    "undo-control"))

  (define (page-undo-callback *window *page redo?)
    (unless (null-pointer? *page)
      (let ((*current-undo (lepton_page_get_undo_current *page)))
        (unless (null-pointer? *current-undo)
          (let ((*undo-to-do (if (true? redo?)
                                 ;; Redo action.
                                 (lepton_undo_get_next *current-undo)
                                 ;; Undo action.
                                 (lepton_undo_get_prev *current-undo))))
            (unless (null-pointer? *undo-to-do)
              (let* ((undo-viewport?
                      (and (= (lepton_undo_get_type *current-undo) UNDO_ALL)
                           (= (lepton_undo_get_type *undo-to-do) UNDO_VIEWPORT_ONLY)))
                     ;; This C boolean variable indicates that for
                     ;; only viewport changes, undo 'filename' or
                     ;; 'object_list' fields have to be set to
                     ;; NULL in o_undo_callback().  This needs to
                     ;; be so since the functions
                     ;; o_undo_find_prev_filename() and
                     ;; o_undo_find_prev_object_head() omit such
                     ;; <undo> data when searching for list of
                     ;; objects to resurrect, or the filename to
                     ;; resurrect them from.  The data can be just
                     ;; retrieved from some previous non-viewport
                     ;; undo list item.
                     (search-for-previous-data? (if undo-viewport? TRUE FALSE)))
                (when undo-viewport?
                  ;; Debugging stuff.
                  (log! 'debug "Type: ~A\n" (lepton_undo_get_type *undo-to-do))
                  (log! 'debug "Current is an undo all, next is viewport only!\n")

                  (if (= (schematic_window_get_undo_type *window) UNDO_DISK)
                      (lepton_undo_set_filename *undo-to-do
                                                (o_undo_find_prev_filename *undo-to-do))
                      (lepton_undo_set_object_list *undo-to-do
                                                   (o_undo_find_prev_object_head *undo-to-do))))
                ;; Save page filename to restore it later in case
                ;; a temporary file is opened for undo.  The
                ;; filename is stored as a Scheme string as the
                ;; data pointed to by pointer returned by
                ;; lepton_page_get_filename() is freed in between
                ;; by lepton_page_set_filename() in
                ;; o_undo_callback(), so we could get corrupted
                ;; data otherwise.
                (let ((save-filename (pointer->string (lepton_page_get_filename *page)))
                      ;; Save undo structure so it's not nuked.
                      (*save-undo-bottom (lepton_page_get_undo_bottom *page))
                      (*save-undo-top (lepton_page_get_undo_tos *page)))
                  ;; Initialize a new undo structure.
                  (lepton_page_set_undo_bottom *page %null-pointer)
                  (lepton_page_set_undo_tos *page %null-pointer)
                  (lepton_page_set_undo_current *page %null-pointer)

                  ;; Unselect all objects.
                  (o_select_unselect_all *window)

                  (when (or (and (= (schematic_window_get_undo_type *window) UNDO_DISK)
                                 (not (null-pointer? (lepton_undo_get_filename *undo-to-do))))
                            (and (= (schematic_window_get_undo_type *window) UNDO_MEMORY)
                                 (not (null-pointer? (lepton_undo_get_object_list *undo-to-do)))))
                    ;; Delete page objects.
                    (lepton_page_delete_objects *page)
                    ;; Free the objects in the place list.
                    (schematic_window_delete_place_list *window)
                    ;; Mark active page as changed.
                    (schematic_window_active_page_changed *window))

                  (let ((save-logging? (lepton_log_get_logging_enabled)))
                    ;; Temporarily disable logging.  It will be
                    ;; enabled in o_undo_callback() after undo is
                    ;; accomplished.
                    (lepton_log_set_logging_enabled FALSE)

                    (o_undo_callback *window
                                     *page
                                     *current-undo
                                     *save-undo-bottom
                                     *save-undo-top
                                     *undo-to-do
                                     (string->pointer save-filename)
                                     redo?
                                     ;; See comments above.
                                     search-for-previous-data?
                                     save-logging?))))))))))

  (if undo-enabled?
      (let ((*page-view (gschem_toplevel_get_current_page_view *window)))
        (if (null-pointer? *page-view)
            (log! 'warning "undo-callback: NULL page view.")
            (page-undo-callback *window
                                (gschem_page_view_get_page *page-view)
                                redo?)))
      (log! 'message (G_ "Undo/Redo is disabled in configuration"))))


(define (callback-edit-undo *widget *window)
  ;; If we're cancelling from a move action, re-wind the
  ;; page contents back to their state before we started.
  ;;
  ;; It "might" be nice to sub-undo rotates / zoom changes
  ;; made whilst moving components, but when the undo code
  ;; hits lepton_page_delete(), the place list objects are free'd.
  ;; Since they are also contained in the schematic page, a
  ;; crash occurs when the page objects are free'd.
  (if (in-action?)
      (i_callback_cancel *widget *window)
      (undo-callback *window FALSE)))


(define (callback-edit-redo *widget *window)
  (undo-callback *window TRUE))


(define (undo!)
  "Undo the last action done in the current window."
  (callback-edit-undo %null-pointer (*current-window)))


(define (redo!)
  "Redo the last action undone in the current window."
  (callback-edit-redo %null-pointer (*current-window)))
