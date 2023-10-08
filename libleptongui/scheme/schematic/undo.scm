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
  #:use-module (srfi srfi-1)
  #:use-module (system foreign)

  #:use-module (lepton ffi boolean)
  #:use-module (lepton ffi glib)
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
  #:export (undo-cleanup-backup-files!
            undo-save-state
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
;;; Flags defined in struct.h.
(define F_OPEN_RC 1)
(define F_OPEN_CHECK_BACKUP 2)
(define F_OPEN_FORCE_BACKUP 4)
(define F_OPEN_RESTORE_CWD 8)


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

  (define (page-undo-callback *window *page-view *page redo?)
    (unless (null-pointer? *page)
      (let ((*current-undo (lepton_page_get_undo_current *page)))
        (unless (null-pointer? *current-undo)
          (let ((*undo-to-do (if (true? redo?)
                                 ;; Redo action.
                                 (lepton_undo_get_next *current-undo)
                                 ;; Undo action.
                                 (lepton_undo_get_prev *current-undo))))
            (unless (null-pointer? *undo-to-do)
              (let ((undo-viewport?
                     (and (= (lepton_undo_get_type *current-undo) UNDO_ALL)
                          (= (lepton_undo_get_type *undo-to-do) UNDO_VIEWPORT_ONLY))))
                (when undo-viewport?
                  ;; Debugging stuff.
                  (log! 'debug "Type: ~A\n" (lepton_undo_get_type *undo-to-do))
                  (log! 'debug "Current is an undo all, next is viewport only!\n")

                  ;; For only viewport changes, <undo> data omits
                  ;; 'filename' or 'object_list' as they are
                  ;; retrieved from previous list items.  Hence,
                  ;; 'filename' and 'object_list' are not freed
                  ;; and just set to NULL below.
                  (if (= (schematic_window_get_undo_type *window) UNDO_DISK)
                      (lepton_undo_set_filename *undo-to-do
                                                (o_undo_find_prev_filename *undo-to-do))
                      (lepton_undo_set_object_list *undo-to-do
                                                   (o_undo_find_prev_object_head *undo-to-do))))
                ;; Save page filename to restore it later in case
                ;; a temporary file is opened for undo.  The
                ;; filename is stored as a Scheme string as the
                ;; data pointed to by pointer returned by
                ;; lepton_page_get_filename() is freed inside
                ;; lepton_page_set_filename() below, so we could
                ;; get corrupted data otherwise.
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
                    ;; Temporarily disable logging.
                    (lepton_log_set_logging_enabled FALSE)

                    (if (and (= (schematic_window_get_undo_type *window) UNDO_DISK)
                             (not (null-pointer? (lepton_undo_get_filename *undo-to-do))))
                        ;; F_OPEN_RESTORE_CWD: go back from
                        ;; temporary directory, so that local
                        ;; config files can be read.
                        (f_open (gschem_toplevel_get_toplevel *window)
                                *page
                                (lepton_undo_get_filename *undo-to-do)
                                F_OPEN_RESTORE_CWD
                                %null-pointer)

                        ;; Otherwise objects are restored from
                        ;; memory.
                        (when (and (= (schematic_window_get_undo_type *window) UNDO_MEMORY)
                                   (not (null-pointer? (lepton_undo_get_object_list *undo-to-do))))
                          (lepton_page_append_list *page
                                                   (o_glist_copy_all (lepton_undo_get_object_list *undo-to-do)
                                                                     %null-pointer))))
                    (lepton_page_set_page_control *page
                                                  (lepton_undo_get_page_control *undo-to-do))
                    (lepton_page_set_up *page (lepton_undo_get_up *undo-to-do))
                    (gschem_toplevel_page_content_changed *window *page)

                    (let ((*geometry (gschem_page_view_get_page_geometry *page-view)))
                      (when (or (true? (schematic_window_get_undo_panzoom *window))
                                (true? (o_undo_modify_viewport)))
                        (if (not (zero? (lepton_undo_get_scale *undo-to-do)))
                            (begin
                              (gschem_page_geometry_set_viewport *geometry
                                                                 (lepton_undo_get_x *undo-to-do)
                                                                 (lepton_undo_get_y *undo-to-do)
                                                                 (lepton_undo_get_scale *undo-to-do))
                              (gschem_page_view_invalidate_all *page-view))
                            (gschem_page_view_zoom_extents *page-view
                                                           (lepton_undo_get_object_list *undo-to-do)))))

                    ;; Restore logging.
                    (lepton_log_set_logging_enabled save-logging?)
                    ;; Set filename right.
                    (lepton_page_set_filename *page
                                              (string->pointer save-filename))
                    ;; Final redraw.
                    (page_select_widget_update *window)
                    (x_multiattrib_update *window)
                    (i_update_menus *window)

                    ;; Restore saved undo structures.
                    (lepton_page_set_undo_bottom *page *save-undo-bottom)
                    (lepton_page_set_undo_tos *page *save-undo-top)
                    (lepton_page_set_undo_current *page *current-undo)

                    (if (not (true? redo?))
                        ;; Undo action.
                        (unless (null-pointer? (lepton_page_get_undo_current *page))
                          (lepton_page_set_undo_current *page
                                                        (lepton_undo_get_prev (lepton_page_get_undo_current *page)))
                          (when (null-pointer? (lepton_page_get_undo_current *page))
                            (lepton_page_set_undo_current *page
                                                          (lepton_page_get_undo_bottom *page))))
                        ;; Redo action.
                        (unless (null-pointer? (lepton_page_get_undo_current *page))
                          (lepton_page_set_undo_current *page
                                                        (lepton_undo_get_next (lepton_page_get_undo_current *page)))
                          (when (null-pointer? (lepton_page_get_undo_current *page))
                            (lepton_page_set_undo_current *page
                                                          (lepton_page_get_undo_tos *page)))))
                    ;; Don't have to free data here since 'filename' or 'object_list' are
                    ;; just pointers to the real data (lower in the stack).
                    (when undo-viewport?
                      (lepton_undo_set_filename *undo-to-do %null-pointer)
                      (lepton_undo_set_object_list *undo-to-do %null-pointer))

                    ;; Debugging stuff.
                    (log! 'debug "\n\n---Undo----\n")
                    (lepton_undo_print_all (lepton_page_get_undo_bottom *page))
                    (log! 'debug "TOS: ~A\n" (lepton_undo_get_filename (lepton_page_get_undo_tos *page)))
                    (log! 'debug "CURRENT: ~A\n" (lepton_undo_get_filename (lepton_page_get_undo_current *page)))
                    (log! 'debug "----\n"))))))))))

  (if undo-enabled?
      (let ((*page-view (gschem_toplevel_get_current_page_view *window)))
        (if (null-pointer? *page-view)
            (log! 'warning "undo-callback: NULL page view.")
            (page-undo-callback *window
                                *page-view
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


(define (undo-cleanup-backup-files!)
  "Removes undo backup files created during schematic editing
session."
  (define max-id (schematic_undo_get_file_index))

  (define (unlink-by-id id)
    (let* ((*filename (schematic_undo_index_to_filename id))
           (filename (pointer->string *filename)))
      (when (file-exists? filename)
        (delete-file filename))
      (g_free *filename)))

  (for-each unlink-by-id (iota max-id))
  (schematic_undo_set_tmp_path %null-pointer))
