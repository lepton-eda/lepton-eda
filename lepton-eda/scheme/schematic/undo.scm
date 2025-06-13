;; Lepton EDA Schematic Capture
;; Scheme API
;; Copyright (C) 2017 dmn <graahnul.grom@gmail.com>
;; Copyright (C) 2017-2024 Lepton EDA Contributors
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
            undo-init-backup-path
            undo-save-state
            undo-save-viewport
            undo!
            redo!
            ;; Toolbar callbacks.
            callback-edit-undo
            callback-edit-redo)

) ; define-module


;;; Variables defined in schematic_defines.h.
(define UNDO_DISK 0)
(define UNDO_MEMORY 1)
;;; Flags defined in struct.h.
(define F_OPEN_RC 1)
(define F_OPEN_CHECK_BACKUP 2)
(define F_OPEN_FORCE_BACKUP 4)
(define F_OPEN_RESTORE_CWD 8)


(define (undo-panzoom?)
  "Return the value of the config setting 'undo-panzoom'."
  (config-boolean (path-config-context (getcwd))
                  "schematic.undo"
                  "undo-panzoom"))


(define* (undo-type #:optional (window (current-window)))
  "Return the value of the config setting 'undo-type'."
  (let ((type (config-string (path-config-context (getcwd))
                             "schematic.undo"
                             "undo-type")))
    (and (string? type)
         (string->symbol type))))


(define (undo-init-backup-path)
  (define undo-tmp-path (or (getenv "TMP") "/tmp"))

  (schematic_undo_set_tmp_path (string->pointer undo-tmp-path))
  (log! 'debug "UNDO backup path: ~S" undo-tmp-path))


(define (undo-save-state)
  "Saves current state onto the undo stack.  Returns #t on
success, #f on failure."
  (define *window (*current-window))

  (let ((*view (schematic_window_get_current_canvas *window)))
    (and (not (null-pointer? *view))
         (let ((*page (schematic_canvas_get_page *view)))
           (and (not (null-pointer? *page))
                (o_undo_savestate *window *page FALSE)
                #t)))))


(define (undo-save-viewport)
  "Saves current viewport data onto the undo stack."
  (define *window (*current-window))

  (when (undo-panzoom?)
    (o_undo_savestate_viewport *window)))


;;; Recursively search in the undo list *UNDO for an undo item
;;; having non-NULL structure field retrieved by C *GETTER.
(define (*undo-lookup-non-null-field *undo *getter)
  (let loop ((*undo-item (lepton_undo_get_prev *undo)))
    (if (null-pointer? *undo-item)
        %null-pointer
        (let ((*field-value (*getter *undo-item)))
          (if (null-pointer? *field-value)
              (loop (lepton_undo_get_prev *undo-item))
              *field-value)))))


;;; Recursively search in the undo list *UNDO for the first item
;;; having non-NULL field 'filename'.
(define (*find-previous-filename *undo)
  (*undo-lookup-non-null-field *undo lepton_undo_get_filename))


;;; Recursively search in the undo list *UNDO for the first item
;;; having non-NULL field 'object_list'.
(define (*find-previous-object-list *undo)
  (*undo-lookup-non-null-field *undo lepton_undo_get_object_list))


(define (undo-callback *window redo?)
  (define window (pointer->window *window))
  (define undo-enabled?
    (config-boolean (path-config-context (getcwd))
                    "schematic.undo"
                    "undo-control"))
  ;; Test if viewport only changes can be undone/redone.
  (define modify-viewport?
    (config-boolean (path-config-context (getcwd))
                    "schematic.undo"
                    "modify-viewport"))

  (define (update-window *window)
    (page_select_widget_update *window)
    (schematic_multiattrib_widget_update *window)
    (i_update_menus *window))

  (define (debug-print-undo-info *page)
    (log! 'debug "\n\n---Undo----\n")
    (lepton_undo_print_all (lepton_page_get_undo_bottom *page))
    (log! 'debug
          "TOS: ~A\n"
          (lepton_undo_get_filename (lepton_page_get_undo_tos *page)))
    (log! 'debug
          "CURRENT: ~A\n"
          (lepton_undo_get_filename (lepton_page_get_undo_current *page)))
    (log! 'debug "----\n"))

  (define (redo-action *page)
    (let ((*undo-item (lepton_page_get_undo_current *page)))
      (unless (null-pointer? *undo-item)
        (lepton_page_set_undo_current *page
                                      (lepton_undo_get_next *undo-item))
        (when (null-pointer? (lepton_page_get_undo_current *page))
          (lepton_page_set_undo_current *page
                                        (lepton_page_get_undo_tos *page))))))

  (define (undo-action *page)
    (let ((*undo-item (lepton_page_get_undo_current *page)))
      (unless (null-pointer? *undo-item)
        (lepton_page_set_undo_current *page
                                      (lepton_undo_get_prev *undo-item))
        (when (null-pointer? (lepton_page_get_undo_current *page))
          (lepton_page_set_undo_current *page
                                        (lepton_page_get_undo_bottom *page))))))

  (define (set-page-undo-structure! *page *bottom *top *current)
    (lepton_page_set_undo_bottom *page *bottom)
    (lepton_page_set_undo_tos *page *top)
    (lepton_page_set_undo_current *page *current))

  (define (restore-hierarchy-by-undo *page *undo-item)
    (lepton_page_set_page_control *page
                                  (lepton_undo_get_page_control *undo-item))
    (lepton_page_set_up *page (lepton_undo_get_up *undo-item)))

  (define (restore-viewport-by-undo *canvas *undo-item)
    (let ((*geometry (schematic_canvas_get_viewport *canvas)))
      (when (or (undo-panzoom?)
                modify-viewport?)
        (if (not (zero? (lepton_undo_get_scale *undo-item)))
            (begin
              (schematic_viewport_pan *geometry
                                      (lepton_undo_get_x *undo-item)
                                      (lepton_undo_get_y *undo-item)
                                      (lepton_undo_get_scale *undo-item))
              (schematic_canvas_invalidate_all *canvas))
            (schematic_canvas_zoom_extents *canvas
                                           (lepton_undo_get_object_list *undo-item))))))

  (define (page-undo *canvas *page *current-undo *undo-to-do)
    (let ((undo-viewport?
           (and (false? (lepton_undo_get_type *current-undo))
                (true? (lepton_undo_get_type *undo-to-do)))))
      (when undo-viewport?
        ;; Debugging stuff.
        (log! 'debug "Type: ~A\n" (lepton_undo_get_type *undo-to-do))
        (log! 'debug "Current is an undo all, next is viewport only!\n")

        ;; For only viewport changes, <undo> data omits 'filename'
        ;; or 'object_list' as they are retrieved from previous
        ;; list items.  Hence, 'filename' and 'object_list' are
        ;; not freed and just set to NULL below.
        (if (eq? (undo-type window) 'disk)
            (lepton_undo_set_filename *undo-to-do
                                      (*find-previous-filename *undo-to-do))
            (lepton_undo_set_object_list *undo-to-do
                                         (*find-previous-object-list *undo-to-do))))
      ;; Save page filename to restore it later in case a
      ;; temporary file is opened for undo.  The filename is
      ;; stored as a Scheme string as the data pointed to by
      ;; pointer returned by lepton_page_get_filename() is freed
      ;; inside lepton_page_set_filename() below, so we could get
      ;; corrupted data otherwise.
      (let ((save-filename (pointer->string (lepton_page_get_filename *page)))
            ;; Save undo structure so it's not nuked.
            (*save-undo-bottom (lepton_page_get_undo_bottom *page))
            (*save-undo-top (lepton_page_get_undo_tos *page)))
        ;; Initialize a new undo structure.
        (set-page-undo-structure! *page
                                  %null-pointer
                                  %null-pointer
                                  %null-pointer)
        ;; Unselect all objects.
        (o_select_unselect_all *window)

        (when (or (and (eq? (undo-type window) 'disk)
                       (not (null-pointer? (lepton_undo_get_filename *undo-to-do))))
                  (and (eq? (undo-type window) 'memory)
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

          (if (and (eq? (undo-type window) 'disk)
                   (not (null-pointer? (lepton_undo_get_filename *undo-to-do))))
              ;; F_OPEN_RESTORE_CWD: go back from temporary
              ;; directory, so that local config files can be
              ;; read.
              (f_open (schematic_window_get_toplevel *window)
                      *page
                      (lepton_undo_get_filename *undo-to-do)
                      F_OPEN_RESTORE_CWD
                      %null-pointer)

              ;; Otherwise objects are restored from memory.
              (when (and (eq? (undo-type window) 'memory)
                         (not (null-pointer? (lepton_undo_get_object_list *undo-to-do))))
                (lepton_page_append_list *page
                                         (lepton_object_list_copy (lepton_undo_get_object_list *undo-to-do)
                                                                  %null-pointer))))

          ;; Restore hierarchy.
          (restore-hierarchy-by-undo *page *undo-to-do)

          (schematic_window_page_content_changed *window *page)

          ;; Restore viewport size if necessary.
          (restore-viewport-by-undo *canvas *undo-to-do)

          ;; Restore logging.
          (lepton_log_set_logging_enabled save-logging?)
          ;; Set filename right.
          (lepton_page_set_filename *page
                                    (string->pointer save-filename))
          ;; Final redraw.
          (update-window *window)

          ;; Restore saved undo structures.
          (set-page-undo-structure! *page
                                    *save-undo-bottom
                                    *save-undo-top
                                    *current-undo)
          (if (true? redo?)
              (redo-action *page)
              (undo-action *page))

          ;; Don't have to free data here since 'filename' or
          ;; 'object_list' are just pointers to the real data
          ;; (lower in the stack).
          (when undo-viewport?
            (lepton_undo_set_filename *undo-to-do %null-pointer)
            (lepton_undo_set_object_list *undo-to-do %null-pointer))

          ;; Debugging stuff.
          (debug-print-undo-info *page)))))

  (define (page-undo-callback *canvas *page redo?)
    (unless (null-pointer? *page)
      (let ((*current-undo (lepton_page_get_undo_current *page)))
        (unless (null-pointer? *current-undo)
          (let ((*undo-to-do (if (true? redo?)
                                 ;; Redo action.
                                 (lepton_undo_get_next *current-undo)
                                 ;; Undo action.
                                 (lepton_undo_get_prev *current-undo))))
            (unless (null-pointer? *undo-to-do)
              (page-undo *canvas *page *current-undo *undo-to-do)))))))

  (define (canvas-undo *canvas)
    (page-undo-callback *canvas
                        (schematic_canvas_get_page *canvas)
                        redo?))

  (define (window-undo)
    (let ((*canvas (schematic_window_get_current_canvas *window)))
      (if (null-pointer? *canvas)
          (log! 'warning "undo-callback: NULL page view.")
          (canvas-undo *canvas))))

  (if undo-enabled?
      (window-undo)
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
