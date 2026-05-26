;;; Lepton EDA attribute editor
;;; Copyright (C) 2003-2010 Stuart D. Brorson.
;;; Copyright (C) 2005-2016 gEDA Contributors
;;; Copyright (C) 2017-2026 Lepton EDA Contributors
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
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

(use-modules (ice-9 getopt-long)
             (ice-9 receive)
             (rnrs bytevectors)
             (srfi srfi-1)
             (system foreign)

             (lepton config)
             (lepton ffi boolean)
             (lepton ffi glib)
             (lepton ffi gobject)
             (lepton ffi)
             (lepton file-system)
             (lepton init)
             (lepton log)
             (lepton page foreign)
             (lepton page)
             (lepton object foreign)
             (lepton object)
             (lepton rc)
             (lepton toplevel foreign)
             (lepton toplevel)
             (lepton version)

             (schematic ffi gtk)

             (attrib ffi))


(define %program-basename (basename (car (program-arguments))))
(define %theme-icon-name "lepton-attrib")


;;; Initialize liblepton library.
(init-liblepton)


;;; Localization.
(define %textdomain "libleptonattrib")
(bindtextdomain %textdomain %lepton-localedir)
(textdomain %textdomain)
(bind-textdomain-codeset %textdomain "UTF-8")
(setlocale LC_ALL "")
(setlocale LC_NUMERIC "C")

(define (G_ msg) (gettext msg %textdomain))

(define (usage)
  (format #t
          (G_ "Usage: ~A [OPTIONS] FILE ...

~A: Lepton EDA attribute editor.
Presents schematic attributes in easy-to-edit spreadsheet format.

Options:
  -v, --verbose          Verbose mode on
  -V, --version          Show version information
  -h, --help             This help menu

Report bugs at ~S
Lepton EDA homepage: ~S
")
          %program-basename
          %program-basename
          (lepton-version-ref 'bugs)
          (lepton-version-ref 'url))

  (exit 0))


(define (report-unreadable filename)
  (format (current-error-port)
          "Could not open file ~S.\n"
          filename))


(define (process-gafrc* name)
  (process-gafrc "lepton-attrib" name))


;;; Verifies the entire design by looping through all objects in
;;; the design looking for missing components, that is, those
;;; components for which no corresponding symbol files was found.
(define (design-has-missing-symbols?)
  (define (missing-symbol? object)
    ;; Look for object, and verify that it has a symbol file
    ;; attached and signal that problem exists.
    (and (component? object)
         (true? (lepton_component_object_get_missing
                 (object->pointer object)))))

  (define (page-with-missing-symbol? page)
    (any missing-symbol? (page-contents page)))

  (any page-with-missing-symbol? (active-pages)))


;;; Save main window's geometry to the cache config context.
;;; Almost the same function as in the module (schematic window).
(define (save-geometry)
  (define *main-window (attrib_get_window))
  (define (get-int bv)
    (bytevector-sint-ref bv 0 (native-endianness) (sizeof int)))
  (define x-bv (make-bytevector (sizeof int) 0))
  (define y-bv (make-bytevector (sizeof int) 0))
  (define width-bv (make-bytevector (sizeof int) 0))
  (define height-bv (make-bytevector (sizeof int) 0))

  (gtk_window_get_position *main-window
                           (bytevector->pointer x-bv)
                           (bytevector->pointer y-bv))

  (gtk_window_get_size *main-window
                       (bytevector->pointer width-bv)
                       (bytevector->pointer height-bv))

  (let ((config (cache-config-context))
        (x (get-int x-bv))
        (y (get-int y-bv))
        (width (get-int width-bv))
        (height (get-int height-bv)))
    (set-config! config "attrib.window-geometry" "x" x)
    (set-config! config "attrib.window-geometry" "y" y)
    (set-config! config "attrib.window-geometry" "width" width)
    (set-config! config "attrib.window-geometry" "height" height)

    (config-save! config)))


(define (save-page page filename)
  "Saves PAGE under FILENAME returning #t on success, or #f on
failure."
  (define *page (check-page page 1))
  (true? (f_save *page (string->pointer filename) %null-pointer)))


;;; Saves all toplevel pages.
(define (save-pages)
  (define (save page)
    (let ((filename (page-filename page)))
      (if (save-page page filename)
          (begin
            (log! 'message (G_ "Saved ~S") filename)
            ;; Reset the changed state of page.
            (set-page-dirty! page #f))

          (log! 'message (G_ "Could NOT save ~S") filename))))

  (for-each save (active-pages)))


;;; Copies data from gtksheet into LeptonToplevel struct.  The
;;; function is called when the user invokes File -> Save.  It
;;; first places all data from gtksheet into SHEET_DATA.  Then it
;;; loops through all pages and saves them.
(define (save-sheet)
  (define *toplevel (attrib_get_toplevel))

  (when (null-pointer? *toplevel)
    (error "NULL toplevel."))

  ;; Read data from gtksheet into SHEET_DATA.
  (s_sheet_data_gtksheet_to_sheetdata)

  ;; Iterate over all pages in design.
  (for-each
   (lambda (*page)
     ;; Only traverse pages which are toplevel.
     (when (zero? (lepton_page_get_page_control *page))
       ;; Add all objects from page.
       (s_toplevel_sheetdata_to_toplevel *toplevel *page)))
   (glist->list
    (lepton_list_get_glist (lepton_toplevel_get_pages *toplevel))
    identity))

  ;; Save all pages in design.
  (save-pages)
  (s_sheet_data_set_changed (attrib_get_sheet_data) FALSE))


(define (callback-file-save *action *parameter *data)
  (save-sheet))
(define *callback-file-save
  (procedure->pointer void callback-file-save '(* * *)))

(define (callback-file-export-csv *action *parameter *data)
  (menu_file_export_csv *action *parameter *data))
(define *callback-file-export-csv
  (procedure->pointer void callback-file-export-csv '(* * *)))


(define (quit-program return-code)
  "Unconditionally quit the program with the given RETURN-CODE."
  (s_clib_free)

  (unless %m4-use-gtk3
    (gtk_main_quit))

  (exit return-code))



(define (unsaved-data-dialog)
  (define *dialog (x_dialog_unsaved_data))

  (gtk_dialog_set_default_response *dialog GTK_RESPONSE_YES)

  (let ((response (gtk_dialog_run *dialog)))
    (cond
     ((= response GTK_RESPONSE_NO)
      (quit-program 0))
     ((= response GTK_RESPONSE_YES)
      (save-sheet)
      (quit-program 0))
     (else #f)))

  (gtk_widget_destroy *dialog))


;;; Quit the program using the UI. On execution, the function
;;; checks for unsaved changes before calling quit-program() to
;;; quit the program.
(define (callback-file-quit *action *parameter *data)
  (save-geometry)
  ;; Deactivate the current cell to trigger "deactivate" signal.
  ;; This allows changing of the sheet_head->CHANGED flag in the
  ;; on_deactivate() handler function if needed.
  (for-each
   (lambda (i)
     (let ((*sheet (attrib_get_sheet i)))
       (unless (null-pointer? *sheet)
         (gtk_sheet_set_active_cell *sheet -1 -1))))
   (iota (attrib_get_sheets_number)))

  (if (true? (s_sheet_data_changed (attrib_get_sheet_data)))
      (unsaved-data-dialog)
      (quit-program 0)))

(define *callback-file-quit
  (procedure->pointer void callback-file-quit '(* * *)))


(define (callback-edit-add-attrib *action *parameter *data)
  (menu_edit_newattrib *action *parameter *data))
(define *callback-edit-add-attrib
  (procedure->pointer void callback-edit-add-attrib '(* * *)))

(define (callback-edit-delete-attrib *action *parameter *data)
  (menu_edit_delattrib *action *parameter *data))
(define *callback-edit-delete-attrib
  (procedure->pointer void callback-edit-delete-attrib '(* * *)))

(define (callback-visibility-invisible *action *parameter *data)
  (s_visibility_set_invisible *action *parameter *data))
(define *callback-visibility-invisible
  (procedure->pointer void callback-visibility-invisible '(* * *)))

(define (callback-visibility-name-only *action *parameter *data)
  (s_visibility_set_name_only *action *parameter *data))
(define *callback-visibility-name-only
  (procedure->pointer void callback-visibility-name-only '(* * *)))

(define (callback-visibility-value-only *action *parameter *data)
  (s_visibility_set_value_only *action *parameter *data))
(define *callback-visibility-value-only
  (procedure->pointer void callback-visibility-value-only '(* * *)))

(define (callback-visibility-name-value *action *parameter *data)
  (s_visibility_set_name_and_value *action *parameter *data))
(define *callback-visibility-name-value
  (procedure->pointer void callback-visibility-name-value '(* * *)))

(define (callback-help-about *action *parameter *data)
  (x_dialog_about_dialog *action *parameter *data))
(define *callback-help-about
  (procedure->pointer void callback-help-about '(* * *)))


(define (init-callbacks)
  (attrib_window_set_menu_callback (string->pointer "file-save")
                                   *callback-file-save)
  (attrib_window_set_menu_callback (string->pointer "file-export-csv")
                                   *callback-file-export-csv)
  (attrib_window_set_menu_callback (string->pointer "file-quit")
                                   *callback-file-quit)
  (attrib_window_set_menu_callback (string->pointer "edit-add-attrib")
                                   *callback-edit-add-attrib)
  (attrib_window_set_menu_callback (string->pointer "edit-delete-attrib")
                                   *callback-edit-delete-attrib)
  (attrib_window_set_menu_callback (string->pointer "visibility-invisible")
                                   *callback-visibility-invisible)
  (attrib_window_set_menu_callback (string->pointer "visibility-name-only")
                                   *callback-visibility-name-only)
  (attrib_window_set_menu_callback (string->pointer "visibility-value-only")
                                   *callback-visibility-value-only)
  (attrib_window_set_menu_callback (string->pointer "visibility-name-value")
                                   *callback-visibility-name-value)
  (attrib_window_set_menu_callback (string->pointer "help-about")
                                   *callback-help-about))

(define (init-window)
  (define *window-widget (attrib_get_window))

  (g_signal_connect *window-widget
                    (string->pointer "delete_event")
                    *callback-file-quit
                    %null-pointer)

  ;; Init menu functions.
  (init-callbacks)

  (x_window_init))


(define (activate *app *toplevel)
  (define *window-widget (attrib_window_new *app))
  (define *sheet-data (s_sheet_data_new))
  (define *pages
    (lepton_list_get_glist (lepton_toplevel_get_pages *toplevel)))

  (attrib_set_window *window-widget)

  (attrib_set_toplevel *toplevel)

  ;; Initialize GTK window.
  (init-window)

  ;; Initialize main sheet data structure.
  (attrib_set_sheet_data *sheet-data)

  (for-each
   (lambda (*page)
     (lepton_toplevel_set_page_current *toplevel *page)

     ;; Now add all items found to the master lists
     (s_sheet_data_add_master_comp_list_items
      (lepton_page_objects *page))
     (s_sheet_data_add_master_comp_attrib_list_items
      (lepton_page_objects *page))
     ;; Note that this must be changed.  We need to input the
     ;; entire project before doing anything with the nets because
     ;; we need to first determine where they are all connected!
     (when #f
       (s_sheet_data_add_master_net_list_items
        (lepton_page_objects *page))
       (s_sheet_data_add_master_net_attrib_list_items
        (lepton_page_objects *page)))

     (s_sheet_data_add_master_pin_list_items
      (lepton_page_objects *page))
     (s_sheet_data_add_master_pin_attrib_list_items
      (lepton_page_objects *page)))

   (glist->list *pages identity))

  ;; Sort the master lists.
  (s_string_list_sort_master_comp_list)
  (s_string_list_sort_master_comp_attrib_list)

  (when #f
    ;; Note that this must be changed.  We need to input the
    ;; entire project before doing anything with the nets because
    ;; we need to first determine where they are all connected!
    (s_string_list_sort_master_net_list)
    (s_string_list_sort_master_net_attrib_list))

  (s_string_list_sort_master_pin_list)
  (s_string_list_sort_master_pin_attrib_list)

  ;; Create and load the tables.
  (attrib_sheet_data_set_component_table
   *sheet-data
   (s_table_new (attrib_sheet_data_get_component_count *sheet-data)
                (attrib_sheet_data_get_component_attrib_count *sheet-data)))
  (attrib_sheet_data_set_net_table
   *sheet-data
   (s_table_new (attrib_sheet_data_get_net_count *sheet-data)
                (attrib_sheet_data_get_net_attrib_count *sheet-data)))
  (attrib_sheet_data_set_pin_table
   *sheet-data
   (s_table_new (attrib_sheet_data_get_pin_count *sheet-data)
                (attrib_sheet_data_get_pin_attrib_count *sheet-data)))

  ;; Must iterate over all pages in design.
  (for-each
   (lambda (*page)
     ;; Only traverse pages which are toplevel.
     (when  (zero? (lepton_page_get_page_control *page))
       ;; Adds all components from page to the component table.
       (s_table_add_toplevel_comp_items_to_comp_table (lepton_page_objects *page))
       (when #f
         ;; Note that this must be changed.  We need to input the
         ;; entire project before doing anything with the nets
         ;; because we need to first determine where they are all
         ;; connected!

         ;; Adds all nets from page to the net table.
         (s_table_add_toplevel_net_items_to_net_table (lepton_page_objects *page)))

       ;; Adds all pins from page to the pin table.
       (s_table_add_toplevel_pin_items_to_pin_table (lepton_page_objects *page))))

   (glist->list *pages identity))

  ;; Update windows.
  ;; This updates the top level stuff, and then calls another
  ;; function to update the GtkSheet itself.
  (x_window_add_items)
  ;; Verify correctness of entire design.
  (when (design-has-missing-symbols?)
    ;; Dialog gives user option to quit.
    (x_dialog_missing_sym))

  ;; Set the main window's title.
  (let ((page-count (length (active-pages))))
    (gtk_window_set_title
     *window-widget
     (string->pointer
      (cond
       ((= page-count 1)
        (string-append (basename (page-filename (car (active-pages))))
                       " - "
                       %program-basename))
       ((> page-count 1)
        (string-append (G_ "Multiple files") " - " %program-basename))
       (else %program-basename)))))

  ;; Set default icon.
  (gtk_window_set_default_icon_name (string->pointer %theme-icon-name))

  (gtk_widget_show_all *window-widget))


;;; Init logging.
(init-log "attrib")
(display-lepton-version #:print-name #t #:log #t)


(let* ((option-spec '((help (single-char #\h))
                      (verbose (single-char #\v))
                      (version (single-char #\V))))

       (options (getopt-long (program-arguments) option-spec))
       (help (option-ref options 'help #f))
       (version (option-ref options 'version #f))
       (files (option-ref options '() '()))
       (verbose? (option-ref options 'verbose #f)))

  (when help (usage))
  ;; Output version to stdout and exit, if requested.
  (when version
    (display-lepton-version #:print-name #t #:copyright #t)
    (exit 0))
  (when verbose? (set_verbose_mode))

  (receive (readable-files unreadable-files)
      (partition file-readable? files)
    (if (null? unreadable-files)
        ;; Main procedure.
        (begin
          ;; Initialize GTK.
          (gtk_init %null-pointer %null-pointer)
          (let ((files (if (null? readable-files)
                           ;; No files specified on the command
                           ;; line, pop up the File open dialog.
                           (gslist->list (x_fileselect_open) pointer->string 'free)
                           readable-files)))
            (if (null? files)
                (exit 0)
                (with-toplevel (make-toplevel)
                 (lambda ()
                   (for-each process-gafrc* files)
                   ;; Open all files.
                   (for-each file->page files)
                   ;; Run attribute editor.
                   (exit (if %m4-use-gtk3
                             (attrib_run (procedure->pointer void activate '(* *))
                                         (toplevel->pointer (current-toplevel)))
                             (let ((*toplevel (toplevel->pointer (current-toplevel))))
                               (activate %null-pointer *toplevel)
                               (attrib_run %null-pointer *toplevel)))))))))

        ;; There are non-existing or unreadable files.  Report and
        ;; exit.
        (begin
          (for-each report-unreadable unreadable-files)
          (exit 1)))))
