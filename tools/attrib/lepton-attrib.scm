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
             (srfi srfi-1)
             (system foreign)

             (lepton ffi boolean)
             (lepton ffi glib)
             (lepton ffi)
             (lepton file-system)
             (lepton init)
             (lepton log)
             (lepton page)
             (lepton object foreign)
             (lepton object)
             (lepton rc)
             (lepton toplevel foreign)
             (lepton toplevel)
             (lepton version)

             (schematic ffi gtk)

             (attrib ffi))

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

lepton-attrib: Lepton EDA attribute editor.
Presents schematic attributes in easy-to-edit spreadsheet format.

Options:
  -v, --verbose          Verbose mode on
  -V, --version          Show version information
  -h, --help             This help menu

Report bugs at ~S
Lepton EDA homepage: ~S
")
          (basename (car (program-arguments)))
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
(define (verify-design *toplevel)
  (when (any
         (lambda (*page)
           (any
            (lambda (*object)
              ;; Look for object, and verify that it has a symbol
              ;; file attached and signal that problem exists.
              (and (component? (pointer->object *object))
                   (true? (lepton_component_object_get_missing *object))))
            (glist->list (lepton_page_objects *page) identity)))
         (glist->list (lepton_list_get_glist
                       (lepton_toplevel_get_pages *toplevel))
                      identity))
    ;; Dialog gives user option to quit.
    (x_dialog_missing_sym)))


(define (activate *app *toplevel)
  (define *window-widget (attrib_window_new *app))
  (define *sheet-data (s_sheet_data_new))
  (define *pages
    (lepton_list_get_glist (lepton_toplevel_get_pages *toplevel)))

  (attrib_set_window *window-widget)

  (attrib_set_toplevel *toplevel)

  ;; Initialize GTK window.
  (x_window_init)

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
  (verify-design *toplevel)

  (x_window_set_title *pages)

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
