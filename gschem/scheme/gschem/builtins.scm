;; gEDA - GPL Electronic Design Automation
;; gschem - gEDA Schematic Capture - Scheme API
;; Copyright (C) 2013 Peter Brett <peter@peter-b.co.uk>
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
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA
;;

(define-module (gschem builtins)
  #:use-module (geda object)
  #:use-module (gschem core gettext)
  #:use-module (gschem core builtins)
  #:use-module (gschem action)
  #:use-module (gschem gschemdoc)
  #:use-module (gschem selection)
  #:use-module (gschem window))

(or (defined? 'define-syntax)
    (use-modules (ice-9 syncase)))

(define-syntax define-action-public
  (syntax-rules ()
    ((_ (name . args) . forms)
     (begin
       (define-action (name . args) . forms)
       (export name)))))

;; -------------------------------------------------------------------
;;;; File menu actions

(define-action-public (&file-new #:label (_ "New File") #:icon "gtk-new")
  (%file-new))

(define-action-public (&file-open #:label (_ "Open File") #:icon "gtk-open")
  (%file-open))

(define-action-public (&file-save #:label (_ "Save") #:icon "gtk-save")
  (%file-save))

(define-action-public (&file-save-as #:label (_ "Save As") #:icon "gtk-save-as")
  (%file-save-as))

(define-action-public (&file-save-all #:label (_ "Save All") #:icon "gtk-save")
  (%file-save-all))

(define-action-public (&file-print #:label (_ "Print") #:icon "gtk-print")
  (%file-print))

(define-action-public (&file-image #:label (_ "Export Image"))
  (%file-image))

(define-action-public (&file-script #:label (_ "Run Script") #:icon "gtk-execute")
  (%file-script))

(define-action-public (&file-new-window #:label (_ "New Window") #:icon "window-new")
  (%file-new-window))

(define-action-public (&file-close-window #:label (_ "Close Window") #:icon "gtk-close")
  (%file-close-window))

(define-action-public (&file-quit #:label (_ "Quit") #:icon "gtk-quit")
  (%file-quit))

;; -------------------------------------------------------------------
;;;; General editing actions

(define-action-public (&edit-undo #:label (_ "Undo") #:icon "gtk-undo")
  (%edit-undo))

(define-action-public (&edit-redo #:label (_ "Redo") #:icon "gtk-redo")
  (%edit-redo))

(define-action-public (&edit-select #:label (_ "Select Mode") #:icon "select")
  (%edit-select))

(define-action-public (&edit-select-all #:label (_ "Select All") #:icon "gtk-select-all")
  (%edit-select-all))

(define-action-public (&edit-deselect #:label (_ "Deselect"))
  (%edit-deselect))

(define-action-public (&edit-delete #:label (_ "Delete") #:icon "gtk-delete")
  (%edit-delete))

(define-action-public (&edit-move #:label (_ "Move Mode"))
  (%edit-move))

(define-action-public (&edit-copy #:label (_ "Copy Mode") #:icon "clone")
  (%edit-copy))

(define-action-public (&edit-mcopy #:label (_ "Multiple Copy Mode") #:icon "multi-clone")
  (%edit-mcopy))

(define-action-public (&edit-rotate-90 #:label (_ "Rotate Mode") #:icon "object-rotate-left")
  (%edit-rotate-90))

(define-action-public (&edit-mirror #:label (_ "Mirror Mode") #:icon "object-flip-horizontal")
  (%edit-mirror))

(define-action-public (&edit-edit #:label (_ "Edit..."))
  (%edit-edit))

(define-action-public (&edit-text #:label (_ "Edit Text") #:icon "gtk-edit")
  (%edit-text))

(define-action-public (&edit-slot #:label (_ "Choose Slot"))
  (%edit-slot))

(define-action-public (&edit-color #:label (_ "Edit Color") #:icon "gtk-select-color")
  (%edit-color))

(define-action-public (&edit-linetype #:label (_ "Edit Line Width & Type"))
  (%edit-linetype))

(define-action-public (&edit-filltype #:label (_ "Edit Fill Type"))
  (%edit-filltype))

(define-action-public (&edit-pin-type #:label (_ "Edit Pin Type"))
  (%edit-pin-type))

(define-action-public (&edit-translate #:label (_ "Translate Symbol"))
  (%edit-translate))

(define-action-public (&edit-lock #:label (_ "Lock"))
  (%edit-lock))

(define-action-public (&edit-unlock #:label (_ "Unlock"))
  (%edit-unlock))

(define-action-public (&edit-invoke-macro #:label (_ "Invoke Macro"))
  (%edit-invoke-macro))

(define-action-public (&edit-embed #:label (_ "Embed Component/Pictore"))
  (%edit-embed))

(define-action-public (&edit-unembed #:label (_ "Unembed Component/Picture"))
  (%edit-unembed))

(define-action-public (&edit-update #:label (_ "Update Component") #:icon "gtk-refresh")
  (%edit-update))

(define-action-public (&edit-show-hidden #:label (_ "Show/Hide Invisible Text"))
  (%edit-show-hidden))

;; -------------------------------------------------------------------
;;;; Clipboard actions

(define-action-public (&clipboard-cut #:label (_ "Cut") #:icon "gtk-cut")
  (%clipboard-cut))

(define-action-public (&clipboard-copy #:label (_ "Copy") #:icon "gtk-copy")
  (%clipboard-copy))

(define-action-public (&clipboard-paste #:label (_ "Paste") #:icon "gtk-paste")
  (%clipboard-paste))

;; -------------------------------------------------------------------
;;;; View control actions

(define-action-public (&view-redraw #:label (_ "Redraw") #:icon "gtk-refresh")
  (%view-redraw))

(define-action-public (&view-pan #:label (_ "Pan"))
  (%view-pan))

(define-action-public (&view-pan-left #:label (_ "Pan Left"))
  (%view-pan-left))

(define-action-public (&view-pan-right #:label (_ "Pan Right"))
  (%view-pan-right))

(define-action-public (&view-pan-up #:label (_ "Pan Up"))
  (%view-pan-up))

(define-action-public (&view-pan-down #:label (_ "Pan Down"))
  (%view-pan-down))

(define-action-public (&view-zoom-box #:label (_ "Zoom Box"))
  (%view-zoom-box))

(define-action-public (&view-zoom-extents #:label (_ "Zoom Extents") #:icon "gtk-zoom-fit")
  (%view-zoom-extents))

(define-action-public (&view-zoom-in #:label (_ "Zoom In") #:icon "gtk-zoom-in")
  (%view-zoom-in))

(define-action-public (&view-zoom-out #:label (_ "Zoom Out") #:icon "gtk-zoom-out")
  (%view-zoom-out))

(define-action-public (&view-zoom-full #:label (_ "Zoom Full"))
  (%view-zoom-full))

;; -------------------------------------------------------------------
;;;; Page-related actions

(define-action-public (&page-revert #:label (_ "Revert Changes") #:icon "gtk-revert-to-saved")
  (%page-revert))

(define-action-public (&page-manager #:label (_ "Page Manager"))
  (%page-manager))

(define-action-public (&page-prev #:label (_ "Previous Page") #:icon "gtk-go-back")
  (%page-prev))

(define-action-public (&page-next #:label (_ "Next Page") #:icon "gtk-go-forward")
  (%page-next))

(define-action-public (&page-close #:label (_ "Close Page") #:icon "gtk-close")
  (%page-close))

;; -------------------------------------------------------------------
;;;; Actions related to adding things

(define-action-public (&add-component #:label (_ "Add Component") #:icon "insert-symbol")
  (%add-component))

(define-action-public (&add-attribute #:label (_ "Add Attribute") #:icon "insert-attribute")
  (%add-attribute))

(define-action-public (&add-net #:label (_ "Add Net") #:icon "insert-net")
  (%add-net))

(define-action-public (&add-bus #:label (_ "Add Bus") #:icon "insert-bus")
  (%add-bus))

(define-action-public (&add-text #:label (_ "Add Text") #:icon "insert-text")
  (%add-text))

(define-action-public (&add-line #:label (_ "Add Line") #:icon "insert-line")
  (%add-line))

(define-action-public (&add-path #:label (_ "Add Path") #:icon "insert-path")
  (%add-path))

(define-action-public (&add-box #:label (_ "Add Box") #:icon "insert-box")
  (%add-box))

(define-action-public (&add-circle #:label (_ "Add Circle") #:icon "insert-circle")
  (%add-circle))

(define-action-public (&add-arc #:label (_ "Add Arc") #:icon "insert-arc")
  (%add-arc))

(define-action-public (&add-pin #:label (_ "Add Pin") #:icon "insert-pin")
  (%add-pin))

(define-action-public (&add-picture #:label (_ "Add Picture") #:icon "insert-image")
  (%add-picture))

;; -------------------------------------------------------------------
;;;; Configuration actions

(define-action-public (&help-hotkeys #:label (_ "Show Hotkeys") #:icon "preferences-desktop-keyboard-shortcuts")
  (%help-hotkeys))

;; -------------------------------------------------------------------
;;;; Documentation-related actions

(define-action-public
    (&hierarchy-documentation #:label (_ "Component Documentation")
                              #:icon "symbol-datasheet"
                              #:tooltip (_ "View documentation for selected component"))

  "If a component is selected, search for and display corresponding
documentation in a browser or PDF viewer. If no documentation can be
found, shows a dialog with an error message."

  (catch 'misc-error

   (lambda ()
     (let ((component
            (any (lambda (obj) (and (component? obj) obj))
                 (page-selection (active-page)))))
       (and component (show-component-documentation component))))

   (lambda (key subr msg args . rest)
     (gschem-msg (string-append
                  (_ "Could not show documentation for selected component:\n\n")
                  (apply format #f msg args))))))


(define-action-public
    (&help-manual #:label (_ "gEDA Manuals") #:icon "help-browser"
     #:tooltip (_ "View the front page of the gEDA documention in a browser."))
  (show-wiki "geda:documentation"))


(define-action-public
    (&help-guide #:label (_ "gschem User Guide") #:icon "gtk-help"
                 #:tooltip (_ "View the gschem User Guide in a browser."))
  (show-wiki "geda:gschem_ug"))


(define-action-public
    (&help-faq #:label (_ "gschem FAQ") #:icon "help-faq"
     #:tooltip (_ "Frequently Asked Questions about using gschem."))
  (show-wiki "geda:faq-gschem"))


(define-action-public
    (&help-wiki #:label (_ "gEDA wiki") #:icon "web-browser"
     #:tooltip (_ "View the front page of the gEDA wiki in a browser."))
  (show-wiki))


(define-action-public (&help-about #:label (_ "About gschem") #:icon "gtk-about")
  (%help-about))

;; Local Variables:
;; eval: (put 'define-action-public 'scheme-indent-function 1)
;; End:
