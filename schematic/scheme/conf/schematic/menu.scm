;
; Here are the definitions for the top pull down menu bar
;
; The "menu item name" is the name of the item as it will appear in the menu
; The "menu action" is the scheme function which is executed when the item
; is selected off of the menu.
;
; The hotkeys which are displayed are defined by the global-keymap.
; Actions can have several hotkeys, but the displayed keys are the last
; ones found.
;
; The SEPARATOR keyword is case sensitive and puts a separator into the menu.
;

(or (defined? 'define-syntax)
    (use-modules (ice-9 syncase)))

;; Define a no-op macro for flagging strings as translatable.
(define-syntax N_
  (syntax-rules ()
    ((N_ expr) expr)))

(define file-menu-items
;;
;;          menu item name      menu action             menu stock icon
;;
        `( (,(N_ "_New")              &file-new               "gtk-new")
           (,(N_ "_Open...")          &file-open              "gtk-open")
;; The entry below will be removed from the menu if glib < 2.6 is detected
           (,(N_ "Open Recen_t")      #f                      #f)
           ("SEPARATOR"               #f                      #f)
           (,(N_ "_Save")             &file-save              "gtk-save")
           (,(N_ "Save _As...")       &file-save-as           "gtk-save-as")
           (,(N_ "Save All")          &file-save-all          "gtk-save")
           ("SEPARATOR"               #f                      #f)
           (,(N_ "_Print...")         &file-print             "gtk-print")
           (,(N_ "Write _Image...")   &file-image             #f)
           ("SEPARATOR"               #f                      #f)
           (,(N_ "Invoke Macro...")   &edit-invoke-macro      "gtk-execute")
           (,(N_ "Execute Script...") &file-script            "gtk-execute")
           (,(N_ "REPL...")           &file-repl              "gtk-execute")
           ("SEPARATOR"               #f                      #f)
           (,(N_ "New Window")        &file-new-window        "window-new")
           (,(N_ "_Close Window")     &file-close-window      "gtk-close")
           (,(N_ "_Quit")             &file-quit              "gtk-quit")))

(define edit-menu-items
;;
;;          menu item name      menu action             menu stock icon
;;
        `( (,(N_ "_Undo")              &edit-undo             "gtk-undo")
           (,(N_ "_Redo")              &edit-redo             "gtk-redo")
           ("SEPARATOR"                #f                     #f)
           (,(N_ "Cu_t")               &clipboard-cut         "gtk-cut")
           (,(N_ "_Copy")              &clipboard-copy        "gtk-copy")
           (,(N_ "_Paste")             &clipboard-paste       "gtk-paste")
           (,(N_ "_Delete")            &edit-delete           "gtk-delete"  )
           ("SEPARATOR"                #f                     #f)
           (,(N_ "Select Mode")        &edit-select           "select")
           (,(N_ "Select All")         &edit-select-all       "gtk-select-all")
           (,(N_ "Deselect")           &edit-deselect         "deselect")
           (,(N_ "Copy Mode")          &edit-copy             "clone")
           (,(N_ "Multiple Copy Mode") &edit-mcopy            "multi-clone")
           (,(N_ "Move Mode")          &edit-move             #f)
           (,(N_ "Rotate 90 Mode")     &edit-rotate-90        "object-rotate-left")
           (,(N_ "Mirror Mode")        &edit-mirror           "object-flip-horizontal")
           ("SEPARATOR"                #f                     #f)
           (,(N_ "Object Properties...") &edit-object-properties "gtk-properties")
           (,(N_ "Edit...")            &edit-edit             #f)
           (,(N_ "Edit Text...")       &edit-text             "gtk-edit")
           (,(N_ "Slot...")            &edit-slot             #f)
           ("SEPARATOR"                #f                     #f)
           (,(N_ "Lock")               &edit-lock             #f)
           (,(N_ "Unlock")             &edit-unlock           #f)
           ("SEPARATOR"                #f                     #f)
           (,(N_ "Embed Component/Picture")    &edit-embed    #f)
           (,(N_ "Unembed Component/Picture")  &edit-unembed  #f)
           (,(N_ "Update Component")   &edit-update           "gtk-refresh")
           (,(N_ "Symbol Translate...")  &edit-translate      #f)))

(define buffer-menu-items
;;
;;          menu item name      menu action             menu stock icon
;;
	`( (,(N_ "Copy into 1")	buffer-copy1    "gtk-copy")
	   (,(N_ "Copy into 2")	buffer-copy2    "gtk-copy")
	   (,(N_ "Copy into 3")	buffer-copy3    "gtk-copy")
	   (,(N_ "Copy into 4")	buffer-copy4    "gtk-copy")
	   (,(N_ "Copy into 5")	buffer-copy5    "gtk-copy")
	   (,(N_ "Cut into 1")	buffer-cut1       	"gtk-cut")
	   (,(N_ "Cut into 2")	buffer-cut2       	"gtk-cut")
	   (,(N_ "Cut into 3")	buffer-cut3       	"gtk-cut")
	   (,(N_ "Cut into 4")	buffer-cut4       	"gtk-cut")
	   (,(N_ "Cut into 5")	buffer-cut5       	"gtk-cut")
	   (,(N_ "Paste from 1")	buffer-paste1   "gtk-paste")
	   (,(N_ "Paste from 2")	buffer-paste2   "gtk-paste")
	   (,(N_ "Paste from 3")	buffer-paste3   "gtk-paste")
	   (,(N_ "Paste from 4")	buffer-paste4   "gtk-paste")
	   (,(N_ "Paste from 5")	buffer-paste5   "gtk-paste")))

(define view-menu-items
;;
;;          menu item name        menu action             menu stock icon
;;
        `( (,(N_ "Side Dock")           &view-sidebar           #f)
           (,(N_ "Bottom Dock")         &view-status            #f)
           ("SEPARATOR"                 #f                      #f)
           (,(N_ "Find Text Results")   &view-find-text-state   #f)
           ("SEPARATOR"                 #f                      #f)
           (,(N_ "_Redraw")             &view-redraw            "gtk-refresh")
           (,(N_ "_Pan")                &view-pan               #f)
           (,(N_ "Zoom _Box")           &view-zoom-box          #f)
           (,(N_ "Zoom _Extents")       &view-zoom-extents      "gtk-zoom-fit")
           (,(N_ "Zoom _In")            &view-zoom-in           "gtk-zoom-in")
           (,(N_ "Zoom _Out")           &view-zoom-out          "gtk-zoom-out")
           (,(N_ "Zoom _Full")          &view-zoom-full         #f)
           ("SEPARATOR"                 #f                      #f)
           (,(N_ "_Dark Color Scheme")  &view-dark-colors       #f)
           (,(N_ "_Light Color Scheme") &view-light-colors      #f)
           (,(N_ "B_W Color Scheme")    &view-bw-colors         #f)
           ("SEPARATOR"                 #f                      #f)
           (,(N_ "Color Scheme Editor...") &view-color-edit     #f)
         ))

(define page-menu-items
;;
;;          menu item name      menu action             menu stock icon
;;
        `( (,(N_ "_Manager...")       &page-manager           #f)
           ("SEPARATOR"               #f                      #f)
           (,(N_ "_Previous")         &page-prev              "gtk-go-back")
           (,(N_ "_Next")             &page-next              "gtk-go-forward")
           (,(N_ "_Close")            &page-close             "gtk-close")
           ("SEPARATOR"               #f                      #f)
           (,(N_ "_Revert...")        &page-revert            "gtk-revert-to-saved")
           ("SEPARATOR"               #f                      #f)
           (,(N_ "Next Tab")          &page-next-tab          "gtk-go-forward")
           (,(N_ "Previous Tab")      &page-prev-tab          "gtk-go-back")
        )
)

(define add-menu-items
;;
;;          menu item name      menu action             menu stock icon
;;
        `( (,(N_ "_Component...")     &add-component   "insert-symbol")
           ("SEPARATOR"              #f                #f)
           (,(N_ "_Net")              &add-net         "insert-net")
           (,(N_ "B_us")              &add-bus         "insert-bus")
           ("SEPARATOR"              #f                #f)
           (,(N_ "_Attribute...")     &add-attribute   "insert-attribute")
           (,(N_ "_Text...")          &add-text        "insert-text")
           ("SEPARATOR"              #f                #f)
           (,(N_ "_Line")             &add-line        "insert-line")
           (,(N_ "Pat_h")             &add-path        "insert-path")
           (,(N_ "_Box")              &add-box         "insert-box")
           (,(N_ "C_ircle")           &add-circle      "insert-circle")
           (,(N_ "A_rc")              &add-arc         "insert-arc")
           (,(N_ "_Pin")              &add-pin         "insert-pin")
           ("SEPARATOR"              #f                #f)
           (,(N_ "Pictu_re...")       &add-picture     "insert-image")))

(define hierarchy-menu-items
;;
;;          menu item name      menu action               menu stock icon
;;
        `( (,(N_ "_Up")               &hierarchy-up             "gtk-go-up")
           ("SEPARATOR"              #f                #f)
           (,(N_ "_Down Schematic")   &hierarchy-down-schematic "gtk-go-down")
           (,(N_ "Down _Symbol")      &hierarchy-down-symbol    "gtk-goto-bottom")))

(define attributes-menu-items
;;
;;          menu item name      menu action             menu stock icon
;;
        `( (,(N_ "_Attach")           &attributes-attach      "attribute-attach")
           (,(N_ "_Detach")           &attributes-detach      "attribute-detach")
           ("SEPARATOR" #f #f)
           (,(N_ "Show _Value")       &attributes-show-value  "attribute-show-value")
           (,(N_ "Show _Name")        &attributes-show-name   "attribute-show-name")
           (,(N_ "Show _Both")        &attributes-show-both   "attribute-show-both")
           (,(N_ "_Toggle Visibility")  &attributes-visibility-toggle   #f)
           ("SEPARATOR" #f #f)
           (,(N_ "_Hide Specific Text...") &edit-hide-text   #f)
           (,(N_ "_Show Specific Text...") &edit-show-text   #f)
           (,(N_ "Show/Hide Hidden Text")  &edit-show-hidden #f)
           ("SEPARATOR" #f #f)
           (,(N_ "_Find Text/Check Symbol...") &edit-find-text   "gtk-find")
           (,(N_ "A_utonumber Text...")        &edit-autonumber  #f)))

(define options-menu-items
;;
;;          menu item name      menu action             menu stock icon
;;
        `( (,(N_ "_Options...") &options-snap-size)
           (,(N_ "_Font...")    &options-select-font)
           ("SEPARATOR" #f #f)
           (,(N_ "Grid +")      &options-scale-up-snap-size)
           (,(N_ "Grid -")      &options-scale-down-snap-size)
           ("SEPARATOR" #f #f)
           (,(N_ "Grid Style: Cycle Dots/Mesh/Off")  &options-grid)
           (,(N_ "Grid Snap: Cycle Grid/Resnap/Off") &options-snap)
           ("SEPARATOR" #f #f)
           (,(N_ "Feedback Mode: Outline/Box") &options-action-feedback)
           (,(N_ "Net: Rubberband On/Off")     &options-rubberband)
           (,(N_ "Net: Magnetic On/Off")       &options-magneticnet)
           ("SEPARATOR" #f #f)
           (,(N_ "_Coord Window") &options-show-coord-window)
           (,(N_ "_Log Window")   &options-show-log-window)))

(define netlist-menu-items
  ;; Format of list items is:
  ;; name action icon
  `(
    (,(N_ "_1 allegro") &netlist-allegro #f))
  )

(define help-menu-items
;;
;;          menu item name                menu action               menu stock icon
;;
        `(
           (,(N_ "User _Guide")  &help-guide                "gtk-help")
           (,(N_ "_FAQ")         &help-faq                  "help-faq")
           (,(N_ "Docu_mentation") &help-manual               "help-browser")
           (,(N_ "_Wiki")          &help-wiki                 "web-browser")
           ("SEPARATOR"                   #f                          #f)
           (,(N_ "Find Component D_ocumentation") &hierarchy-documentation "symbol-datasheet")
           ("SEPARATOR"                   #f                          #f)
           (,(N_ "_Hotkeys...")            &help-hotkeys              "preferences-desktop-keyboard-shortcuts")
           (,(N_ "_About")              &help-about                "gtk-about")))

;
; Now actually add the menus.  The order here defines the order in which
; the menus appear in the top menu bar.
;
(add-menu (N_ "_File") file-menu-items)
(add-menu (N_ "_Edit") edit-menu-items)
;(add-menu (N_ "_Buffer") buffer-menu-items)
(add-menu (N_ "_View") view-menu-items)
(add-menu (N_ "_Page") page-menu-items)
(add-menu (N_ "_Add") add-menu-items)
(add-menu (N_ "Hie_rarchy") hierarchy-menu-items)
(add-menu (N_ "A_ttributes") attributes-menu-items)
(add-menu (N_ "_Options") options-menu-items)
(add-menu (N_ "_Netlist") netlist-menu-items)
(add-menu (N_ "_Help") help-menu-items)

