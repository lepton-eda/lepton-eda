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

(use-modules (ice-9 format)
             (lepton config)
             (schematic menu))

;; Define a no-op macro for flagging strings as translatable.
(define-syntax N_
  (syntax-rules ()
    ((N_ expr) expr)))


( define file-menu-items
( list
;;
;;       menu item name           menu action              menu stock icon
;;
  ( list (N_ "_New")              '&file-new               "gtk-new"     )
  ( list (N_ "_Open...")          '&file-open              "gtk-open"    )
  ( list (N_ "Open Recen_t")      #f                       #f            )
  ( list "SEPARATOR"              #f                       #f            )
  ( list (N_ "_Save")             '&file-save              "gtk-save"    )
  ( list (N_ "Save _As...")       '&file-save-as           "gtk-save-as" )
  ( list (N_ "Save All")          '&file-save-all          "gtk-save"    )
  ( list "SEPARATOR"              #f                       #f            )
  ( list (N_ "_Print...")         '&file-print             "gtk-print"   )
  ( list (N_ "Write _Image...")   '&file-image             #f            )
  ( list "SEPARATOR"              #f                       #f            )
  ( list (N_ "Invoke Macro...")   '&edit-invoke-macro      "gtk-execute" )
  ( list (N_ "Execute Script...") '&file-script            "gtk-execute" )
  ( list (N_ "REPL...")           '&file-repl              "gtk-execute" )
  ( list "SEPARATOR"              #f                       #f )
  ( list (N_ "New Window")        '&file-new-window        "window-new"  )
  ( list (N_ "_Close Window")     '&file-close-window      "gtk-close"   )
  ( list (N_ "_Quit")             '&file-quit              "gtk-quit"    )
)
) ; file-menu-items


( define edit-menu-items
( list
  ( list (N_ "_Undo")              '&edit-undo       "gtk-undo" )
  ( list (N_ "_Redo")              '&edit-redo       "gtk-redo" )
  ( list "SEPARATOR" #f #f )
  ( list (N_ "Cu_t")               '&clipboard-cut   "gtk-cut" )
  ( list (N_ "_Copy")              '&clipboard-copy  "gtk-copy" )
  ( list (N_ "_Paste")             '&clipboard-paste "gtk-paste" )
  ( list (N_ "_Delete")            '&edit-delete     "gtk-delete" )
  ( list "SEPARATOR" #f #f )
  ( list (N_ "Select Mode")        '&edit-select     "select" )
  ( list (N_ "Select All")         '&edit-select-all "gtk-select-all" )
  ( list (N_ "Deselect")           '&edit-deselect   "deselect" )
  ( list (N_ "Copy Mode")          '&edit-copy       "clone" )
  ( list (N_ "Multiple Copy Mode") '&edit-mcopy      "multi-clone" )
  ( list (N_ "Move Mode")          '&edit-move       #f)
  ( list (N_ "Rotate 90 Mode")     '&edit-rotate-90  "object-rotate-left" )
  ( list (N_ "Mirror Mode")        '&edit-mirror     "object-flip-horizontal" )
  ( list "SEPARATOR" #f #f )
  ( list (N_ "Object Properties...") '&edit-object-properties "gtk-properties" )
  ( list (N_ "Edit...")              '&edit-edit              #f )
  ( list (N_ "Edit Text...")         '&edit-text              "gtk-edit" )
  ( list (N_ "Slot...")              '&edit-slot              #f )
  ( list "SEPARATOR" #f #f )
  ( list (N_ "Lock")                 '&edit-lock              #f )
  ( list (N_ "Unlock")               '&edit-unlock            #f )
  ( list "SEPARATOR" #f #f )
  ( list (N_ "Embed Component/Picture")   '&edit-embed        #f )
  ( list (N_ "Unembed Component/Picture") '&edit-unembed      #f )
  ( list (N_ "Update Component")          '&edit-update       "gtk-refresh" )
  ( list (N_ "Symbol Translate...")       '&edit-translate    #f )
)
) ; edit-menu-items


( define buffer-menu-items
( list
  ( list (N_ "Copy into 1")  'buffer-copy1  "gtk-copy" )
  ( list (N_ "Copy into 2")  'buffer-copy2  "gtk-copy" )
  ( list (N_ "Copy into 3")  'buffer-copy3  "gtk-copy" )
  ( list (N_ "Copy into 4")  'buffer-copy4  "gtk-copy" )
  ( list (N_ "Copy into 5")  'buffer-copy5  "gtk-copy" )
  ( list (N_ "Cut into 1")   'buffer-cut1   "gtk-cut" )
  ( list (N_ "Cut into 2")   'buffer-cut2   "gtk-cut" )
  ( list (N_ "Cut into 3")   'buffer-cut3   "gtk-cut" )
  ( list (N_ "Cut into 4")   'buffer-cut4   "gtk-cut" )
  ( list (N_ "Cut into 5")   'buffer-cut5   "gtk-cut" )
  ( list (N_ "Paste from 1") 'buffer-paste1 "gtk-paste" )
  ( list (N_ "Paste from 2") 'buffer-paste2 "gtk-paste" )
  ( list (N_ "Paste from 3") 'buffer-paste3 "gtk-paste" )
  ( list (N_ "Paste from 4") 'buffer-paste4 "gtk-paste" )
  ( list (N_ "Paste from 5") 'buffer-paste5 "gtk-paste" )
)
) ; buffer-menu-items


( define ( view-menu-items )
( let*
  (
  ( default-use-docks #f )
  ( use-docks default-use-docks )
  ( grp "schematic.gui" )
  ( key "use-docks" )
  ( cfg #f )
  )

  ( define ( opt-item item )
    ; return:
    ( if use-docks
      item ; if
      '()  ; else
    )
  )


  ( catch #t
    ( lambda()
      ( set! cfg ( path-config-context (getcwd) ) )
      ( set! use-docks ( config-boolean cfg grp key ) )
    )
    ( lambda( ex . args )
      ( format
        ( current-error-port )
        "menu.scm: Cannot read configuration key [~a::~a]:~%~
         '~a: ~a~%~
         Please check your installation.~%"
        grp key ex args
      )
    )
  )

  ; return:
  ( list
    ( opt-item (list (N_ "Side Dock")   '&view-sidebar #f) )
    ( opt-item (list (N_ "Bottom Dock") '&view-status  #f) )
    ( opt-item (list "SEPARATOR" #f #f) )
    ( list (N_ "Find Text Results")   '&view-find-text-state   #f )
    ( list "SEPARATOR" #f #f )
    ( list (N_ "_Redraw")             '&view-redraw            "gtk-refresh" )
    ( list (N_ "_Pan")                '&view-pan               #f )
    ( list (N_ "Zoom _Box")           '&view-zoom-box          #f )
    ( list (N_ "Zoom _Extents")       '&view-zoom-extents      "gtk-zoom-fit" )
    ( list (N_ "Zoom _In")            '&view-zoom-in           "gtk-zoom-in" )
    ( list (N_ "Zoom _Out")           '&view-zoom-out          "gtk-zoom-out" )
    ( list (N_ "Zoom _Full")          '&view-zoom-full         #f )
    ( list "SEPARATOR" #f #f )
    ( list (N_ "_Dark Color Scheme")  '&view-dark-colors       #f )
    ( list (N_ "_Light Color Scheme") '&view-light-colors      #f )
    ( list (N_ "B_W Color Scheme")    '&view-bw-colors         #f )
    ( list "SEPARATOR" #f #f )
    ( list (N_ "Color Scheme Editor...") '&view-color-edit     #f )
  )

) ; let
) ; view-menu-items()


( define ( page-menu-items )
( let*
  (
  ( default-use-tabs #t )
  ( use-tabs default-use-tabs )
  ( grp "schematic.gui" )
  ( key "use-tabs" )
  ( cfg #f )
  )

  ( define ( opt-item item )
    ; return:
    ( if use-tabs
      item ; if
      '()  ; else
    )
  )


  ( catch #t
    ( lambda()
      ( set! cfg ( path-config-context (getcwd) ) )
      ( set! use-tabs ( config-boolean cfg grp key ) )
    )
    ( lambda( ex . args )
      ( format
        ( current-error-port )
        "menu.scm: Cannot read configuration key [~a::~a]:~%~
         '~a: ~a~%~
         Please check your installation.~%"
        grp key ex args
      )
    )
  )

  ; return:
  ( list
    ( list (N_ "_Manager...")  '&page-manager  #f )
    ( list "SEPARATOR" #f #f )
    ( list (N_ "_Previous")    '&page-prev     "gtk-go-back" )
    ( list (N_ "_Next")        '&page-next     "gtk-go-forward" )
    ( list (N_ "_Close")       '&page-close    "gtk-close" )
    ( list "SEPARATOR" #f #f )
    ( list (N_ "_Revert...")   '&page-revert   "gtk-revert-to-saved" )
    ( opt-item (list "SEPARATOR" #f #f) )
    ( opt-item (list (N_ "Next Tab")     '&page-next-tab "gtk-go-forward") )
    ( opt-item (list (N_ "Previous Tab") '&page-prev-tab "gtk-go-back") )
  )

) ; let
) ; page-menu-items()


( define add-menu-items
( list
  ( list (N_ "_Component...") '&add-component "insert-symbol" )
  ( list "SEPARATOR" #f #f )
  ( list (N_ "_Net")          '&add-net       "insert-net" )
  ( list (N_ "B_us")          '&add-bus       "insert-bus" )
  ( list "SEPARATOR" #f #f )
  ( list (N_ "_Attribute...") '&add-attribute "insert-attribute" )
  ( list (N_ "_Text...")      '&add-text      "insert-text" )
  ( list "SEPARATOR" #f #f )
  ( list (N_ "_Line")         '&add-line      "insert-line" )
  ( list (N_ "Pat_h")         '&add-path      "insert-path" )
  ( list (N_ "_Box")          '&add-box       "insert-box" )
  ( list (N_ "C_ircle")       '&add-circle    "insert-circle" )
  ( list (N_ "A_rc")          '&add-arc       "insert-arc" )
  ( list (N_ "_Pin")          '&add-pin       "insert-pin" )
  ( list "SEPARATOR" #f #f )
  ( list (N_ "Pictu_re...")   '&add-picture   "insert-image" )
)
) ; add-menu-items


(define hierarchy-menu-items
( list
  ( list (N_ "_Up")             '&hierarchy-up             "gtk-go-up" )
  ( list "SEPARATOR" #f #f )
  ( list (N_ "_Down Schematic") '&hierarchy-down-schematic "gtk-go-down" )
  ( list (N_ "Down _Symbol")    '&hierarchy-down-symbol    "gtk-goto-bottom" )
)
) ; hierarchy-menu-items


( define attributes-menu-items
( list
  ( list (N_ "_Attach")      '&attributes-attach     "attribute-attach" )
  ( list (N_ "_Detach")      '&attributes-detach     "attribute-detach" )
  ( list "SEPARATOR" #f #f )
  ( list (N_ "Show _Value")  '&attributes-show-value "attribute-show-value" )
  ( list (N_ "Show _Name")   '&attributes-show-name  "attribute-show-name" )
  ( list (N_ "Show _Both")   '&attributes-show-both  "attribute-show-both" )
  ( list (N_ "_Toggle Visibility") '&attributes-visibility-toggle #f )
  ( list "SEPARATOR" #f #f )
  ( list (N_ "_Hide Specific Text...") '&edit-hide-text   #f )
  ( list (N_ "_Show Specific Text...") '&edit-show-text   #f )
  ( list (N_ "Show/Hide Hidden Text")  '&edit-show-hidden #f )
  ( list "SEPARATOR" #f #f )
  ( list (N_ "_Find Text/Check Symbol...") '&edit-find-text  "gtk-find" )
  ( list (N_ "A_utonumber Text...")        '&edit-autonumber #f )
)
) ; attributes-menu-items


( define options-menu-items
( list
  ( list (N_ "_Options...")  '&options-snap-size #f )
  ( list (N_ "_Font...")     '&options-select-font #f )
  ( list "SEPARATOR" #f #f )
  ( list (N_ "Grid +")       '&options-scale-up-snap-size #f )
  ( list (N_ "Grid -")       '&options-scale-down-snap-size #f )
  ( list "SEPARATOR" #f #f )
  ( list (N_ "Grid Style: Cycle Dots/Mesh/Off")  '&options-grid #f )
  ( list (N_ "Grid Snap: Cycle Grid/Resnap/Off") '&options-snap #f )
  ( list "SEPARATOR" #f #f )
  ( list (N_ "Grips: On/Off")              '&options-draw-grips #f )
  ( list (N_ "Feedback Mode: Outline/Box") '&options-action-feedback #f )
  ( list (N_ "Net: Rubberband On/Off")     '&options-rubberband #f )
  ( list (N_ "Net: Magnetic On/Off")       '&options-magneticnet #f )
  ( list "SEPARATOR" #f #f )
  ( list (N_ "_Coord Window") '&options-show-coord-window #f )
  ( list (N_ "_Log Window")   '&options-show-log-window #f )
)
) ; options-menu-items


( define netlist-menu-items
( list
  ( list (N_ "_1 allegro") '&netlist-allegro #f )
)
) ; netlist-menu-items


( define help-menu-items
( list
  ( list (N_ "Lepton EDA Reference _Manual") '&help-manual "help-browser" )
  ( list (N_ "gEDA _Wiki Documentation")     '&help-wiki   "web-browser" )
  ( list "SEPARATOR" #f #f )
  ( list (N_ "Find Component D_ocumentation") '&hierarchy-documentation "symbol-datasheet" )
  ( list "SEPARATOR" #f #f )
  ( list (N_ "_Hotkeys...")  '&help-hotkeys "preferences-desktop-keyboard-shortcuts" )
  ( list (N_ "_About")       '&help-about   "gtk-about" )
)
) ; help-menu-items



;
; Now actually add the menus.  The order here defines the order in which
; the menus appear in the top menu bar.
;
( add-menu (N_ "_File")       file-menu-items )
( add-menu (N_ "_Edit")       edit-menu-items )
; ( add-menu (N_ "_Buffer")     buffer-menu-items )
( add-menu (N_ "_View")       (view-menu-items) )
( add-menu (N_ "_Page")       (page-menu-items) )
( add-menu (N_ "_Add")        add-menu-items )
( add-menu (N_ "Hie_rarchy")  hierarchy-menu-items )
( add-menu (N_ "A_ttributes") attributes-menu-items )
( add-menu (N_ "_Options")    options-menu-items )
( add-menu (N_ "_Netlist")    netlist-menu-items )
( add-menu (N_ "_Help")       help-menu-items )

