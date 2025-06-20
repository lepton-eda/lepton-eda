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
             (lepton gettext)
             (schematic menu))


( define file-menu-items
( list
;;
;;       menu item name           menu action              menu stock icon
;;
  ( list (G_ "_New")              '&file-new               "gtk-new"     )
  ( list (G_ "_Open...")          '&file-open              "gtk-open"    )
  ( list (G_ "Open Recen_t")      #f                       #f            )
  ( list "SEPARATOR"              #f                       #f            )
  ( list (G_ "_Save")             '&file-save              "gtk-save"    )
  ( list (G_ "Save _As...")       '&file-save-as           "gtk-save-as" )
  ( list (G_ "Save All")          '&file-save-all          "gtk-save"    )
  ( list "SEPARATOR"              #f                       #f            )
  ( list (G_ "_Print...")         '&file-print             "gtk-print"   )
  ( list (G_ "Write _Image...")   '&file-image             #f            )
  ( list "SEPARATOR"              #f                       #f            )
  ( list (G_ "Invoke Macro...")   '&edit-invoke-macro      "gtk-execute" )
  ( list (G_ "Execute Script...") '&file-script            "gtk-execute" )
  ( list (G_ "REPL...")           '&file-repl              "gtk-execute" )
  ( list "SEPARATOR"              #f                       #f )
  ( list (G_ "New Window")        '&file-new-window        "window-new"  )
  ( list (G_ "_Close Window")     '&file-close-window      "gtk-close"   )
  ( list (G_ "_Quit")             '&file-quit              "gtk-quit"    )
)
) ; file-menu-items


( define edit-menu-items
( list
  ( list (G_ "_Undo")              '&edit-undo       "gtk-undo" )
  ( list (G_ "_Redo")              '&edit-redo       "gtk-redo" )
  ( list "SEPARATOR" #f #f )
  ( list (G_ "Cu_t")               '&clipboard-cut   "gtk-cut" )
  ( list (G_ "_Copy")              '&clipboard-copy  "gtk-copy" )
  ( list (G_ "_Paste")             '&clipboard-paste "gtk-paste" )
  ( list (G_ "_Delete")            '&edit-delete     "gtk-delete" )
  ( list "SEPARATOR" #f #f )
  ( list (G_ "Select Mode")        '&edit-select     "select" )
  ( list (G_ "Select All")         '&edit-select-all "gtk-select-all" )
  ( list (G_ "Deselect")           '&edit-deselect   "deselect" )
  ( list (G_ "Copy Mode")          '&edit-copy       "clone" )
  ( list (G_ "Multiple Copy Mode") '&edit-mcopy      "multi-clone" )
  ( list (G_ "Move Mode")          '&edit-move       #f)
  ( list (G_ "Rotate 90 Mode")     '&edit-rotate-90  "object-rotate-left" )
  ( list (G_ "Mirror Mode")        '&edit-mirror     "object-flip-horizontal" )
  ( list "SEPARATOR" #f #f )
  ( list (G_ "Object Properties...") '&edit-object-properties "gtk-properties" )
  ( list (G_ "Edit...")              '&edit-edit              #f )
  ( list (G_ "Edit Text...")         '&edit-text              "gtk-edit" )
  ( list (G_ "Slot...")              '&edit-slot              #f )
  ( list "SEPARATOR" #f #f )
  ( list (G_ "Lock")                 '&edit-lock              #f )
  ( list (G_ "Unlock")               '&edit-unlock            #f )
  ( list (G_ "Select Locked")        '&edit-select-locked     #f )
  ( list "SEPARATOR" #f #f )
  ( list (G_ "Embed Component/Picture")   '&edit-embed        #f )
  ( list (G_ "Unembed Component/Picture") '&edit-unembed      #f )
  ( list (G_ "Update Component")          '&edit-update       "gtk-refresh" )
  ( list (G_ "Symbol Translate...")       '&edit-translate    #f )
)
) ; edit-menu-items


( define buffer-menu-items
( list
  ( list (G_ "Copy into 1")  'buffer-copy1  "gtk-copy" )
  ( list (G_ "Copy into 2")  'buffer-copy2  "gtk-copy" )
  ( list (G_ "Copy into 3")  'buffer-copy3  "gtk-copy" )
  ( list (G_ "Copy into 4")  'buffer-copy4  "gtk-copy" )
  ( list (G_ "Copy into 5")  'buffer-copy5  "gtk-copy" )
  ( list (G_ "Cut into 1")   'buffer-cut1   "gtk-cut" )
  ( list (G_ "Cut into 2")   'buffer-cut2   "gtk-cut" )
  ( list (G_ "Cut into 3")   'buffer-cut3   "gtk-cut" )
  ( list (G_ "Cut into 4")   'buffer-cut4   "gtk-cut" )
  ( list (G_ "Cut into 5")   'buffer-cut5   "gtk-cut" )
  ( list (G_ "Paste from 1") 'buffer-paste1 "gtk-paste" )
  ( list (G_ "Paste from 2") 'buffer-paste2 "gtk-paste" )
  ( list (G_ "Paste from 3") 'buffer-paste3 "gtk-paste" )
  ( list (G_ "Paste from 4") 'buffer-paste4 "gtk-paste" )
  ( list (G_ "Paste from 5") 'buffer-paste5 "gtk-paste" )
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
    ( opt-item (list (G_ "Side Dock")   '&view-sidebar #f) )
    ( opt-item (list (G_ "Bottom Dock") '&view-status  #f) )
    ( opt-item (list "SEPARATOR" #f #f) )
    ( list (G_ "Find Text Results")   '&view-find-text-state   #f )
    ( list "SEPARATOR" #f #f )
    ( list (G_ "_Redraw")             '&view-redraw            "gtk-refresh" )
    ( list (G_ "_Pan")                '&view-pan               #f )
    ( list (G_ "Zoom _Box")           '&view-zoom-box          #f )
    ( list (G_ "Zoom _Extents")       '&view-zoom-extents      "gtk-zoom-fit" )
    ( list (G_ "Zoom _In")            '&view-zoom-in           "gtk-zoom-in" )
    ( list (G_ "Zoom _Out")           '&view-zoom-out          "gtk-zoom-out" )
    ( list (G_ "Zoom _Full")          '&view-zoom-full         #f )
    ( list "SEPARATOR" #f #f )
    ( list (G_ "_Dark Color Scheme")  '&view-dark-colors       #f )
    ( list (G_ "_Light Color Scheme") '&view-light-colors      #f )
    ( list (G_ "B_W Color Scheme")    '&view-bw-colors         #f )
    ( list "SEPARATOR" #f #f )
    ( list (G_ "Color Scheme Editor...") '&view-color-edit     #f )
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
    ( list (G_ "_Manager...")  '&page-manager  #f )
    ( list "SEPARATOR" #f #f )
    ( list (G_ "_Previous")    '&page-prev     "gtk-go-back" )
    ( list (G_ "_Next")        '&page-next     "gtk-go-forward" )
    ( list (G_ "_Close")       '&page-close    "gtk-close" )
    ( list "SEPARATOR" #f #f )
    ( list (G_ "_Revert...")   '&page-revert   "gtk-revert-to-saved" )
    ( opt-item (list "SEPARATOR" #f #f) )
    ( opt-item (list (G_ "Next Tab")     '&page-next-tab "gtk-go-forward") )
    ( opt-item (list (G_ "Previous Tab") '&page-prev-tab "gtk-go-back") )
  )

) ; let
) ; page-menu-items()


( define add-menu-items
( list
  ( list (G_ "_Component...") '&add-component "insert-symbol" )
  ( list "SEPARATOR" #f #f )
  ( list (G_ "_Net")          '&add-net       "insert-net" )
  ( list (G_ "B_us")          '&add-bus       "insert-bus" )
  ( list "SEPARATOR" #f #f )
  ( list (G_ "_Attribute...") '&add-attribute "insert-attribute" )
  ( list (G_ "_Text...")      '&add-text      "insert-text" )
  ( list "SEPARATOR" #f #f )
  ( list (G_ "_Line")         '&add-line      "insert-line" )
  ( list (G_ "Pat_h")         '&add-path      "insert-path" )
  ( list (G_ "_Box")          '&add-box       "insert-box" )
  ( list (G_ "C_ircle")       '&add-circle    "insert-circle" )
  ( list (G_ "A_rc")          '&add-arc       "insert-arc" )
  ( list (G_ "_Pin")          '&add-pin       "insert-pin" )
  ( list "SEPARATOR" #f #f )
  ( list (G_ "Pictu_re...")   '&add-picture   "insert-image" )
)
) ; add-menu-items


(define hierarchy-menu-items
( list
  ( list (G_ "_Up")             '&hierarchy-up             "gtk-go-up" )
  ( list "SEPARATOR" #f #f )
  ( list (G_ "_Down Schematic") '&hierarchy-down-schematic "gtk-go-down" )
  ( list (G_ "Down _Symbol")    '&hierarchy-down-symbol    "gtk-goto-bottom" )
)
) ; hierarchy-menu-items


( define attributes-menu-items
( list
  ( list (G_ "_Attach")      '&attributes-attach     "attribute-attach" )
  ( list (G_ "_Detach")      '&attributes-detach     "attribute-detach" )
  ( list "SEPARATOR" #f #f )
  ( list (G_ "Show _Value")  '&attributes-show-value "attribute-show-value" )
  ( list (G_ "Show _Name")   '&attributes-show-name  "attribute-show-name" )
  ( list (G_ "Show _Both")   '&attributes-show-both  "attribute-show-both" )
  ( list (G_ "_Toggle Visibility") '&attributes-visibility-toggle #f )
  ( list "SEPARATOR" #f #f )
  ( list (G_ "_Hide Specific Text...") '&edit-hide-text   #f )
  ( list (G_ "_Show Specific Text...") '&edit-show-text   #f )
  ( list (G_ "Show/Hide Hidden Text")  '&edit-show-hidden #f )
  ( list "SEPARATOR" #f #f )
  ( list (G_ "_Find Text/Check Symbol...") '&edit-find-text  "gtk-find" )
  ( list (G_ "A_utonumber Text...")        '&edit-autonumber #f )
)
) ; attributes-menu-items


( define options-menu-items
( list
  ( list (G_ "_Options...")  '&options-snap-size #f )
  ( list (G_ "_Font...")     '&options-select-font #f )
  ( list "SEPARATOR" #f #f )
  ( list (G_ "Grid +")       '&options-scale-up-snap-size #f )
  ( list (G_ "Grid -")       '&options-scale-down-snap-size #f )
  ( list "SEPARATOR" #f #f )
  ( list (G_ "Grid Style: Cycle Dots/Mesh/Off")  '&options-grid #f )
  ( list (G_ "Grid Snap: Cycle Grid/Resnap/Off") '&options-snap #f )
  ( list "SEPARATOR" #f #f )
  ( list (G_ "Grips: On/Off")              '&options-draw-grips #f )
  ( list (G_ "Feedback Mode: Outline/Box") '&options-action-feedback #f )
  ( list (G_ "Net: Rubberband On/Off")     '&options-rubberband #f )
  ( list (G_ "Net: Magnetic On/Off")       '&options-magneticnet #f )
  ( list "SEPARATOR" #f #f )
  ( list (G_ "_Coord Window") '&options-show-coord-window #f )
  ( list (G_ "_Log Window")   '&options-show-log-window #f )
)
) ; options-menu-items


( define netlist-menu-items
( list
  ( list (G_ "_1 allegro") '&netlist-allegro #f )
)
) ; netlist-menu-items


( define help-menu-items
( list
  ( list (G_ "Lepton EDA Reference _Manual") '&help-manual "help-browser" )
  ( list (G_ "gEDA _Wiki Documentation")     '&help-wiki   "web-browser" )
  ( list "SEPARATOR" #f #f )
  ( list (G_ "Find Component D_ocumentation") '&hierarchy-documentation "symbol-datasheet" )
  ( list "SEPARATOR" #f #f )
  ( list (G_ "_Hotkeys...")  '&help-hotkeys "preferences-desktop-keyboard-shortcuts" )
  ( list (G_ "_About")       '&help-about   "gtk-about" )
)
) ; help-menu-items



;
; Now actually add the menus.  The order here defines the order in which
; the menus appear in the top menu bar.
;
( add-menu (G_ "_File")       file-menu-items )
( add-menu (G_ "_Edit")       edit-menu-items )
; ( add-menu (G_ "_Buffer")     buffer-menu-items )
( add-menu (G_ "_View")       (view-menu-items) )
( add-menu (G_ "_Page")       (page-menu-items) )
( add-menu (G_ "_Add")        add-menu-items )
( add-menu (G_ "Hie_rarchy")  hierarchy-menu-items )
( add-menu (G_ "A_ttributes") attributes-menu-items )
( add-menu (G_ "_Options")    options-menu-items )
( add-menu (G_ "_Netlist")    netlist-menu-items )
( add-menu (G_ "_Help")       help-menu-items )
