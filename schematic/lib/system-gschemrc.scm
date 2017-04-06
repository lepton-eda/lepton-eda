;                                                         -*-Scheme-*-
;
; Init file for gschem
;

(use-modules (gschem deprecated))

;  ;'s are comments
;  keywords are case sensitive (guile feature)
;  mode strings are case sensitive
;  colors are not case sensitive
;

; gschem-version string
;
; Specifies the version of this file.  This number is used to make sure
; that the rc file is compatible with the version of gschem that is
; being run. The end user should *not* change this value.
;
(gschem-version "@DATE_VERSION@")

;
; Start of color section
;

;; Make the gschem color maps more user-friendly
(color-map-make-friendly display-color-map)
(color-map-make-friendly display-outline-color-map)

; Load up a color scheme which has a dark (black) background.
; Comment out the first line and comment in the second line for a
; light (almost white) background.  The dark background is the
; original look.
;
(primitive-load (build-path geda-rc-path "gschem-colormap-darkbg")) ; dark background
;(load (build-path geda-rc-path "gschem-colormap-lightbg")) ; light background
;(load (build-path geda-rc-path "gschem-colormap-bw")) ; light background, bw

;
; End of color section
;

;
; Start of mode related keywords
;

; toolbars string
;
; Controls if the toolbars are visible or not.
;
(toolbars "enabled")
;(toolbars "disabled")

; handleboxes string
;
; Controls if the handleboxes (which contain the menu and toolbar) are
; visible or not.
;
(handleboxes "enabled")
;(handleboxes "disabled")

; undo-control string
;
; Controls if the undo is enabled or not
;
(undo-control "enabled")
;(undo-control "disabled")

; undo-levels number
;
; Determines the number of levels of undo.  Basically this number decides
; how many backup schematics are saved on disk.
;
(undo-levels 10)

; undo-type string
;
; Controls which kind of undo is used.  The default is to use the disk as
; the storing medium (ie after every action the undo information is stored
; to disk).  The other mechanism uses only memory.  The disk mechanism is
; nice because you get undo-level number of backups of the schematic written
; to disk as backups so you should never lose a schematic due to a crash.
;
(undo-type "disk")
;(undo-type "memory")

; undo-panzoom string
;
; Controls if pan or zoom commands are saved in the undo list.  If this
; is enabled then a pan or zoom command will be considered a command and
; can be undone.  If this is false, then panning and zooming is not saved
; in the undo list and cannot be undone.  Note, the current viewport
; information is saved for every command, so the display will change to the
; viewport before a command is executed.
;
(undo-panzoom "enabled")
;(undo-panzoom "disabled")


; autosave interval
;
; Controls if a backup copy is made every "interval" seconds.
; Note that the backup copy is made when you make some change to the schematic,
; and there were more than "interval" seconds from the last autosave.
; Autosaving will not be allowed if setting it to zero.
(auto-save-interval 120)

; draw-grips string
;
; Controls if the editing grips are drawn when selecting objects
;
(draw-grips "enabled")
;(draw-grips "disabled")

;  net-direction-mode string
;
;  Controlls if the net direction mode is used. This mode tries to guess
;  the best continuation direction of a L-shape net when adding a net.
;
(net-direction-mode "enabled")
;(net-direction-mode "disabled")

;  net-selection-mode string
;
; Controls how many net segments are selected when you click at a net
; If one of the enabled items is used, the selection state will toggle
; through all selection states. The mode defines the maximum search depth
; for the net selection mode
;
;(net-selection-mode "disabled")
(net-selection-mode "enabled_net")
;(net-selection-mode "enabled_all")

;  net-consolidate string
;
;  Controls if the net consolidation code is used when schematics are read
;  in, written to disk, and when nets are being drawn (does not consolidate
;  when things are being copied or moved yet).  Net consolidation is the
;  connection of nets which can be combined into one.
;  Comment out if you want the default mode
;
(net-consolidate "enabled")
;(net-consolidate "disabled")

; netconn-rubberband string
;
; Controls if net connections are maintained when you move a connecting
; component or net.
;
(netconn-rubberband "enabled")
;(netconn-rubberband "disabled")

; magnetic-net-mode string
;
; Controls the initial setting of the magnetic net mode. The magnetic
; net mode marks a possible connection that is close to the current
; cursor position
(magnetic-net-mode "enabled")
;(magnetic-net-mode "disabled")

; zoom-with-pan string
;
; Sets the zoom in and zoom out functions to pan the display and then zoom.
; Basically zoom in / out where the mouse pointer sits on the display.
; Comment out if you want the default mode.
;
(zoom-with-pan "enabled")
;(zoom-with-pan "disabled")

; zoom-gain integer
;
; Controls the percentage size increase when zooming into the page.
; Un-zooming uses the inverse factor such that a zoom in / zoom out
; pair will return the schematic to the same size.
;  E.g:
;    20% increment => x 1.2 original size when zooming in
;                  => x 1 / 1.2 x original size when zooming out
;
(zoom-gain 20)
;(zoom-gain 50) ; Hard-coded behaviour up to version 1.5.0.20080706

; fast-mousepan string
;
; Controls if text is drawn properly or if a simplified version (a line which
; represents the text string) is drawn during mouse pan.  Drawing a simple
; line speeds up mousepan a lot for big schematics
(fast-mousepan "enabled")
;(fast-mousepan "disabled")

; mousepan-gain integer
;
; Controls how much the display pans when using mousepan.  A larger value
; provides greater pan distance when moving the mouse, while a smaller value
; provides a smoother, but smaller pan distance when moving the mouse.
(mousepan-gain 1)
;(mousepan-gain 5)
;(mousepan-gain 10)

; keyboardpan-gain integer
;
; Controls how much the display pans when using the keyboard cursor keys.
; A larger value provides greater pan distance when pressing the cursor
; keys, while a smaller value provides a smoother, but smaller pan
; distance when moving the cursor keys.
(keyboardpan-gain 20)
;(keyboardpan-gain 10)
;(keyboardpan-gain 1)
;(keyboardpan-gain 5)

; select-slack-pixels integer
;
; Controls how many pixels around an object can still be clicked as part of
; that object.
; A larger value gives greater ease in selecting small, or narrow objects.
(select-slack-pixels 10)
;(select-slack-pixels 4)
;(select-slack-pixels 0)
;(select-slack-pixels 1)


; action-feedback-mode string
;
; Set the default action feedback mode (for copy/move/component place).
; Set to outline to get an outline of the selection.
; Set to boundingbox to get a bounding box of the selection.
; For a fast machines with fast video use outline (it looks good).
; For a slow machine use boundingbox; it is much faster.
; Comment out if you want the default mode.
;
(action-feedback-mode "outline")
;(action-feedback-mode "boundingbox")

; continue-component-place string
;
; If this enabled then multiple instances of the same component can be placed
; immediately without having to click on the name or Apply in the Component
; Place... dialog box.  If this is disabled then only one component can be
; placed (the user must then press Apply in the dialog box to place multiple
; instances of the same component)
;
(continue-component-place "enabled")
;(continue-component-place "disabled")

; scrollbars string
;
; Controls if the scrollbars are displayed (enabled) or not (disabled)
; If you disable the scrollbars, you will not be able to use the scroll
; wheel on your mouse.  This is an unfortunate side effect of how the
; code is implemented.
;
(scrollbars "enabled")
;(scrollbars "disabled")

; raise-dialog-boxes-on-expose string
;
; Controls if dialog boxes are raised whenever an expose event happens
; Default is disabled since gtk2 supposedly handles the raising of these
; dialogs correctly now.
;
;(raise-dialog-boxes-on-expose "enabled")
(raise-dialog-boxes-on-expose "disabled")

; embed-components string
;
; Determines if the newly placed components are embedded in the schematic
; or if only the filename is specified and the component is searched for
; instead.  If it is enabled, then all new components will be embedded
; otherwise they are not embedded.  This can be controlled on the fly during
; runtime with the "Embed Component" checkbox on the select component dialog
; box
;
;(embed-components "enabled")
(embed-components "disabled")

; logging string
;
; Determines if the logging mechanism is enabled or disabled
;   Possible options: enabled or disabled
; Default is enabled.
; See below for the logging-destination keyword for control over
; where the messages go.
;
(logging "enabled")
;(logging "disabled")


; log-window string
;
; Controls if the log message window is mapped when gschem is started up
; Possible options:
;       startup - opened up when gschem starts
;       later   - NOT opened up when gschem starts
;                 (can be opened by Options/Show Log Window)
;
(log-window "startup")
;(log-window "later")


; log-window-type string
;
; Controls if the log message window is a transient or if it is decorated
; as a normal window (this is dependant on the window manager doing decoration
; right)
;
; Possible options:
;       decorated       - log window is a normal decorated window
;       transient       - log window is a transient dialog box, typically
;                         not decorated by the window manager
;
(log-window-type "decorated")
;(log-window-type "transient")


; logging-destination string
;
; Specifies where log message go during run time.
; Possible options are:
;      log_window      The log window (if it's visible)
;      tty             The stdout of the terminal where gschem was run from
;      both            Both of the above locations
; Message are always written to the log file (unless logging is disabled)
; by the above keyword
;
; Default is log_window
;
(logging-destination "log_window")
;(logging-destination "tty")
;(logging-destination "both")

; text-size number
;
; Sets the default text size.
;
(text-size 10)

; snap-size number
;
; Sets the default grid spacing at start-up of gschem.
;
(snap-size 100)

; text-caps-style string
;
; Sets the default caps style used for the input of text
; lower specifies that all inputed text is in lowercase
; upper specifies that all inputed text is in uppercase
; both specifies that all inputed text is used as is (no case conversion)
;
(text-caps-style "both")
;(text-caps-style "lower")
;(text-caps-style "upper")

;  file-preview string
;
;  Controls if the preview area in the File Open/Save As and Component
;  dialog boxes is enabled by default or not
;
(file-preview "enabled")
;(file-preview "disabled")

;  enforce-hierarchy string
;
;  Controls if the movement between hierarchy levels (of the same underlying
;  schematics) is allowed or not.
;  If this is enabled, then the user cannot (without using the page manager)
;  move between hierarchy levels otherwise, if enabled, the user sees all
;  the hierarchy levels as being flat.
;
(enforce-hierarchy "enabled")
;(enforce-hierarchy "disabled")

; window-size width height
;
; Specifies the size of the drawing area window.  The width and height
; are specified in pixels and do not include the three menu bars and
; scrollbars (so the window will be larger than the specified
; measurements). Try to keep an aspect ratio of 1.333333 if at all possible.
; These numbers are NOT the true size of the window, but of the drawing area.
;
;(window-size 650 487)  ; Good size for 800x600
(window-size 900 650)   ; Good size for 1024x768
;(window-size 950 712)  ; Good size for 1152x864
;(window-size 1100 825) ; Good size for 1280x1024

; image-color string
;
; Controls if image (png) is color (enabled) or black/white (disabled)
;
(image-color "enabled")
;(image-color "disabled")

; middle-button string
;
; Controls if the middle mouse button draws strokes, repeats the last
; command, does an action (move and copy (holding down the ALT key)
; are supported) on a single objects, or if it does the mouse panning.
;
(middle-button "mousepan")
;(middle-button "action")
;(middle-button "stroke")
;(middle-button "repeat")

; third-button string
;
; Controls if the third mouse button performs the popup ("popup") or
; if it does the mouse panning ("mousepan")
;
(third-button "popup")
;(third-button "mousepan")

; third-button-cancel string
;
; Controls if the third mouse in mousepan mode cancels draw actions such as
; placing of a component or drawing of a primitive
;
(third-button-cancel "enabled")
;(third-button-cancel "disabled")

; scroll-wheel string
;
; Controls the binding of the mouse scroll wheel.
; "classic" style is the gschem default, where scrolling with no modifier
; key is mapped to zoom, + CTRL -> x-axis pan, + SHIFT -> y-axis pan.
; "gtk" style changes the behaviour to be more like other GTK applications,
; no modifier -> y-axis pan, + CTRL -> zoom, + SHIFT -> x-axis pan.
(scroll-wheel "classic")
;(scroll-wheel "gtk")


; scrollpan-steps integer
;
; Controls the number of scroll pan events required to traverse the viewed
; schematic area. Larger numbers mean more scroll steps are required to
; pan across the viewed area and giving finer control over positioning.
(scrollpan-steps 8)
;(scrollpan-steps 4) ; Hard-coded behaviour up to version 1.5.0.20080706

; warp-cursor string
;
; Controls if the cursor is warped (or moved) when you zoom in and out.
; Some people find this forced cursor movement annoying.
;
(warp-cursor "enabled")
;(warp-cursor "disabled")


; Bus ripper controls
; The following keywords control the auto bus ripper addition code
;
; bus-ripper-size  : Sets the size of the auto bus rippers.
; bus-ripper-type  : Sets the bus ripper type either a "component" or
;                    plain "net"
; bus-ripper-symname  : If above is set to component, specify the symbol name.
;                       The symbol must exist in a component library
; bus-ripper-rotation  : Either "symmetric" or "non-symmetric".  This deals
;                        with how the bus ripper symbol is rotated when it
;                        is auto added to a schematic.
;

; The default bus ripper
(bus-ripper-size 200)
(bus-ripper-type "component")
(bus-ripper-symname "busripper-1.sym")
(bus-ripper-rotation "non-symmetric")

; A symmetric alternative
;(bus-ripper-size 200)
;(bus-ripper-type "component")
;(bus-ripper-symname "busripper-2.sym")
;(bus-ripper-rotation "symmetric")

; A simple net
;(bus-ripper-size 200)
;(bus-ripper-type "net")

; Grid mode
;
; The grid-mode keyword controls which mode of grid is used by default in
; gschem.
;(grid-mode "none")
;(grid-mode "dots")
(grid-mode "mesh")

; Dots grid dot size
;
; The dots-grid-dot-size keyword controls the size of the grid dots in the
; dots grid display. The units are in pixels. The default (min) value of 1
; is the best performing as the grid dot size is rendered as a single pixel.
; Values of 2 and 3 are good values to try if the default grid dot size is
; too small for your tastes. Anything larger than 3 is probably too large.
;
(dots-grid-dot-size 1)
;(dots-grid-dot-size 2)
;(dots-grid-dot-size 3)

; Dots grid mode
;
; The dots-grid-mode keyword controls the mode of the dotted grid, either
; variable or fixed. In the variable mode, the grid spacing changes
; depending on the zoom factor. In the fixed mode, the grid always
; represents the same number of units as the snap-spacing. You can
; control the density of the grid using the dots-grid-fixed-threshold.
(dots-grid-mode "variable")
;(dots-grid-mode "fixed")

; Dots grid fixed threshold
;
; The dots-grid-fixed-threshold specifies the minimum number of pixels
; grid-spacing for the grid to be displayed. Using this parameter you can
; control thedensity of the displayed grid (smaller numbers will cause the
; grid to be drawn denser). This mode is only used when grid-mode is fixed.
;
(dots-grid-fixed-threshold 10)

; Mesh grid display threshold
;
; The mesh-grid-display-threshold specifies the minimum line pitch for a the
; grid to be displayed. Using this parameter you can control maximum density
; of the displayed before the minor, then major grid-lines are switched off.
;
(mesh-grid-display-threshold 3)

; force-boundingbox string
;
; Controls if the entire bounding box of a symbol is used when figuring out
; whichend of the pin is considered the active port.  Enable this when
; gschem is guessing incorrectly.
;
(force-boundingbox "disabled")
;(force-boundingbox "enabled")


; add-attribute-offset integer
;
; This has not been implemented/debugged yet.
; This has not been implemented/debugged yet.
; This has not been implemented/debugged yet.
;
; Controls a offset which is added to the location of text items that are
; added to an object as an attribute.  This offset is added when the following
; conditions occur:
;
;  1) Add/Attribute... has been invoked via the hotkey
;  2) It is the "netname" attribute being added
;  3) It is being attached to a horizontal or vertical net segment
;  4) The initial mouse position is at or near the actual net (with one
;     grid unit).
;
; If these four conditions are not met, then this offset is not added.
;(add-attribute-offset 50)


; reset-component-library
;
; When reset-component-library is executed, then all known component library
; paths are erased.  This is useful if the user wants to override all the
; system provided paths and provide his/her own set.  Normally this is not
; commented in.
;
; (reset-component-library)


; reset-source-library
;
; When reset-source-library is executed, then all known source library
; paths are erased.  This is useful if the user wants to override all the
; system provided paths and provide his/her own set.  Normally this is not
; commented in.
;
; (reset-source-library)

;
; End of mode related keywords
;


;
; Start of hooks
;

;;
;; Comment in this scheme code if you want automatic numbering when
;; placing new component and copying components.
;
;(load-from-path "auto-uref.scm")
;(add-hook! add-component-hook auto-uref)
;(add-hook! copy-component-hook auto-uref)
;
;; Define value of page-offset for auto number on insert.
;; Refdeses will be numbered from integer multiples of page-offset,
;; depending on the lowest refdes value found on the page.
;; If lowest value is 323 and page offset is 100, then next refdes
;; will be 301.
;; Setting to 0 disables the feature.
;
;(auto-uref-set-page-offset 100)


; Define default pin attributes
; Attributes:
;   - Attribute name.
;   - Value of the attribute.
;   - Visibility: #t (visible) or #f (hidden).
;   - Show_list:  a list containing what to show, using
;                 elements like "name" or "value", or an empty list.
(define default-pin-attributes
       '(("pintype"   "unknown" #f ())
	 ("pinlabel"  "unknown" #t ("value"))
	 ("pinnumber" "0"       #t ("value"))
	 ("pinseq"    "0"       #f ())))

; Convert a character into a string
(define char2str
  (lambda (char)
    (list->string (list char))))

; Attribute autoplacement grid
(define autoplace-attributes-grid 50)

; Load the default position of attributes, for attribute autoplacing
; functions.
(load-from-path "default-attrib-positions.scm")

; Adds the default pin attributes to each newly placed pin.
(define (add-default-pin-attributes object)
  (for-each
    (lambda (a)
      (apply add-attribute-to-object object a)) default-pin-attributes))

; Comment in this hook to automatically add the default attributes to
; each newly placed pin
(add-hook! add-pin-hook add-default-pin-attributes)


; Comment in this to load the functions to place the attributes automatically.
(load-from-path "auto-place-attribs.scm")

; Autoplace pin text attributes hook.
; Comment in these if you want the pin attributes to be automatically placed.
; There are different hooks for situations like adding a new pin and rotating
; or mirroring an existing one.
; The #t at the end means that function is appended to the end of the hook.
(add-hook! add-pin-hook (lambda (pin)
	(autoplace-pin-attributes pin )) #t)
;(add-hook! rotate-pin-hook (lambda (pin)
;	(autoplace-pin-attributes pin )) #t)
;(add-hook! mirror-pin-hook (lambda (pin)
;	(autoplace-pin-attributes pin )) #t)

; Autoplace component/net/buses text attributes hook.
; Comment in these if you want the component attributes to be
; automatically placed.
; There are different hooks for situations like adding a new pin, rotating
; or mirroring an existing one, adding a new attribute or a new component.
; The #t at the end means that function is appended to the end of the hook.
;(add-hook! add-component-object-hook (lambda (object)
;	(autoplace-object-attributes object)) #t)
;(add-hook! rotate-component-object-hook (lambda (object)
;	(autoplace-object-attributes object)) #t)
;(add-hook! mirror-component-object-hook (lambda (object)
;	(autoplace-object-attributes object)) #t)
;(add-hook! add-attribute-hook (lambda (object)
;	(autoplace-object-attributes object)) #t)
;(add-hook! complex-place-list-changed-hook (lambda (object)
;         (autoplace-object-attributes object)) #t)

; Autoplace netname= attribute hook.  This autoplaces netname
; attribute at the time that it's added.
(load-from-path "auto-place-netname.scm")
(add-hook! add-objects-hook place-netname-attribute-handler)

; Automatically place a titleblock (or other components) when creating
; a new page.
; Comment in these lines if you want gschem to automatically place a titleblock
; when you create a new _empty_ page.
; Users can customize the default titleblock by adding the following line
; (without the semi-colons at the beginning) to the gschemrc file:
;; (define default-titleblock "title-A4.sym")
;; Change "title-A4.sym" by the name of your preferred titleblock!
;
; If you don't want a titleblock to be added automatically, then add one of
; the following lines to your gschemrc file (without the semicolon).
; There are several ways, so just choose one:
;   (define default-titleblock "")
;   (define default-titleblock '())
;   (define default-titleblock #f)
;
(define default-titleblock "title-B.sym")

; Load the regular expressions module
(if (provided? 'regex)
    (use-modules (ice-9 regex))
    (display "Your Guile installation doesn't provide the regex module.\n"))

(add-hook! (@ (gschem hook) new-page-hook) (lambda (page)
   ; Only place the titleblock if there are no objects in the page
   ; and the page filename ends in ".sym".
   (if (and (null? (get-objects-in-page page))
	    ; If the guile installation doesn't provide the regex module,
	    ; don't care about the page filename.
	    (if (provided? 'regex)
		(not (string-match ".*\\.[sS][yY][mM]"
				   (get-page-filename page)))
		#t))
;      Syntax             Symbol name        X   Y    angle selectable mirrored
       (add-component-at-xy page default-titleblock 40000 40000   0       #f       #f))

   ;; After adding titleblock, reset page to mark as unchanged.
   ((@ (geda page) set-page-dirty!) (active-page) #f))
	   #t)

; Evaluate an expression entered in the magic-colon text box.
; In 20 years this might dispatch to an interpreter for some other language.
(define (invoke-macro s-expr)
  (gschem-log (format #f "~s\n" (eval-string-protected s-expr))))

;
; End of hooks
;

;
; Start of path related keywords
;

; attribute-name string
;
; Specifies the default attributes which are presented to the user in the
; "Add Attribute" dialog box.
; The main purpose of this keyword is to allow the user to add any attributes
; which should be in this dialog box.
; Some of these names are specific for symbols while others are for general
; components or nets.  The attribute names are case sensitive. (change this?)
;
; The order of the attribute-name keywords determines the order they
; are displayed.
;
(attribute-name "netname")
(attribute-name "footprint")
(attribute-name "value")
(attribute-name "refdes")
(attribute-name "source")
(attribute-name "model-name")
(attribute-name "model")
(attribute-name "net")
(attribute-name "device")
(attribute-name "pinnumber")
(attribute-name "pinseq")
(attribute-name "pintype")
(attribute-name "pinlabel")
(attribute-name "numslots")
(attribute-name "slot")
(attribute-name "slotdef")
(attribute-name "graphical")
(attribute-name "description")
(attribute-name "documentation")
(attribute-name "symversion")
(attribute-name "comment")
(attribute-name "author")
(attribute-name "dist-license")
(attribute-name "use-license")
(attribute-name "file")

;
; End of path related keywords
;

;
; Start of stroke related keywords
;

;
; This section defines associations between a stroke sequence and a
; guile function which is executed when the stroke is drawn in the
; gschem window
;
; Strokes are defined as follows:
;
; 1  2  3
;
; 4  5  6
;
; 7  8  9
;
; The sequence of number such as "852" specify how the stroke is drawn.
; Sequence "852" happens to be a vertical line drawn from the bottom going
; up.
;
; Please see the libstroke documentation for further information on the
; stroke description.
;
; For the most part I went a little overboard on the stroke defs, you
; probably can get away with many less stroke defs, but I'm a very
; sloppy stroke drawing person. :-)  Guess my teachers were always
; right-- my handwritting was/is awful.
;
; Be careful here, strokes is a rather large list, and make sure you maintain
; proper ( and )'s.
;

(define strokes
; Letter L for line
  '(("14789" . &add-line)

; Letter Z for zoom window
("125789"   . &view-zoom-box)
("1254789"  . &view-zoom-box)
("1235789"  . &view-zoom-box)
("2354789"  . &view-zoom-box)
("2324789"  . &view-zoom-box)
("12354789" . &view-zoom-box)
("12324789" . &view-zoom-box)
("12365789" . &view-zoom-box)
("1232789"  . &view-zoom-box)

; line up for zoom out
("852" . &view-zoom-out)
; line down for zoom in
("258" . &view-zoom-in)

; Letter C for copy
("3214789" . &edit-copy)
("214789"  . &edit-copy)
("21489"   . &edit-copy)
("32478"   . &edit-copy)

; Letter E for edit
("563214789" . &edit-edit)
("53214789"  . &edit-edit)
("5321478"   . &edit-edit)
("5214789"   . &edit-edit)
("521478"    . &edit-edit)
("453214789" . &edit-edit)
("45321478"  . &edit-edit)
("456321478" . &edit-edit)
("456214789" . &edit-edit)
("45621478"  . &edit-edit)

; Letter N for net
("415963"   . &add-net)
("7414863"  . &add-net)
("74148963" . &add-net)
("74158963" . &add-net)
("7415963"  . &add-net)


; Letter M for move
("741236963"   . &edit-move)
("7412572369"  . &edit-move)
("7412575369"  . &edit-move)
("741258369"   . &edit-move)
("74125852369" . &edit-move)
("7412585369"  . &edit-move)
("74125863"    . &edit-move)
("74126963"    . &edit-move)
("741475369"   . &edit-move)
("7414785369"  . &edit-move)
("74148369"    . &edit-move)
("7414852369"  . &edit-move)
("741485369"   . &edit-move)
("74148669"    . &edit-move)
("741552369"   . &edit-move)
("741575369"   . &edit-move)
("7415852369"  . &edit-move)
("741585369"   . &edit-move)
("74185369"    . &edit-move)
("74255369"    . &edit-move)
("7425852369"  . &edit-move)
("742585369"   . &edit-move)
("7426963"     . &edit-move)
("74585369"    . &edit-move)

; Letter D for delete
("14786321"  . &edit-delete)
("14789621"  . &edit-delete)
("147896321" . &edit-delete)
("15896321"  . &edit-delete)
("257896321" . &edit-delete)
("25896321"  . &edit-delete)
("4789621"   . &edit-delete)

; Letter S for select
("2145987"  . &edit-select )
("215987"   . &edit-select )
("2156987"  . &edit-select )
("21256987" . &edit-select )
("3215987"  . &edit-select )
("32156987" . &edit-select )
("32148987" . &edit-select )
("32145987" . &edit-select )))

;
; End of stroke related keywords
;

;
; Start of keymapping related keywords
;

;;;; Keymapping
;;
;; Everything is case-sensitive.  Any number of keys may be bound in
;; sequence, and each keystroke consists of a non-modifier key with
;; some number of modifiers applied.  Examples:
;;
;;  * (global-set-key "F N" '&file-new-window)
;;
;;    The "New Window" command will be run when an <F> is typed,
;;    followed by an <A>.
;;
;;  * (global-set-key "<Control><Shift>A" '&edit-deselect)
;;
;;    The "Deselect All" command will be run when the <Ctrl> and
;;    <Shift> keys are held down, and the <A> key is pressed.
;;
;;  * (global-set-key "O <Shift>S" '&options-snap-size)
;;
;;    The "Snap Size" dialog box will be shown when an <O> is typed,
;;    followed by an <S> typed with the <Shift> key held down.
;;
;; Key names can be found in /usr/include/gtk-2.0/gdk/gdkkeysyms.h on
;; most Linux systems.  For other systems, please see your platform
;; documentation.
;;
;; Later keybindings override earlier ones.

(global-set-key "A C" '&add-component)
(global-set-key "A A" '&add-attribute)
(global-set-key "A N" '&add-net)
(global-set-key "A U" '&add-bus)
(global-set-key "A T" '&add-text)
(global-set-key "A L" '&add-line)
(global-set-key "A H" '&add-path)
(global-set-key "A B" '&add-box)
(global-set-key "A I" '&add-circle)
(global-set-key "A R" '&add-arc)
(global-set-key "A P" '&add-pin)
(global-set-key "A G" '&add-picture)

(global-set-key "<Control>A" '&edit-select-all)
(global-set-key "<Control><Shift>A" '&edit-deselect)

(global-set-key "B" '&add-box)
(global-set-key "<Shift>B" '&add-bus)
(global-set-key "C" '&edit-copy)
(global-set-key "<Control>C" '&clipboard-copy)
(global-set-key "D" '&edit-delete)

(global-set-key "E <Shift>U" '&edit-undo)
(global-set-key "E <Shift>R" '&edit-redo)
(global-set-key "E S" '&edit-select)
(global-set-key "E C" '&edit-copy)
(global-set-key "E E" '&edit-edit)
(global-set-key "E Y" '&edit-mcopy)
(global-set-key "E X" '&edit-text)
(global-set-key "E M" '&edit-move)
(global-set-key "E D" '&edit-delete)
(global-set-key "E R" '&edit-rotate-90)
(global-set-key "E I" '&edit-mirror)
(global-set-key "E <Shift>S" '&edit-slot)
(global-set-key "E O" '&edit-color)
(global-set-key "E L" '&edit-lock)
(global-set-key "E <Shift>L" '&edit-unlock)
(global-set-key "E W" '&edit-linetype)
(global-set-key "E F" '&edit-filltype)
(global-set-key "E P" '&edit-pin-type)
(global-set-key "E T" '&edit-translate)
(global-set-key "E <Shift>colon" '&edit-invoke-macro)
(global-set-key "E B" '&edit-embed)
(global-set-key "E <Shift>B" '&edit-unembed)
(global-set-key "E U" '&edit-update)
(global-set-key "E N" '&edit-show-hidden)

(global-set-key "F W" '&file-new-window)
(global-set-key "F N" '&file-new)
(global-set-key "F O" '&file-open)
(global-set-key "F S" '&file-save)
(global-set-key "F E" '&page-close)
(global-set-key "F A" '&file-save-as)
(global-set-key "F L" '&file-save-all)
(global-set-key "F P" '&file-print)
(global-set-key "F R" '&page-revert)
(global-set-key "F I" '&file-image)
(global-set-key "F T" '&file-script)
(global-set-key "F <Shift>R" '&file-repl)
(global-set-key "F C" '&file-close-window)
(global-set-key "F Q" '&file-quit)

(global-set-key "H A" '&help-about)
(global-set-key "H M" '&help-manual)
(global-set-key "H F" '&help-faq)
(global-set-key "H G" '&help-guide)
(global-set-key "H W" '&help-wiki)
(global-set-key "H H" '&help-hotkeys)
(global-set-key "H C" '&hierarchy-documentation)

(global-set-key "<Shift>H D" '&hierarchy-down-schematic)
(global-set-key "<Shift>H S" '&hierarchy-down-symbol)
(global-set-key "<Shift>H U" '&hierarchy-up)
(global-set-key "<Shift>H O" '&hierarchy-documentation)

(global-set-key "I" '&add-component)
(global-set-key "L" '&add-line)
(global-set-key "M" '&edit-move)
(global-set-key "N" '&add-net)

(global-set-key "O T" '&options-text-size)
(global-set-key "O A" '&options-action-feedback)
(global-set-key "O G" '&options-grid)
(global-set-key "O S" '&options-snap)
(global-set-key "O R" '&options-rubberband)
(global-set-key "O M" '&options-magneticnet)
(global-set-key "O <Shift>S" '&options-snap-size)
(global-set-key "O L" '&options-show-log-window)
(global-set-key "O C" '&options-show-coord-window)

(global-set-key "P M" '&page-manager)
(global-set-key "P N" '&page-next)
(global-set-key "P P" '&page-prev)
(global-set-key "P R" '&page-revert)
(global-set-key "P C" '&page-close)
(global-set-key "P <Shift>P" '&page-print)

(global-set-key "<Alt>Q" '&file-quit)
(global-set-key "R" '&view-redraw)
(global-set-key "<Shift>R" '&edit-redo)
(global-set-key "S" '&edit-select)

(global-set-key "T A" '&attributes-attach)
(global-set-key "T D" '&attributes-detach)
(global-set-key "T N" '&attributes-show-name)
(global-set-key "T V" '&attributes-show-value)
(global-set-key "T B" '&attributes-show-both)
(global-set-key "T T" '&attributes-visibility-toggle)
(global-set-key "T <Shift>F" '&edit-find-text)
(global-set-key "T H" '&edit-hide-text)
(global-set-key "T <Shift>H" '&edit-show-text)
(global-set-key "T U" '&edit-autonumber)

(global-set-key "U" '&edit-undo)
(global-set-key "<Shift>U" '&edit-undo)

(global-set-key "V A" '&view-sidebar)
(global-set-key "V S" '&view-status)
(global-set-key "V R" '&view-redraw)
(global-set-key "V B" '&view-zoom-box)
(global-set-key "V F" '&view-zoom-full)
(global-set-key "V E" '&view-zoom-extents)
(global-set-key "V P" '&view-pan)
(global-set-key "V O" '&view-zoom-out)
(global-set-key "V I" '&view-zoom-in)
(global-set-key "V D" '&view-dark-colors)
(global-set-key "V L" '&view-light-colors)
(global-set-key "V W" '&view-bw-colors)

(global-set-key "<Control>V" '&clipboard-paste)
(global-set-key "W" '&view-zoom-box)
(global-set-key "X" '&view-pan)
(global-set-key "<Control>X" '&clipboard-cut)

;(global-set-key "Y C" 'buffer-copy1)
;(global-set-key "Y U" 'buffer-cut1)
;(global-set-key "Y P" 'buffer-paste1)

(global-set-key "<Control>Y" '&edit-redo)
(global-set-key "Z" '&view-zoom-in)
(global-set-key "<Shift>Z" '&view-zoom-out)
(global-set-key "<Control>Z" '&edit-undo)

(global-set-key "Escape" '&cancel)
(global-set-key "bracketright" '&options-scale-up-snap-size)
(global-set-key "bracketleft" '&options-scale-down-snap-size)
(global-set-key "Left" '&view-pan-left)
(global-set-key "Right" '&view-pan-right)
(global-set-key "Up" '&view-pan-up)
(global-set-key "Down" '&view-pan-down)
(global-set-key "period" '&repeat-last-action)
(global-set-key "colon" '&edit-invoke-macro)
(global-set-key "Delete" '&edit-delete)
(global-set-key "greater" '&page-next)
(global-set-key "Page_Down" '&page-next)
(global-set-key "less" '&page-prev)
(global-set-key "Page_Up" '&page-prev)

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
           (,(N_ "_Revert")           &page-revert            "gtk-revert-to-saved")
           ("SEPARATOR"               #f                      #f                      #f)
           (,(N_ "_Print...")         &file-print             "gtk-print")
           (,(N_ "Write _image...")   &file-image             #f)
           ("SEPARATOR"               #f                      #f                      #f)
           (,(N_ "Execute Script...") &file-script            "gtk-execute")
           (,(N_ "REPL...")           &file-repl              "gtk-execute")
           ("SEPARATOR"               #f                      #f                      #f)
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
           (,(N_ "Edit...")            &edit-edit             #f)
           (,(N_ "Edit Text...")       &edit-text             "gtk-edit")
           (,(N_ "Slot...")            &edit-slot             #f)
           (,(N_ "Color...")           &edit-color            "gtk-select-color")
           (,(N_ "Line Width & Type...") &edit-linetype       #f)
           (,(N_ "Fill Type...")         &edit-filltype       #f)
           (,(N_ "Pin Type...")        &edit-pin-type         #f)
           (,(N_ "Symbol Translate...")  &edit-translate      #f)
           (,(N_ "Lock")               &edit-lock             #f)
           (,(N_ "Unlock")             &edit-unlock           #f)
           ("SEPARATOR"                #f                     #f)
           (,(N_ "Invoke Macro")         &edit-invoke-macro   #f)
           (,(N_ "Embed Component/Picture")    &edit-embed    #f)
           (,(N_ "Unembed Component/Picture")  &edit-unembed  #f)
           (,(N_ "Update Component")   &edit-update           "gtk-refresh")
           (,(N_ "Show/Hide Inv Text") &edit-show-hidden      #f)))

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
        `( (,(N_ "Sidebar")             &view-sidebar           #f)
           (,(N_ "Status")              &view-status            #f)
           ("SEPARATOR"                 #f                      #f)
           (,(N_ "_Redraw")             &view-redraw            "gtk-refresh")
           (,(N_ "_Pan")                &view-pan               #f)
           (,(N_ "Zoom _Box")           &view-zoom-box          #f)
           (,(N_ "Zoom _Extents")       &view-zoom-extents      "gtk-zoom-fit")
           (,(N_ "Zoom _In")            &view-zoom-in           "gtk-zoom-in")
           (,(N_ "Zoom _Out")           &view-zoom-out          "gtk-zoom-out")
           (,(N_ "Zoom _Full")          &view-zoom-full         #f)
           ("SEPARATOR"                 #f                      #f)
           (,(N_ "_Dark color scheme")  &view-dark-colors       #f)
           (,(N_ "_Light color scheme") &view-light-colors      #f)
           (,(N_ "B_W color scheme")    &view-bw-colors         #f)
         ))

(define page-menu-items
;;
;;          menu item name      menu action             menu stock icon
;;
        `( (,(N_ "_Manager...")       &page-manager           #f)
           (,(N_ "_Previous")         &page-prev              "gtk-go-back")
           (,(N_ "_Next")             &page-next              "gtk-go-forward")
           (,(N_ "_Revert")           &page-revert            "gtk-revert-to-saved")
           (,(N_ "_Close")            &page-close             "gtk-close")))

(define add-menu-items
;;
;;          menu item name      menu action             menu stock icon
;;
        `( (,(N_ "_Component...")     &add-component   "insert-symbol")
           (,(N_ "_Net")              &add-net         "insert-net")
           (,(N_ "B_us")              &add-bus         "insert-bus")
           (,(N_ "_Attribute...")     &add-attribute   "insert-attribute")
           (,(N_ "_Text...")          &add-text        "insert-text")
           ("SEPARATOR"              #f)
           (,(N_ "_Line")             &add-line        "insert-line")
           (,(N_ "Pat_h")             &add-path        "insert-path")
           (,(N_ "_Box")              &add-box         "insert-box")
           (,(N_ "C_ircle")           &add-circle      "insert-circle")
           (,(N_ "A_rc")              &add-arc         "insert-arc")
           (,(N_ "_Pin")              &add-pin         "insert-pin")
           (,(N_ "Pictu_re...")       &add-picture     "insert-image")))

(define hierarchy-menu-items
;;
;;          menu item name      menu action               menu stock icon
;;
        `( (,(N_ "_Down Schematic")   &hierarchy-down-schematic "gtk-go-down")
           (,(N_ "Down _Symbol")      &hierarchy-down-symbol    "gtk-goto-bottom")
           (,(N_ "_Up")               &hierarchy-up             "gtk-go-up")
           (,(N_ "D_ocumentation...") &hierarchy-documentation  "symbol-datasheet")))

(define attributes-menu-items
;;
;;          menu item name      menu action             menu stock icon
;;
        `( (,(N_ "_Attach")           &attributes-attach      "attribute-attach")
           (,(N_ "_Detach")           &attributes-detach      "attribute-detach")
           (,(N_ "Show _Value")       &attributes-show-value  "attribute-show-value")
           (,(N_ "Show _Name")        &attributes-show-name   "attribute-show-name")
           (,(N_ "Show _Both")        &attributes-show-both   "attribute-show-both")
           (,(N_ "_Toggle Visibility")  &attributes-visibility-toggle   #f)
           (,(N_ "_Find Specific Text...")  &edit-find-text   "gtk-find")
           (,(N_ "_Hide Specific Text...")  &edit-hide-text   #f)
           (,(N_ "_Show Specific Text...")  &edit-show-text   #f)
           (,(N_ "A_utonumber Text...")     &edit-autonumber  #f)))

(define options-menu-items
;;
;;          menu item name      menu action             menu stock icon
;;
        `( (,(N_ "_Text Size...")            &options-text-size)
           (,(N_ "Cycle _grid styles")       &options-grid)
           (,(N_ "Toggle _Snap On/Off")      &options-snap)
           (,(N_ "Snap Grid S_pacing...")    &options-snap-size)
           (,(N_ "Scale _up Grid Spacing")   &options-scale-up-snap-size)
           (,(N_ "Scale _down Grid Spacing") &options-scale-down-snap-size)
           (,(N_ "Toggle _Outline/Box")      &options-action-feedback)
           (,(N_ "Toggle Net _Rubberband")   &options-rubberband)
           (,(N_ "Toggle _Magnetic Net")     &options-magneticnet)
           (,(N_ "Show _Log Window...")      &options-show-log-window)
           (,(N_ "Show _Coord Window...")    &options-show-coord-window)))

(define help-menu-items
;;
;;          menu item name                menu action               menu stock icon
;;
        `(
           (,(N_ "gschem User _Guide...")  &help-guide                "gtk-help")
           (,(N_ "gschem _FAQ...")         &help-faq                  "help-faq")
           (,(N_ "gEDA Docu_mentation...") &help-manual               "help-browser")
           (,(N_ "gEDA _Wiki...")          &help-wiki                 "web-browser")
           (,(N_ "Component D_ocumentation...") &hierarchy-documentation   "symbol-datasheet")
           ("SEPARATOR"                   #f                        #f)
           (,(N_ "_Hotkeys...")            &help-hotkeys              "preferences-desktop-keyboard-shortcuts")
           (,(N_ "_About...")              &help-about                "gtk-about")))

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
(add-menu (N_ "_Help") help-menu-items)

;
; End of keymapping related keywords
;


;;
;; Major modes
;;

;; Comment in this scheme code if you want to link with pcb
;;
;; Please note that the code in pcb.scm is still highly experimental
;; and there are known (and easy) ways to crash pcb and/or gschem with this code.
;; The short answer is neither program likes a pipe to break.
;;
; (load-from-path "pcb.scm")
