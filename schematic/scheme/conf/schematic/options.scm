;
; Start of mode related keywords
;

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
(undo-levels 20)

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
;(undo-panzoom "enabled")
(undo-panzoom "disabled")


; autosave interval
;
; Controls if a backup copy is made every "interval" seconds.
; Note that the backup copy is made when you make some change to the schematic,
; and there were more than "interval" seconds from the last autosave.
; Autosaving will not be allowed if setting it to zero.
(auto-save-interval 120)

;  net-direction-mode string
;
;  Controls if the net direction mode is used. This mode tries to guess
;  the best continuation direction of a L-shape net when adding a net.
;
(net-direction-mode "enabled")
;(net-direction-mode "disabled")

; net-selection-mode string
;
; Controls how many net segments are selected when you click at a net.
;
; - enabled_all:
;   - first click selects the net itself
;   - second click selects all nets directly connected to the selected one
;   - third click in addition selects all nets with equal "netname" attributes
; - enabled_net:
;   - first click selects the net itself
;   - second click selects all nets directly connected to the selected one
; - disabled:
;   - mouse clicks just selects the clicked net

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
;(log-window "startup")
(log-window "later")


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



; middle-button string
;
; Controls if the middle mouse button draws strokes, repeats the last
; command, does an action (move and copy (holding down the ALT key)
; are supported) on a single objects, performs the popup,
; or if it does the mouse panning.
;
(middle-button "mousepan")
;(middle-button "popup")
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
;(warp-cursor "enabled")
(warp-cursor "disabled")


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

