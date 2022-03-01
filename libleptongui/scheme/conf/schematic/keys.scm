;
; Start of keymapping related keywords
;

(use-modules (schematic gui keymap))

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
(global-set-key "E L" '&edit-lock)
(global-set-key "E <Shift>L" '&edit-unlock)
(global-set-key "E T" '&edit-translate)
(global-set-key "E <Shift>colon" '&edit-invoke-macro)
(global-set-key "E O" '&edit-object-properties )
(global-set-key "E B" '&edit-embed)
(global-set-key "E <Shift>B" '&edit-unembed)
(global-set-key "E U" '&edit-update)
(global-set-key "E N" '&edit-show-hidden)
(global-set-key "T I" '&edit-show-hidden)

(global-set-key "F W" '&file-new-window)
(global-set-key "F N" '&file-new)
(global-set-key "<Control>N" '&file-new)
(global-set-key "F O" '&file-open)
(global-set-key "<Control>O" '&file-open)
(global-set-key "F S" '&file-save)
(global-set-key "<Control>S" '&file-save)
(global-set-key "F E" '&page-close)
(global-set-key "F A" '&file-save-as)
(global-set-key "F L" '&file-save-all)
(global-set-key "F P" '&file-print)
(global-set-key "<Control>P" '&file-print)
(global-set-key "F R" '&page-revert)
(global-set-key "F I" '&file-image)
(global-set-key "F T" '&file-script)
(global-set-key "F <Shift>R" '&file-repl)
(global-set-key "F C" '&file-close-window)
(global-set-key "F Q" '&file-quit)

(global-set-key "H A" '&help-about)
(global-set-key "H M" '&help-manual)
(global-set-key "H W" '&help-wiki)
(global-set-key "H H" '&help-hotkeys)
(global-set-key "H C" '&hierarchy-documentation)

(global-set-key "<Shift>H D" '&hierarchy-down-schematic)
(global-set-key "<Shift>H S" '&hierarchy-down-symbol)
(global-set-key "<Shift>H U" '&hierarchy-up)
(global-set-key "H O" '&hierarchy-documentation)

(global-set-key "I" '&add-component)
(global-set-key "L" '&add-line)
(global-set-key "M" '&edit-move)
(global-set-key "N" '&add-net)

(global-set-key "O A" '&options-action-feedback)
(global-set-key "O G" '&options-grid)
(global-set-key "O S" '&options-snap)
(global-set-key "O R" '&options-rubberband)
(global-set-key "O M" '&options-magneticnet)
(global-set-key "O O" '&options-snap-size)
(global-set-key "O L" '&options-show-log-window)
(global-set-key "O C" '&options-show-coord-window)
(global-set-key "O F" '&options-select-font)
(global-set-key "O I" '&options-draw-grips)

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
(global-set-key "T F" '&edit-find-text)
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

; new zoom control keys:
;
(global-set-key "equal" '&view-zoom-in)
(global-set-key "minus" '&view-zoom-out)
(global-set-key "0"     '&view-zoom-extents)
(global-set-key "<Control>equal" '&view-zoom-in)
(global-set-key "<Control>minus" '&view-zoom-out)
(global-set-key "<Control>0"     '&view-zoom-extents)
(global-set-key "KP_Add"      '&view-zoom-in )
(global-set-key "KP_Subtract" '&view-zoom-out )
(global-set-key "KP_Multiply" '&view-zoom-extents )

(global-set-key "<Control>V" '&clipboard-paste)
(global-set-key "W" '&view-zoom-box)
(global-set-key "X" '&view-pan)
(global-set-key "<Control>X" '&clipboard-cut)

;(global-set-key "Y C" 'buffer-copy1)
;(global-set-key "Y U" 'buffer-cut1)
;(global-set-key "Y P" 'buffer-paste1)

(global-set-key "<Control>Y" '&edit-redo)
(global-set-key "<Control><Shift>Z" '&edit-redo)
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
; End of keymapping related keywords
;

