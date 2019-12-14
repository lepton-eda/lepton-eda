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

