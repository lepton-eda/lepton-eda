;; $Id$
;;
;; This file may be used to print gschem schematics from the
;; command line.  Typical usage is:
;;
;;   gschem -p -o mysch.ps -s /path/to/this/file/print.scm mysch.sch
;;
;; The schematic in "mysch.sch" will be printed to the file "mysch.ps"

;; Uncomment these to override defaults when printing from the command line
;(print-orientation "portrait")
;(print-color "enabled")
;(print-paper "na-letter")

; You need call this after you call any rc file function
(gschem-use-rc-values)

; filename is specified on the command line
(gschem-pdf "dummyfilename")

(format (current-error-port)
"WARNING: Exporting with 'print.scm' is deprecated.  Also, it now
outputs PDF rather than PostScript, which is probably not what you
want.

Please use `gaf export' instead.  See `man gaf' for details.

")

(gschem-exit)
