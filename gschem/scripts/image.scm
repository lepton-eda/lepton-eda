;; $Id$
;;
;; This file may be used to produce png files from gschem schematics from the
;; command line.  Typical usage is:
;;
;;   gschem -p -o mysch.png -s /path/to/this/file/image.scm mysch.sch
;;
;; The schematic in "mysch.sch" will be exported to the file "mysch.png"

(image-size 1024 768)
;(image-size 3200 2400)
(image-color "enabled")
;(image-color "disabled")

; You need call this after you call any rc file function
(gschem-use-rc-values)

; filename is specified on the command line
(gschem-image "dummyfilename")

(gschem-exit)
