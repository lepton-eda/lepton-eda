
(image-size 1024 768)
;(image-size 3200 2400)
(image-color "enabled")
;(image-color "disabled")

; You need call this after you call any rc file function
(gschem-use-rc-values)

; filename is specified on the command line
(gschem-image "dummyfilename")

(gschem-exit)
