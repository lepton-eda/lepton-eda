
(output-orientation "portrait")
(output-type "limits")
(output-color "enabled")
(output-text "ps")

; You need call this after you call any rc file function
(gschem-use-rc-values)

; filename is specified on the command line
(gschem-print "dummyfilename")

(gschem-exit)
