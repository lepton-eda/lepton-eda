;
; Start of stroke related keywords
;

;
; This section defines associations between a stroke sequence and a
; guile function which is executed when the stroke is drawn in the
; lepton-schematic window
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

