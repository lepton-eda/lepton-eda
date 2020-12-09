; Lepton EDA
; Backward-compatible color-map.scm file
; Copyright (C) 2020 Lepton EDA Contributors
; License: GPLv2+. See the COPYING file

( use-modules ( lepton color-map ) )


; Function: color-map-name-to-index()
;
; Returns an integer index given one of a color
; symbols like 'background, 'net, 'text, etc.
; Returns #f (false) if wrong symbol is specified.
;
( define ( color-map-name-to-index sym-name )
  ; return:
  ( assoc-ref %color-name-map sym-name )
)


; Function: color-map-name-from-index()
;
; Returns one of a color symbols like 'background,
; 'net, 'text, etc., given an integer index.
; Returns #f (false) if index is out of bounds.
;
( define ( color-map-name-from-index index )
  ( define ( swap-pair ent )
    ( cons (cdr ent) (car ent) )
  )
  ( define ( invert-mapping )
    ( map swap-pair %color-name-map )
  )

  ; return:
  ( assoc-ref (invert-mapping) index )
)

