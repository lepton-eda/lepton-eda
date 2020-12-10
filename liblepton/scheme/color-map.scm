; Lepton EDA
; Backward-compatible color-map.scm file
; Copyright (C) 2020 Lepton EDA Contributors
; License: GPLv2+. See the COPYING file


; Association list: %color-name-map
;
; Defines mapping between color symbols
; like 'background, 'net, 'text, etc.
; and integer values.
;
( define %color-name-map
  ( @ (lepton color-map) %color-name-map )
)


; Function: color-map-name-to-index()
;
; Returns an integer index given one of a color
; symbols like 'background, 'net, 'text, etc.
; Returns its argument if wrong symbol is specified.
;
( define color-map-name-to-index
  ( @ (lepton color-map) color-map-name-to-index )
)


; Function: color-map-name-from-index()
;
; Returns one of a color symbols like 'background,
; 'net, 'text, etc., given an integer index.
; Returns its argument if wrong index is specified.
;
( define color-map-name-from-index
  ( @ (lepton color-map) color-map-name-from-index )
)



; Top-level code:
;
( format (current-error-port)
"
WARNING: You are loading color-map.scm file, which is deprecated.
Please use the new (lepton color-map) module instead.

"
)

