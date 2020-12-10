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


( define color-map-name-to-index
  ( @ (lepton color-map) color-map-name-to-index )
)


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

