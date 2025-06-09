; Lepton EDA
; Backward-compatible color-map.scm file
; Copyright (C) 2020 Lepton EDA Contributors
; License: GPLv2+. See the COPYING file


( use-modules
(
  ( lepton color-map )
  #:select
  (
    %color-name-map
    color-map-name-to-index
    color-map-name-from-index
  )
)
)



; Top-level code:
;
( format (current-error-port)
"
WARNING: You are loading color-map.scm file, which is deprecated.
Please use the new (lepton color-map) module instead.

"
)

