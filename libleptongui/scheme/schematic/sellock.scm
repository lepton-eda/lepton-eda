;; Lepton EDA
;; ( schematic sellock ) module: select locked components.
;; Copyright (C) 2020-2023 dmn <graahnul.grom@ya.ru>
;; License: GPLv2+. See the COPYING file.
;;

( define-module ( schematic sellock )
  #:use-module ( lepton log )
  #:use-module ( lepton page )
  #:use-module ( lepton attrib )
  #:use-module ( schematic window )
  #:use-module ( schematic selection )

  #:export ( select-locked )
)


( define ( deselect-all )
  ( for-each deselect-object! (page-contents (active-page)) )
)


( define ( select-comp comp )
  ( select-object! comp )
  ( for-each select-object! (object-attribs comp) )
)


; public:
;
( define ( select-locked )
    ( log! 'message ".. select-locked()" )

    ( deselect-all )
)

