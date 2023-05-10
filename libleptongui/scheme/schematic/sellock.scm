;; Lepton EDA
;; ( schematic sellock ) module: select locked components.
;; Copyright (C) 2020-2023 dmn <graahnul.grom@ya.ru>
;; License: GPLv2+. See the COPYING file.
;;

( define-module ( schematic sellock )
  #:use-module ( srfi srfi-1 )
  #:use-module ( lepton log )
  #:use-module ( lepton page )
  #:use-module ( lepton attrib )
  #:use-module ( lepton object )
  #:use-module ( schematic window )
  #:use-module ( schematic selection )
  #:use-module ( schematic dialog )
  #:use-module ( schematic gettext )

  #:export ( select-locked )
)


( define ( deselect-all )
  ( for-each deselect-object! (page-contents (active-page)) )
)


( define ( select-comp comp )
  ( select-object! comp )
  ( for-each select-object! (object-attribs comp) )
)


( define ( selected-locked-comp comps )
  ( define sel ( page-selection (active-page) ) )
  ( define sel-locked ( remove object-selectable? sel ) )
  ( define sel-locked-comps ( filter component? sel-locked ) )
  ; return:
  ( if ( = (length sel-locked-comps) 1 )
    ( first sel-locked-comps ) ; if
    #f                         ; else
  )
)


; public:
;
( define ( select-locked )
( let*
  (
  ( comps ( filter component? (page-contents (active-page)) ) )
  ( locked-comps ( remove object-selectable? comps ) )
  ( comp ( selected-locked-comp comps ) )
  )

    ( deselect-all )

    ( if ( null? locked-comps )
      ( schematic-message-dialog ( G_ "No locked components" ) )
      ( if ( not comp )
        ( select-comp (first locked-comps) )
      )
    )

) ; let
) ; select-locked()

