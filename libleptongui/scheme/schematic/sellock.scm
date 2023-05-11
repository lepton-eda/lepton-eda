;; Lepton EDA
;; ( schematic sellock ) module: select locked components.
;; Copyright (C) 2020-2023 dmn <graahnul.grom@ya.ru>
;; License: GPLv2+. See the COPYING file.
;;

( define-module ( schematic sellock )
  #:use-module ( srfi srfi-1 )
  #:use-module ( system foreign )
  #:use-module ( schematic window global )
  #:use-module ( schematic ffi )
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


( define ( next-in-list val lst )
  ; return:
  ( if ( eq? val (last lst) )
    ( first lst )             ; if
    ( cadr (member val lst) ) ; else (cadr: car of cdr)
  )
)


( define ( bind2nd val ) ; returns predicate func taking 1 arg
  ( lambda( arg ) (eq? arg val) )
)


; public:
;
( define ( select-locked )

  ( define comps ( filter component? (page-contents (active-page)) ) )
  ( define locked-comps ( remove object-selectable? comps ) )
  ( define comp ( selected-locked-comp comps ) )

  ( define ( select-and-report c )
    ( select-comp c )
    ( log!
        'message
        ( G_ "Select locked: ~a of ~a (~a)~a" )
        ( 1+ (list-index (bind2nd c) locked-comps) )
        ( length locked-comps )
        ( component-basename c )
        ( G_", press <E E> to edit, <E Shift+L> to unlock" )
    )
    ( i_show_state
      ( *current-window )
      ( string->pointer
        ( format
          #f
          ( G_ "Locked: ~a of ~a" )
          ( 1+ (list-index (bind2nd c) locked-comps) )
          ( length locked-comps )
        )
      )
    )
  )

  ( deselect-all )

  ( if ( null? locked-comps )
    ( schematic-message-dialog (G_ "No locked components") )
    ( if comp
      ( select-and-report (next-in-list comp locked-comps) )
      ( select-and-report (first locked-comps) )
    )
  )

) ; select-locked()

