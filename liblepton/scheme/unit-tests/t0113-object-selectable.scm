;; Test procedures for Scheme functions:
;; - object-selectable?
;; - set-object-selectable!

( use-modules ( unit-test ) )

( use-modules ( geda object ) )
( use-modules ( geda page   ) )



( begin-test 'object-selectable
( let
  (
  ( page #f )
  ( obj  #f )
  ( tmp  #f )
  )

  ; create a page and an object:
  ;
  ( set! page (make-page "/test/page/A") )
  ( set! obj  (make-box  '(1 . 4) '(3 . 2)) )
  ( page-append! page obj )


  ; obj should be initially unlocked (i.e. selectable):
  ;
  ( assert-true (object-selectable? obj) )


  ; clear the page modification flag and lock the object:
  ;
  ( set-page-dirty! page #f )
  ( set! tmp (set-object-selectable! obj #f) )

  ; set-object-selectable!() should return the object:
  ;
  ( assert-equal tmp obj )

  ; ensure obj is now locked:
  ;
  ( assert-false (object-selectable? obj) )

  ; ensure the page modification flag is set:
  ;
  ( assert-true (page-dirty? page) )


  ; clear the page modification flag and lock the object again:
  ;
  ( set-page-dirty! page #f )
  ( set-object-selectable! obj #f )

  ; ensure the page modification flag is NOT set (obj is not modified):
  ;
  ( assert-false (page-dirty? page) )


  ; clear the page modification flag and unlock the object:
  ;
  ( set-page-dirty! page #f )
  ( set! tmp (set-object-selectable! obj #t) )

  ; set-object-selectable!() should return the object:
  ;
  ( assert-equal tmp obj )

  ; ensure obj is now unlocked:
  ;
  ( assert-true (object-selectable? obj) )

  ; ensure the page modification flag is set:
  ;
  ( assert-true (page-dirty? page) )


  ( close-page! page )

) ; let
) ; 'object-selectable()

