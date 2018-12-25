;; Test Scheme procedures for object-locked? and set-object-locked! functions

( use-modules ( unit-test ) )

( use-modules ( geda object ) )
( use-modules ( geda page   ) )



( begin-test 'object-locked
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


  ; obj should be initially unlocked:
  ;
  ( assert-false (object-locked? obj) )


  ; clear the page modification flag and lock the object:
  ;
  ( set-page-dirty! page #f )
  ( set! tmp (set-object-locked! obj #t) )

  ; set-object-locked!() should return the object:
  ;
  ( assert-equal tmp obj )

  ; ensure obj is now locked:
  ;
  ( assert-true (object-locked? obj) )

  ; ensure the page modification flag is set:
  ;
  ( assert-true (page-dirty? page) )


  ; clear the page modification flag and lock the object again:
  ;
  ( set-page-dirty! page #f )
  ( set-object-locked! obj #t )

  ; ensure the page modification flag is NOT set (obj is not modified):
  ;
  ( assert-false (page-dirty? page) )


  ; clear the page modification flag and unlock the object:
  ;
  ( set-page-dirty! page #f )
  ( set! tmp (set-object-locked! obj #f) )

  ; set-object-locked!() should return the object:
  ;
  ( assert-equal tmp obj )

  ; ensure obj is now unlocked:
  ;
  ( assert-false (object-locked? obj) )

  ; ensure the page modification flag is set:
  ;
  ( assert-true (page-dirty? page) )


  ( close-page! page )

) ; let
) ; 'object-locked()

