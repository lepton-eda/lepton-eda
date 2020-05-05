;;; Test procedures for Scheme functions:
;;; - object-selectable?
;;; - set-object-selectable!

(use-modules ((geda object) #:renamer (symbol-prefix-proc 'geda:))
             (lepton page))

(test-begin "geda:object-selectable")
( let
  (
  ( page #f )
  ( obj  #f )
  ( tmp  #f )
  )

  ; create a page and an object:
  ;
  ( set! page (make-page "/test/page/A") )
  ( set! obj  (geda:make-box  '(1 . 4) '(3 . 2)) )
  ( page-append! page obj )


  ; obj should be initially unlocked (i.e. selectable):
  ;
  ( test-assert (geda:object-selectable? obj) )


  ; clear the page modification flag and lock the object:
  ;
  ( set-page-dirty! page #f )
  ( set! tmp (geda:set-object-selectable! obj #f) )

  ; geda:set-object-selectable!() should return the object:
  ;
  ( test-equal tmp obj )

  ; ensure obj is now locked:
  ;
  (test-assert (not (geda:object-selectable? obj)))

  ; ensure the page modification flag is set:
  ;
  ( test-assert (page-dirty? page) )


  ; clear the page modification flag and lock the object again:
  ;
  ( set-page-dirty! page #f )
  ( geda:set-object-selectable! obj #f )

  ; ensure the page modification flag is NOT set (obj is not modified):
  ;
  (test-assert (not (page-dirty? page) ))


  ; clear the page modification flag and unlock the object:
  ;
  ( set-page-dirty! page #f )
  ( set! tmp (geda:set-object-selectable! obj #t) )

  ; geda:set-object-selectable!() should return the object:
  ;
  ( test-equal tmp obj )

  ; ensure obj is now unlocked:
  ;
  ( test-assert (geda:object-selectable? obj) )

  ; ensure the page modification flag is set:
  ;
  ( test-assert (page-dirty? page) )


  ( close-page! page )

  )

(test-end "geda:object-selectable")
