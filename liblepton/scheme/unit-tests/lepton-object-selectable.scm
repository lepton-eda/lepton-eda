;;; Test procedures for Scheme functions:
;;; - object-selectable?
;;; - set-object-selectable!

(use-modules (lepton object)
             (lepton page))

(test-begin "object-selectable")

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
  ( test-assert (object-selectable? obj) )


  ; clear the page modification flag and lock the object:
  ;
  ( set-page-dirty! page #f )
  ( set! tmp (set-object-selectable! obj #f) )

  ; set-object-selectable!() should return the object:
  ;
  ( test-equal tmp obj )

  ; ensure obj is now locked:
  ;
  (test-assert (not (object-selectable? obj)))

  ; ensure the page modification flag is set:
  ;
  ( test-assert (page-dirty? page) )


  ; clear the page modification flag and lock the object again:
  ;
  ( set-page-dirty! page #f )
  ( set-object-selectable! obj #f )

  ; ensure the page modification flag is NOT set (obj is not modified):
  ;
  (test-assert (not (page-dirty? page)))


  ; clear the page modification flag and unlock the object:
  ;
  ( set-page-dirty! page #f )
  ( set! tmp (set-object-selectable! obj #t) )

  ; set-object-selectable!() should return the object:
  ;
  ( test-equal tmp obj )

  ; ensure obj is now unlocked:
  ;
  ( test-assert (object-selectable? obj) )

  ; ensure the page modification flag is set:
  ;
  ( test-assert (page-dirty? page) )


  ( close-page! page )

  )

(test-end "object-selectable")

(test-begin "object-selectable-wrong-argument")

(test-assert-thrown 'wrong-type-arg (object-selectable? 'a))
(test-assert-thrown 'wrong-type-arg (set-object-selectable! 'a #t))
(test-assert-thrown 'wrong-type-arg (set-object-selectable! 'a #f))

(test-end "object-selectable-wrong-argument")
