;;; Test procedures for Scheme functions:
;;; - object-embedded?
;;; - set-object-embedded!

(use-modules (lepton object)
             (lepton page)
             (lepton library component))

( define ( mk-component ) ; create and return a component object

  ( with-output-to-file "unit-tests/dummy.sym"
    ( lambda()
      ( format #t "v 20191003 2~%" )
      ( format #t "B 0 0 500 500 3 10 1 0 -1 -1 0 -1 -1 -1 -1 -1~%" )
      ( format #t "T 0 600 21 6 1 0 0 0 1~%" )
      ( format #t "refdes=R?" )
    )
  )

  ( component-library "unit-tests" ) ; cwd is liblepton/scheme/

  ; return:
  ( make-component/library "dummy.sym"     ; basename
                            (cons 100 100) ; position
                            0              ; angle
                            #f             ; mirror
                            #f             ; locked
  )

) ; mk-component()



( define ( mk-picture ) ; create and return a picture object

  ( define dummy-str ; red square 5x5
    "/* XPM */
    static char * dummy_xpm[] = {
    \"5 5 2 1\",
    \" 	c None\",
    \".	c #FF0000\",
    \".....\",
    \".....\",
    \".....\",
    \".....\",
    \".....\"};"
  )

  ( define dummy-vector
    ( map char->integer
      ( string->list dummy-str )
    )
  )

  ( with-output-to-file "unit-tests/dummy.xpm"
    ( lambda()
      ( format #t dummy-str )
    )
  )

  ; return:
  ( make-picture/vector
    dummy-vector           ; vector
    "unit-tests/dummy.xpm" ; filename
    (cons 100 200)         ; top-left
    (cons 200 100)         ; bottom-right
    0                      ; angle
    #f                     ; mirror
  )

) ; mk-picture()



( define ( test-embedded obj )
( let
  (
  ( page #f )
  ( tmp  #f )
  )

  ; create a page and add an object to it:
  ;
  ( set! page ( make-page "/test/page/A" ) )
  ( page-append! page obj )

  ; obj should be unembedded:
  ;
  (test-assert (not (object-embedded? obj)))


  ; clear the page modification flag and embed the object:
  ;
  ( set-page-dirty! page #f )
  ( set! tmp (set-object-embedded! obj #t) )

  ; set-object-embedded!() should return the object:
  ;
  ( test-equal tmp obj )

  ; ensure obj is now embedded:
  ;
  ( test-assert (object-embedded? obj) )

  ; ensure the page modification flag is set:
  ;
  ( test-assert (page-dirty? page) )


  ; clear the page modification flag and try to embed the object again:
  ;
  ( set-page-dirty! page #f )
  ( set-object-embedded! obj #t )

  ; ensure the page modification flag is NOT set (obj is not modified):
  ;
  (test-assert (not (page-dirty? page)))

  ; clear the page modification flag and unembed the object:
  ;
  ( set-page-dirty! page #f )
  ( set! tmp (set-object-embedded! obj #f) )

  ; set-object-embedded!() should return the object:
  ;
  ( test-equal tmp obj )

  ; ensure obj is now unembedded:
  ;
  (test-assert (not (object-embedded? obj)))

  ; ensure the page modification flag is set:
  ;
  ( test-assert (page-dirty? page) )


  ( close-page! page )

) ; let
) ; test-embedded()


(test-begin "object-embedded")
( let
  (
  ( obj #f )
  )

  ( set! obj ( mk-component ) )
  ( test-embedded obj )

  ( set! obj ( mk-picture ) )
  ( set-object-embedded! obj #f ) ; make-picture/vector() creates embedded object
  ( test-embedded obj )
  )

(test-end "object-embedded")


(test-begin "object-embedded-wrong-argument")

(test-assert-thrown 'wrong-type-arg (object-embedded? 'a))
(test-assert-thrown 'wrong-type-arg (set-object-embedded! 'a #t))
(test-assert-thrown 'wrong-type-arg (set-object-embedded! 'a #f))

(test-end "object-embedded-wrong-argument")
