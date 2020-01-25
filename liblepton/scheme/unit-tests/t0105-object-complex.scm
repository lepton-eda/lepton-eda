;; Test Scheme procedures related to component objects.

(use-modules (unit-test)
             (geda attrib)
             (lepton library)
             (lepton object)
             ((geda object) #:renamer (symbol-prefix-proc 'geda:))
             (lepton page))

(begin-test 'component
  (let ((a (make-component "test component" '(1 . 2) 0 #t #f)))

    (assert-equal 'complex (object-type a))

    (assert-true (component? a))

    (assert-equal "test component" (component-basename a))
    (assert-equal '(1 . 2) (component-position a))
    (assert-equal 0 (component-angle a))
    (assert-true (component-mirror? a))
    (assert-true (not (component-locked? a)))

    (assert-equal (list (component-basename a) (component-position a)
                        (component-angle a) (component-mirror? a)
                        (component-locked? a))
                  (component-info a))

    (set-component! a '(3 . 4) 90 #f #t)

    (assert-equal '(3 . 4) (component-position a))
    (assert-equal 90 (component-angle a))
    (assert-true (not (component-mirror? a)))
    (assert-true (component-locked? a))

    (assert-thrown 'misc-error
                   (set-component! a '(3 . 4) 45 #f #t))))

(begin-test 'component-append
  (let ((A (make-component "test component" '(1 . 2) 0 #t #f))
        (B (make-component "test component" '(1 . 2) 0 #t #f))
        (x (make-line '(0 . 0) '(2 . 0)))
        (y (make-line '(0 . 0) '(0 . 2))))

    (assert-equal '() (component-contents A))

    (assert-equal A (component-append! A x))
    (assert-equal (list x) (component-contents A))

    (component-append! A x)
    (assert-equal (list x) (component-contents A))

    (component-append! A y)
    (assert-equal (list x y) (component-contents A))

    (assert-thrown 'object-state
                   (component-append! B x))))

(begin-test 'component-remove
  (let ((A (make-component "test component" '(1 . 2) 0 #t #f))
        (B (make-component "test component" '(1 . 2) 0 #t #f))
        (x (make-line '(0 . 0) '(2 . 0)))
        (y (make-line '(0 . 0) '(0 . 2)))
        (z (make-line '(1 . 0) '(2 . 2))))

    (component-append! A x)
    (assert-equal A (component-remove! A x))
    (assert-equal '() (component-contents A))
    (component-remove! A x)
    (component-remove! B x)

    (component-append! A x y)
    (component-remove! A x y)
    (assert-equal '() (component-contents A))

    (component-append! A x y)
    (component-remove! A x)
    (assert-equal (list y) (component-contents A))

    (assert-thrown 'object-state
                   (component-remove! B y))))

(begin-test 'component-append/page
  (let ((P (make-page "/test/page/A"))
        (A (make-component "test component" '(1 . 2) 0 #t #f))
        (x (make-line '(0 . 0) '(2 . 0)))
        (y (make-line '(0 . 0) '(0 . 2))))
    (dynamic-wind
     (lambda () #t)
     (lambda ()
       (page-append! P x)
       (assert-thrown 'object-state
                      (component-append! A x))

       (page-append! P A)
       (assert-thrown 'object-state
                      (component-append! A x))

       (component-append! A y)
       (assert-equal (list y) (component-contents A)))

     (lambda ()
       (close-page! P)))
    ))

(begin-test 'component-remove/page
  (let ((P (make-page "/test/page/A"))
        (A (make-component "test component" '(1 . 2) 0 #t #f))
        (x (make-line '(0 . 0) '(2 . 0)))
        (y (make-line '(0 . 0) '(0 . 2))))
    (dynamic-wind
     (lambda () #t)
     (lambda ()
       ;; Test that if a primitive object is attached directly to
       ;; a page, attempting to remove it from a component
       ;; doesn't work.
       (page-append! P x)
       (assert-thrown 'object-state
                      (component-remove! A x))

       (page-append! P A)
       (assert-thrown 'object-state
                      (component-remove! A x))

       ;; Test that you can remove primitive objects from a
       ;; component that is attached to a page.
       (component-append! A y)
       (component-remove! A y)
       (assert-equal '() (component-contents A)))

     (lambda ()
       (close-page! P)))
    ))

(begin-test 'component-translate
  (let* ((A (make-component "test component" '(0 . 0) 0 #t #f))
         (x (make-box '(0 . 2) '(2 . 0))))

    (component-append! A x)
    (set-component! A '(1 . 1) 0 #t #f)
    (assert-equal '(1 . 3) (box-top-left x))
    (assert-equal '(3 . 1) (box-bottom-right x))))

(begin-test 'component-remove-attrib
  (let ((comp (make-component "test component" '(1 . 2) 0 #t #f))
        (pin (make-net-pin '(0 . 0) '(100 . 0)))
        (attrib (make-text '(0 . 0) 'lower-left 0 "name=x" 10 #t 'both)))
    (component-append! comp pin attrib)
    (attach-attribs! pin attrib)
    (assert-thrown 'object-state (component-remove! comp pin))
    (assert-thrown 'object-state (component-remove! comp attrib))))


;; Set up component library, making blatant assumptions about the
;; directory layout.
(component-library (string-join (list (getenv "srcdir") "../../symbols/analog") "/")
                   "Basic devices")

(begin-test 'component/library
  (let ((A (make-component/library "resistor-1.sym" '(1 . 2) 0 #t #f))
        (B (make-component/library "invalid-component-name" '(1 . 2) 0 #t #f)))

    (assert-true A)
    (assert-equal '(1 . 2) (component-position A))
    (assert-equal 0 (component-angle A))
    (assert-true (component-mirror? A))
    (assert-true (not (component-locked? A)))

    (assert-equal "resistor-1.sym" (component-basename A))

    (assert-true (not (null? (component-contents A))))

    (assert-true (not B))))

;; Clear component library again
(reset-component-library)

(begin-test 'object-component
  (let* ((A (make-component "test component" '(0 . 0) 0 #t #f))
         (x (make-box '(0 . 2) '(2 . 0))))
    (assert-equal #f (object-component x))
    (component-append! A x)
    (assert-equal A (object-component x))))

;; New symbol library with one component containing only one
;; line primitive
(component-library-funcs
  (lambda ()       ; list-symbol-names function
    '("line.sym"))
  (lambda (name)   ; get-symbol-by-name function
    (let ((page (make-page "/test/page/line")))
      (page-append! page (make-line '(1 . 2) '(3 . 4)))
      (let ((s (page->string page)))
        (close-page! page)
        s)))
  "Test symbols"   ; Library name
  )

;; Test the 'set-component-with-transform!' procedure. This test
;; includes testing of mirroring and rotation of a component's
;; primitives.

(begin-test 'set-component-with-transform
  (let ((P (make-page "/test/page/A"))
        (C (make-component/library "line.sym" '(0 . 0) 90 #f #f)))

    (dynamic-wind
      (lambda () #t)
      (lambda ()
        (page-append! P C)

        (set-component-with-transform! C '(0 . 0) 90 #t #f)

        (assert-equal '(-2 . -1) (line-start (car (component-contents C))))
        (assert-equal '(-4 . -3) (line-end   (car (component-contents C))))
        )
      (lambda ()
        (close-page! P)))
    ))

;; Clear component library again
(reset-component-library)



( begin-test 'component-filename
( let*
  (
  ( fname1  ( format #f "~a.sym" (tmpnam) ) )
  ( symname ( basename fname1 ) )
  ( symdir  ( dirname  fname1 ) )
  ( comp1   #f )
  ( comp2   #f )
  )

  ( define ( mk-comp1 )
    ( with-output-to-file fname1
      ( lambda()
        ( format #t "v 20191003 2~%" )
        ( format #t "B 0 0 500 500 3 10 1 0 -1 -1 0 -1 -1 -1 -1 -1~%" )
        ( format #t "T 0 600 21 6 1 0 0 0 1~%" )
        ( format #t "refdes=R?" )
      )
    )

    ( component-library symdir )

    ; return:
    ( make-component/library
      symname                ; basename
      ( cons 0 0 )           ; position
      0                      ; angle
      #f                     ; mirror
      #f                     ; locked
    )
  )

  ( define ( mk-comp2 )
    ; return:
    ( make-component
      "does-not-exist"       ; basename
      ( cons 0 0 )           ; position
      0                      ; angle
      #f                     ; mirror
      #f                     ; locked
    )
  )


  ( format #t "cwd:     [~a]~%" (getcwd) ) ; [debug]
  ( format #t "symdir:  [~a]~%" symdir )   ; [debug]
  ( format #t "symname: [~a]~%" symname )  ; [debug]
  ( format #t "fname1:  [~a]~%" fname1 )   ; [debug]

  ( set! comp1 ( mk-comp1 ) )
  ( set! comp2 ( mk-comp2 ) )

  ( assert-equal (component-filename comp1) fname1 )
  ( assert-false (component-filename comp2) )

  ( reset-component-library )

) ; let
) ; 'component-filename()

;;; The same tests for the deprecated (geda object) module
;;; functions.

(begin-test 'geda:component
  (let ((a (geda:make-component "test component" '(1 . 2) 0 #t #f)))

    (assert-equal 'complex (geda:object-type a))

    (assert-true (geda:component? a))

    (assert-equal "test component" (geda:component-basename a))
    (assert-equal '(1 . 2) (geda:component-position a))
    (assert-equal 0 (geda:component-angle a))
    (assert-true (geda:component-mirror? a))
    (assert-true (not (geda:component-locked? a)))

    (assert-equal (list (geda:component-basename a) (geda:component-position a)
                        (geda:component-angle a) (geda:component-mirror? a)
                        (geda:component-locked? a))
                  (geda:component-info a))

    (geda:set-component! a '(3 . 4) 90 #f #t)

    (assert-equal '(3 . 4) (geda:component-position a))
    (assert-equal 90 (geda:component-angle a))
    (assert-true (not (geda:component-mirror? a)))
    (assert-true (geda:component-locked? a))

    (assert-thrown 'misc-error
                   (geda:set-component! a '(3 . 4) 45 #f #t))))

(begin-test 'geda:component-append
  (let ((A (geda:make-component "test component" '(1 . 2) 0 #t #f))
        (B (geda:make-component "test component" '(1 . 2) 0 #t #f))
        (x (geda:make-line '(0 . 0) '(2 . 0)))
        (y (geda:make-line '(0 . 0) '(0 . 2))))

    (assert-equal '() (geda:component-contents A))

    (assert-equal A (geda:component-append! A x))
    (assert-equal (list x) (geda:component-contents A))

    (geda:component-append! A x)
    (assert-equal (list x) (geda:component-contents A))

    (geda:component-append! A y)
    (assert-equal (list x y) (geda:component-contents A))

    (assert-thrown 'object-state
                   (geda:component-append! B x))))

(begin-test 'geda:component-remove
  (let ((A (geda:make-component "test component" '(1 . 2) 0 #t #f))
        (B (geda:make-component "test component" '(1 . 2) 0 #t #f))
        (x (geda:make-line '(0 . 0) '(2 . 0)))
        (y (geda:make-line '(0 . 0) '(0 . 2)))
        (z (geda:make-line '(1 . 0) '(2 . 2))))

    (geda:component-append! A x)
    (assert-equal A (geda:component-remove! A x))
    (assert-equal '() (geda:component-contents A))
    (geda:component-remove! A x)
    (geda:component-remove! B x)

    (geda:component-append! A x y)
    (geda:component-remove! A x y)
    (assert-equal '() (geda:component-contents A))

    (geda:component-append! A x y)
    (geda:component-remove! A x)
    (assert-equal (list y) (geda:component-contents A))

    (assert-thrown 'object-state
                   (geda:component-remove! B y))))

(begin-test 'geda:component-append/page
  (let ((P (make-page "/test/page/A"))
        (A (geda:make-component "test component" '(1 . 2) 0 #t #f))
        (x (geda:make-line '(0 . 0) '(2 . 0)))
        (y (geda:make-line '(0 . 0) '(0 . 2))))
    (dynamic-wind
     (lambda () #t)
     (lambda ()
       (page-append! P x)
       (assert-thrown 'object-state
                      (geda:component-append! A x))

       (page-append! P A)
       (assert-thrown 'object-state
                      (geda:component-append! A x))

       (geda:component-append! A y)
       (assert-equal (list y) (geda:component-contents A)))

     (lambda ()
       (close-page! P)))
    ))

(begin-test 'geda:component-remove/page
  (let ((P (make-page "/test/page/A"))
        (A (geda:make-component "test component" '(1 . 2) 0 #t #f))
        (x (geda:make-line '(0 . 0) '(2 . 0)))
        (y (geda:make-line '(0 . 0) '(0 . 2))))
    (dynamic-wind
     (lambda () #t)
     (lambda ()
       ;; Test that if a primitive object is attached directly to
       ;; a page, attempting to remove it from a component
       ;; doesn't work.
       (page-append! P x)
       (assert-thrown 'object-state
                      (geda:component-remove! A x))

       (page-append! P A)
       (assert-thrown 'object-state
                      (geda:component-remove! A x))

       ;; Test that you can remove primitive objects from a
       ;; component that is attached to a page.
       (geda:component-append! A y)
       (geda:component-remove! A y)
       (assert-equal '() (geda:component-contents A)))

     (lambda ()
       (close-page! P)))
    ))

(begin-test 'geda:component-translate
  (let* ((A (geda:make-component "test component" '(0 . 0) 0 #t #f))
         (x (geda:make-box '(0 . 2) '(2 . 0))))

    (geda:component-append! A x)
    (geda:set-component! A '(1 . 1) 0 #t #f)
    (assert-equal '(1 . 3) (geda:box-top-left x))
    (assert-equal '(3 . 1) (geda:box-bottom-right x))))

(begin-test 'geda:component-remove-attrib
  (let ((comp (geda:make-component "test component" '(1 . 2) 0 #t #f))
        (pin (geda:make-net-pin '(0 . 0) '(100 . 0)))
        (attrib (geda:make-text '(0 . 0) 'lower-left 0 "name=x" 10 #t 'both)))
    (geda:component-append! comp pin attrib)
    (attach-attribs! pin attrib)
    (assert-thrown 'object-state (geda:component-remove! comp pin))
    (assert-thrown 'object-state (geda:component-remove! comp attrib))))


;; Set up component library, making blatant assumptions about the
;; directory layout.
(component-library (string-join (list (getenv "srcdir") "../../symbols/analog") "/")
                   "Basic devices")

(begin-test 'geda:component/library
  (let ((A (geda:make-component/library "resistor-1.sym" '(1 . 2) 0 #t #f))
        (B (geda:make-component/library "invalid-component-name" '(1 . 2) 0 #t #f)))

    (assert-true A)
    (assert-equal '(1 . 2) (geda:component-position A))
    (assert-equal 0 (geda:component-angle A))
    (assert-true (geda:component-mirror? A))
    (assert-true (not (geda:component-locked? A)))

    (assert-equal "resistor-1.sym" (geda:component-basename A))

    (assert-true (not (null? (geda:component-contents A))))

    (assert-true (not B))))

;; Clear component library again
(reset-component-library)

(begin-test 'geda:object-component
  (let* ((A (geda:make-component "test component" '(0 . 0) 0 #t #f))
         (x (geda:make-box '(0 . 2) '(2 . 0))))
    (assert-equal #f (geda:object-component x))
    (geda:component-append! A x)
    (assert-equal A (geda:object-component x))))

;; New symbol library with one component containing only one
;; line primitive
(component-library-funcs
  (lambda ()       ; list-symbol-names function
    '("line.sym"))
  (lambda (name)   ; get-symbol-by-name function
    (let ((page (make-page "/test/page/line")))
      (page-append! page (geda:make-line '(1 . 2) '(3 . 4)))
      (let ((s (page->string page)))
        (close-page! page)
        s)))
  "Test symbols"   ; Library name
  )

;; Test the 'geda:set-component-with-transform!' procedure. This test
;; includes testing of mirroring and rotation of a component's
;; primitives.

(begin-test 'geda:set-component-with-transform
  (let ((P (make-page "/test/page/A"))
        (C (geda:make-component/library "line.sym" '(0 . 0) 90 #f #f)))

    (dynamic-wind
      (lambda () #t)
      (lambda ()
        (page-append! P C)

        (geda:set-component-with-transform! C '(0 . 0) 90 #t #f)

        (assert-equal '(-2 . -1) (geda:line-start (car (geda:component-contents C))))
        (assert-equal '(-4 . -3) (geda:line-end   (car (geda:component-contents C))))
        )
      (lambda ()
        (close-page! P)))
    ))

;; Clear component library again
(reset-component-library)



( begin-test 'geda:component-filename
( let*
  (
  ( fname1  ( format #f "~a.sym" (tmpnam) ) )
  ( symname ( basename fname1 ) )
  ( symdir  ( dirname  fname1 ) )
  ( comp1   #f )
  ( comp2   #f )
  )

  ( define ( mk-comp1 )
    ( with-output-to-file fname1
      ( lambda()
        ( format #t "v 20191003 2~%" )
        ( format #t "B 0 0 500 500 3 10 1 0 -1 -1 0 -1 -1 -1 -1 -1~%" )
        ( format #t "T 0 600 21 6 1 0 0 0 1~%" )
        ( format #t "refdes=R?" )
      )
    )

    ( component-library symdir )

    ; return:
    ( geda:make-component/library
      symname                ; basename
      ( cons 0 0 )           ; position
      0                      ; angle
      #f                     ; mirror
      #f                     ; locked
    )
  )

  ( define ( mk-comp2 )
    ; return:
    ( geda:make-component
      "does-not-exist"       ; basename
      ( cons 0 0 )           ; position
      0                      ; angle
      #f                     ; mirror
      #f                     ; locked
    )
  )


  ( format #t "cwd:     [~a]~%" (getcwd) ) ; [debug]
  ( format #t "symdir:  [~a]~%" symdir )   ; [debug]
  ( format #t "symname: [~a]~%" symname )  ; [debug]
  ( format #t "fname1:  [~a]~%" fname1 )   ; [debug]

  ( set! comp1 ( mk-comp1 ) )
  ( set! comp2 ( mk-comp2 ) )

  ( assert-equal (geda:component-filename comp1) fname1 )
  ( assert-false (geda:component-filename comp2) )

  ( reset-component-library )

) ; let
) ; 'geda:component-filename()
