;;; Test Scheme procedures related to component objects.

(use-modules (lepton attrib)
             (lepton library)
             (lepton page)
             ((geda object) #:renamer (symbol-prefix-proc 'geda:)))

(test-begin "geda:component" 13)

(let ((a (geda:make-component "test component" '(1 . 2) 0 #t #f)))

  (test-equal 'complex (geda:object-type a))

  (test-assert (geda:component? a))

  (test-equal "test component" (geda:component-basename a))
  (test-equal '(1 . 2) (geda:component-position a))
  (test-equal 0 (geda:component-angle a))
  (test-assert (geda:component-mirror? a))
  (test-assert (not (geda:component-locked? a)))

  (test-equal (list (geda:component-basename a) (geda:component-position a)
                    (geda:component-angle a) (geda:component-mirror? a)
                    (geda:component-locked? a))
    (geda:component-info a))

  (geda:set-component! a '(3 . 4) 90 #f #t)

  (test-equal '(3 . 4) (geda:component-position a))
  (test-equal 90 (geda:component-angle a))
  (test-assert (not (geda:component-mirror? a)))
  (test-assert (geda:component-locked? a))

  (test-assert-thrown 'misc-error
                      (geda:set-component! a '(3 . 4) 45 #f #t)))

(test-end "geda:component")


(test-begin "geda:component-append" 6)

(let ((A (geda:make-component "test component" '(1 . 2) 0 #t #f))
      (B (geda:make-component "test component" '(1 . 2) 0 #t #f))
      (x (geda:make-line '(0 . 0) '(2 . 0)))
      (y (geda:make-line '(0 . 0) '(0 . 2))))

  (test-equal '() (geda:component-contents A))

  (test-equal A (geda:component-append! A x))
  (test-equal (list x) (geda:component-contents A))

  (geda:component-append! A x)
  (test-equal (list x) (geda:component-contents A))

  (geda:component-append! A y)
  (test-equal (list x y) (geda:component-contents A))

  (test-assert-thrown 'object-state
                      (geda:component-append! B x)))

(test-end "geda:component-append")


(test-begin "geda:component-remove" 5)

(let ((A (geda:make-component "test component" '(1 . 2) 0 #t #f))
      (B (geda:make-component "test component" '(1 . 2) 0 #t #f))
      (x (geda:make-line '(0 . 0) '(2 . 0)))
      (y (geda:make-line '(0 . 0) '(0 . 2)))
      (z (geda:make-line '(1 . 0) '(2 . 2))))

  (geda:component-append! A x)
  (test-equal A (geda:component-remove! A x))
  (test-equal '() (geda:component-contents A))
  (geda:component-remove! A x)
  (geda:component-remove! B x)

  (geda:component-append! A x y)
  (geda:component-remove! A x y)
  (test-equal '() (geda:component-contents A))

  (geda:component-append! A x y)
  (geda:component-remove! A x)
  (test-equal (list y) (geda:component-contents A))

  (test-assert-thrown 'object-state
                      (geda:component-remove! B y)))

(test-end "geda:component-remove")


(test-begin "geda:component-append/page" 3)

(let ((P (make-page "/test/page/A"))
      (A (geda:make-component "test component" '(1 . 2) 0 #t #f))
      (x (geda:make-line '(0 . 0) '(2 . 0)))
      (y (geda:make-line '(0 . 0) '(0 . 2))))
  (dynamic-wind
    (lambda () #t)
    (lambda ()
      (page-append! P x)
      (test-assert-thrown 'object-state
                          (geda:component-append! A x))

      (page-append! P A)
      (test-assert-thrown 'object-state
                          (geda:component-append! A x))

      (geda:component-append! A y)
      (test-equal (list y) (geda:component-contents A)))

    (lambda () (close-page! P))))

(test-end "geda:component-append/page")


(test-begin "geda:component-remove/page" 3)

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
      (test-assert-thrown 'object-state
                          (geda:component-remove! A x))

      (page-append! P A)
      (test-assert-thrown 'object-state
                          (geda:component-remove! A x))

      ;; Test that you can remove primitive objects from a
      ;; component that is attached to a page.
      (geda:component-append! A y)
      (geda:component-remove! A y)
      (test-equal '() (geda:component-contents A)))

    (lambda () (close-page! P))))

(test-end "geda:component-remove/page")


(test-begin "geda:component-translate" 2)

(let* ((A (geda:make-component "test component" '(0 . 0) 0 #t #f))
       (x (geda:make-box '(0 . 2) '(2 . 0))))

  (geda:component-append! A x)
  (geda:set-component! A '(1 . 1) 0 #t #f)
  (test-equal '(1 . 3) (geda:box-top-left x))
  (test-equal '(3 . 1) (geda:box-bottom-right x)))

(test-end "geda:component-translate")


(test-begin "geda:component-remove-attrib" 2)

(let ((comp (geda:make-component "test component" '(1 . 2) 0 #t #f))
      (pin (geda:make-net-pin '(0 . 0) '(100 . 0)))
      (attrib (geda:make-text '(0 . 0) 'lower-left 0 "name=x" 10 #t 'both)))
  (geda:component-append! comp pin attrib)
  (attach-attribs! pin attrib)
  (test-assert-thrown 'object-state (geda:component-remove! comp pin))
  (test-assert-thrown 'object-state (geda:component-remove! comp attrib)))

(test-end "geda:component-remove-attrib")


;; Set up component library, making blatant assumptions about the
;; directory layout.
(component-library (string-join (list (getenv "srcdir") "../../lepton-eda/sym/analog") "/")
                   "Basic devices")

(test-begin "geda:component/library" 8)

(let ((A (geda:make-component/library "resistor-1.sym" '(1 . 2) 0 #t #f))
      (B (geda:make-component/library "invalid-component-name" '(1 . 2) 0 #t #f)))

  (test-assert A)
  (test-equal '(1 . 2) (geda:component-position A))
  (test-equal 0 (geda:component-angle A))
  (test-assert (geda:component-mirror? A))
  (test-assert (not (geda:component-locked? A)))

  (test-equal "resistor-1.sym" (geda:component-basename A))

  (test-assert (not (null? (geda:component-contents A))))

  (test-assert (not B)))

(test-end "geda:component/library")

;; Clear component library again
(reset-component-library)


(test-begin "geda:object-component" 2)

(let* ((A (geda:make-component "test component" '(0 . 0) 0 #t #f))
       (x (geda:make-box '(0 . 2) '(2 . 0))))
  (test-equal #f (geda:object-component x))
  (geda:component-append! A x)
  (test-equal A (geda:object-component x)))

(test-end "geda:object-component")


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

(test-begin "geda:set-component-with-transform" 2)

(let ((P (make-page "/test/page/A"))
      (C (geda:make-component/library "line.sym" '(0 . 0) 90 #f #f)))

  (dynamic-wind
    (lambda () #t)
    (lambda ()
      (page-append! P C)

      (geda:set-component-with-transform! C '(0 . 0) 90 #t #f)

      (test-equal '(-2 . -1) (geda:line-start (car (geda:component-contents C))))
      (test-equal '(-4 . -3) (geda:line-end   (car (geda:component-contents C))))
      )
    (lambda () (close-page! P))))

(test-end "geda:set-component-with-transform")


;; Clear component library again
(reset-component-library)

;;; Test component file names.
(test-begin "geda:component-filename")
(let*
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
      symname                           ; basename
      ( cons 0 0 )                      ; position
      0                                 ; angle
      #f                                ; mirror
      #f                                ; locked
      )
    )

  ( define ( mk-comp2 )
                                        ; return:
    ( geda:make-component
      "does-not-exist"                  ; basename
      ( cons 0 0 )                      ; position
      0                                 ; angle
      #f                                ; mirror
      #f                                ; locked
      )
    )

  #|
  ( format #t "cwd:     [~a]~%" (getcwd) ) ; [debug]
  ( format #t "symdir:  [~a]~%" symdir )   ; [debug]
  ( format #t "symname: [~a]~%" symname )  ; [debug]
  ( format #t "fname1:  [~a]~%" fname1 )   ; [debug]
  |#

  ( set! comp1 ( mk-comp1 ) )
  ( set! comp2 ( mk-comp2 ) )

  (test-group-with-cleanup "geda:component-filename-grp"

    (test-equal (geda:component-filename comp1) fname1)
    (test-assert (not (geda:component-filename comp2)))
    ;; Clean up.
    (begin
      (reset-component-library)
      (delete-file fname1))))
(test-end "geda:component-filename")
