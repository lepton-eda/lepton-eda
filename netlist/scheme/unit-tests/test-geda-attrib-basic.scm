;;; Test Scheme procedures related to attributes.

(use-modules ((geda attrib) #:renamer (symbol-prefix-proc 'geda:))
             (lepton object)
             (lepton page))

(test-begin "geda:parse-attrib")

(let ((good (make-text '(1 . 2) 'lower-left 0 "name=value" 10 #t 'both))
      (bad (make-text '(1 . 2) 'lower-left 0 "name value" 10 #t 'both)))

  (test-equal #t (geda:attribute? good))
  (test-equal #f (geda:attribute? bad))

  (test-equal "name" (geda:attrib-name good))
  (test-equal "value" (geda:attrib-value good))
  (test-equal (cons (geda:attrib-name good) (geda:attrib-value good))
    (geda:parse-attrib good))

  (test-assert-thrown 'attribute-format (geda:parse-attrib bad))
  (test-assert-thrown 'attribute-format (geda:attrib-name bad))
  (test-assert-thrown 'attribute-format (geda:attrib-value bad)) )

(test-end "geda:parse-attrib")


(test-begin "geda:attach-attrib")

(let ((C (make-component "testcomponent1" '(0 . 0) 0 #f #f))
      (D (make-component "testcomponent2" '(0 . 0) 0 #f #f))
      (p (make-net-pin '(0 . 0) '(100 . 0)))
      (q (make-net-pin '(0 . 0) '(100 . 0)))
      (x (make-text '(0 . 0) 'lower-left 0 "name=x" 10 #t 'both))
      (y (make-text '(0 . 0) 'lower-left 0 "name=y" 10 #t 'both))
      (z (make-text '(0 . 0) 'lower-left 0 "name=z" 10 #t 'both)))

  ;; Attach attribute outside component or page
  (test-assert-thrown 'object-state (geda:attach-attribs! C x))
  (test-equal '() (geda:object-attribs C))
  (test-assert (not (geda:attrib-attachment x)))

  ;; Populate components
  (component-append! C p q x y)
  (component-append! D z)

  ;; Attach attribute to object in same component
  (test-equal p (geda:attach-attribs! p x))
  (test-equal (list x) (geda:object-attribs p))
  (test-equal p (geda:attrib-attachment x))

  ;; Attach attribute twice
  (test-equal p (geda:attach-attribs! p x))
  (test-equal (list x) (geda:object-attribs p))
  (test-equal p (geda:attrib-attachment x))

  ;; Attach attribute which is already attached, within same
  ;; component
  (test-assert-thrown 'object-state (geda:attach-attribs! q x))

  ;; Attach attribute to attached attribute, within same component
  (test-assert-thrown 'object-state (geda:attach-attribs! x y))

  ;; Attach attribute to object in different component
  (test-assert-thrown 'object-state (geda:attach-attribs! p z))
  (test-equal (list x) (geda:object-attribs p))
  (test-assert (not (geda:attrib-attachment z)))

  ;; Attach internal attribute to containing component
  (test-assert-thrown 'object-state (geda:attach-attribs! D z))
  (test-equal '() (geda:object-attribs D))
  (test-assert (not (geda:attrib-attachment z)))

  ;; Attach attribute in component to floating object
  (test-assert-thrown 'object-state (geda:attach-attribs! C z))
  (test-equal '() (geda:object-attribs C))
  (test-assert (not (geda:attrib-attachment z)))

  ;; Attach floating attribute to object in component
  (component-remove! D z)
  (test-assert-thrown 'object-state (geda:attach-attribs! p z))
  (test-equal (list x) (geda:object-attribs p))
  (test-assert (not (geda:attrib-attachment z)))

  ;; Attach multiple attributes
  (test-equal p (geda:attach-attribs! p y))
  (test-equal (list x y) (geda:object-attribs p))
  (test-equal p (geda:attrib-attachment y))
  )

(test-end "geda:attach-attrib")


(test-begin "geda:attach-attrib/page")

(let ((P (make-page "/test/page/A"))
      (Q (make-page "/test/page/A"))
      (p (make-net-pin '(0 . 0) '(100 . 0)))
      (x (make-text '(1 . 2) 'lower-left 0 "name=value" 10 #t 'both))
      (y (make-text '(1 . 2) 'lower-left 0 "name=value" 10 #t 'both))
      (z (make-text '(1 . 2) 'lower-left 0 "name=value" 10 #t 'both))
      (C (make-component "test component" '(1 . 2) 0 #t #f)))

  (dynamic-wind                   ; Make sure pages are cleaned up
    (lambda () #f)
    (lambda ()
                                        ; Populate pages
      (page-append! P x C)
      (component-append! C p y)

      (page-append! Q z)

                                        ; Attach attribute to component in same page
      (geda:attach-attribs! C x)
      (test-equal (list x) (geda:object-attribs C))
      (test-equal C (geda:attrib-attachment x))

                                        ; Remove stuff from page
      (test-assert-thrown 'object-state (page-remove! P x))
      (test-assert-thrown 'object-state (page-remove! P C))

                                        ; Attach attribute to component in different page
      (test-assert-thrown 'object-state (geda:attach-attribs! C z))

                                        ; Attach attribute to pin in component in page
      (geda:attach-attribs! p y)
      (test-equal (list y) (geda:object-attribs p))
      (test-equal p (geda:attrib-attachment y))

                                        ; Remove stuff from component in page
      (test-assert-thrown 'object-state (component-remove! C p))
      (test-assert-thrown 'object-state (component-remove! C y)) )
    (lambda ()
      (close-page! P)
      (close-page! Q) ))

  )

(test-end "geda:attach-attrib/page")


(test-begin "geda:detach-attrib")

(let ((page (make-page "/test/page/1"))
      (pin1 (make-net-pin '(0 . 0) '(100 . 0)))
      (pin2 (make-net-pin '(0 . 100) '(100 . 100)))
      (x (make-text '(0 . 0) 'lower-left 0 "name=x" 10 #t 'both)))

  (page-append! page pin1 pin2 x)

  ;; Detach when already detached
  (test-equal pin1 (geda:detach-attribs! pin1 x))

  (geda:attach-attribs! pin1 x)

  (test-assert-thrown 'object-state
                      (geda:detach-attribs! pin2 x))

  (test-equal pin1 (geda:detach-attribs! pin1 x))
  (test-equal '() (geda:object-attribs pin1)) )

(test-end "geda:detach-attrib")


(test-begin "geda:inherited-attribs")

(let ((C (make-component "test component" '(1 . 2) 0 #t #f))
      (p (make-net-pin '(0 . 0) '(100 . 0)))
      (x (make-text '(1 . 2) 'lower-left 0 "name=x" 10 #t 'both))
      (y (make-text '(1 . 2) 'lower-left 0 "name=y" 10 #t 'both)))

  (test-equal '() (geda:inherited-attribs p))
  (test-equal '() (geda:inherited-attribs C))

  ;; Set up component
  (component-append! C p x y)

  (test-equal (list x y) (geda:inherited-attribs C))

  (geda:attach-attribs! p x)

  (test-equal (list y) (geda:inherited-attribs C)))

(test-end "geda:inherited-attribs")


(test-begin "geda:attrib-inherited?")

(let* ((P (make-page "/test/page/1"))
       (A (make-component "test component" '(0 . 0) 0 #t #f))
       (p (make-net-pin '(0 . 0) '(100 . 0)))
       (w (make-text '(1 . 2) 'lower-left 0 "name=x" 10 #t 'both))
       (x (make-text '(1 . 2) 'lower-left 0 "name=x" 10 #t 'both))
       (y (make-text '(1 . 2) 'lower-left 0 "name=y" 10 #t 'both))
       (z (make-text '(1 . 2) 'lower-left 0 "name=z" 10 #t 'both)))

  (page-append! P A w x)
  (geda:attach-attribs! A x)
  (component-append! A p y z)
  (geda:attach-attribs! p y)

  (test-assert (not (geda:attrib-inherited? w)))
  (test-assert (not (geda:attrib-inherited? x)))
  (test-assert (not (geda:attrib-inherited? y)))
  (test-assert (geda:attrib-inherited? z)))

(test-end "geda:attrib-inherited?")


(test-begin "geda:set-attrib-value!")

(let ((a (make-text '(1 . 2) 'lower-left 0 "name=value" 10 #t 'both)))
  (geda:set-attrib-value! a "foo")
  (test-equal "name" (geda:attrib-name a))
  (test-equal "foo" (geda:attrib-value a)))

(test-end "geda:set-attrib-value!")
