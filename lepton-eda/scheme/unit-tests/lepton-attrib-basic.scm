;;; Test Scheme procedures related to attributes.

(use-modules (lepton attrib)
             (lepton object)
             (lepton page))

(test-begin "parse-attrib")

(let ((good (make-text '(1 . 2) 'lower-left 0 "name=value" 10 #t 'both))
      (bad (make-text '(1 . 2) 'lower-left 0 "name value" 10 #t 'both)))

  (test-equal #t (attribute? good))
  (test-equal #f (attribute? bad))

  (test-equal "name" (attrib-name good))
  (test-equal "value" (attrib-value good))
  (test-equal (cons (attrib-name good) (attrib-value good))
    (parse-attrib good))

  (test-assert-thrown 'attribute-format (parse-attrib bad))
  (test-assert-thrown 'attribute-format (attrib-name bad))
  (test-assert-thrown 'attribute-format (attrib-value bad))

  (test-assert-thrown 'wrong-type-arg (parse-attrib 'x))
  (test-assert-thrown 'wrong-type-arg (attrib-name 'x))
  (test-assert-thrown 'wrong-type-arg (attrib-value 'x)) )

(test-end "parse-attrib")


(test-begin "attach-attrib")

(let ((C (make-component "testcomponent1" '(0 . 0) 0 #f #f))
      (D (make-component "testcomponent2" '(0 . 0) 0 #f #f))
      (p (make-net-pin '(0 . 0) '(100 . 0)))
      (q (make-net-pin '(0 . 0) '(100 . 0)))
      (x (make-text '(0 . 0) 'lower-left 0 "name=x" 10 #t 'both))
      (y (make-text '(0 . 0) 'lower-left 0 "name=y" 10 #t 'both))
      (z (make-text '(0 . 0) 'lower-left 0 "name=z" 10 #t 'both)))

  ;; Attach attribute outside component or page
  (test-assert-thrown 'object-state (attach-attribs! C x))
  (test-equal '() (object-attribs C))
  (test-assert (not (attrib-attachment x)))

  ;; Populate components
  (component-append! C p q x y)
  (component-append! D z)

  ;; Attach attribute to object in same component
  (test-equal p (attach-attribs! p x))
  (test-equal (list x) (object-attribs p))
  (test-equal p (attrib-attachment x))

  ;; Attach attribute twice
  (test-equal p (attach-attribs! p x))
  (test-equal (list x) (object-attribs p))
  (test-equal p (attrib-attachment x))

  ;; Attach attribute which is already attached, within same
  ;; component
  (test-assert-thrown 'object-state (attach-attribs! q x))

  ;; Attach attribute to attached attribute, within same component
  (test-assert-thrown 'object-state (attach-attribs! x y))

  ;; Attach attribute to object in different component
  (test-assert-thrown 'object-state (attach-attribs! p z))
  (test-equal (list x) (object-attribs p))
  (test-assert (not (attrib-attachment z)))

  ;; Attach internal attribute to containing component
  (test-assert-thrown 'object-state (attach-attribs! D z))
  (test-equal '() (object-attribs D))
  (test-assert (not (attrib-attachment z)))

  ;; Attach attribute in component to floating object
  (test-assert-thrown 'object-state (attach-attribs! C z))
  (test-equal '() (object-attribs C))
  (test-assert (not (attrib-attachment z)))

  ;; Attach floating attribute to object in component
  (component-remove! D z)
  (test-assert-thrown 'object-state (attach-attribs! p z))
  (test-equal (list x) (object-attribs p))
  (test-assert (not (attrib-attachment z)))

  ;; Attach multiple attributes
  (test-equal p (attach-attribs! p y))
  (test-equal (list x y) (object-attribs p))
  (test-equal p (attrib-attachment y))

  ;; Wrong type argument.
  (test-assert-thrown 'wrong-type-arg (attrib-attachment 'x))
  (test-assert-thrown 'wrong-type-arg
                      (attrib-attachment (make-text '(1 . 2) 'lower-left 0 "name value" 10 #t 'both)))

  (test-assert-thrown 'wrong-type-arg (object-attribs 'x))
  )

(test-end "attach-attrib")


(test-begin "attach-attrib/page")

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
      (attach-attribs! C x)
      (test-equal (list x) (object-attribs C))
      (test-equal C (attrib-attachment x))

                                        ; Remove stuff from page
      (test-assert-thrown 'object-state (page-remove! P x))
      (test-assert-thrown 'object-state (page-remove! P C))

                                        ; Attach attribute to component in different page
      (test-assert-thrown 'object-state (attach-attribs! C z))

                                        ; Attach attribute to pin in component in page
      (attach-attribs! p y)
      (test-equal (list y) (object-attribs p))
      (test-equal p (attrib-attachment y))

                                        ; Remove stuff from component in page
      (test-assert-thrown 'object-state (component-remove! C p))
      (test-assert-thrown 'object-state (component-remove! C y)) )
    (lambda ()
      (close-page! P)
      (close-page! Q) ))

  )

(test-end "attach-attrib/page")


(test-begin "detach-attrib")

(let ((page (make-page "/test/page/1"))
      (pin1 (make-net-pin '(0 . 0) '(100 . 0)))
      (pin2 (make-net-pin '(0 . 100) '(100 . 100)))
      (x (make-text '(0 . 0) 'lower-left 0 "name=x" 10 #t 'both)))

  (page-append! page pin1 pin2 x)

  ;; Detach when already detached
  (test-equal pin1 (detach-attribs! pin1 x))

  (attach-attribs! pin1 x)

  (test-assert-thrown 'object-state
                      (detach-attribs! pin2 x))

  (test-equal pin1 (detach-attribs! pin1 x))
  (test-equal '() (object-attribs pin1)) )

(test-end "detach-attrib")


(test-begin "inherited-attribs")

(let ((C (make-component "test component" '(1 . 2) 0 #t #f))
      (p (make-net-pin '(0 . 0) '(100 . 0)))
      (x (make-text '(1 . 2) 'lower-left 0 "name=x" 10 #t 'both))
      (y (make-text '(1 . 2) 'lower-left 0 "name=y" 10 #t 'both)))

  (test-equal '() (inherited-attribs p))
  (test-equal '() (inherited-attribs C))

  ;; Set up component
  (component-append! C p x y)

  (test-equal (list x y) (inherited-attribs C))

  (attach-attribs! p x)

  (test-equal (list y) (inherited-attribs C)))

(test-end "inherited-attribs")


(test-begin "attrib-inherited?")

(let* ((P (make-page "/test/page/1"))
       (A (make-component "test component" '(0 . 0) 0 #t #f))
       (p (make-net-pin '(0 . 0) '(100 . 0)))
       (w (make-text '(1 . 2) 'lower-left 0 "name=x" 10 #t 'both))
       (x (make-text '(1 . 2) 'lower-left 0 "name=x" 10 #t 'both))
       (y (make-text '(1 . 2) 'lower-left 0 "name=y" 10 #t 'both))
       (z (make-text '(1 . 2) 'lower-left 0 "name=z" 10 #t 'both)))

  (page-append! P A w x)
  (attach-attribs! A x)
  (component-append! A p y z)
  (attach-attribs! p y)

  (test-assert (not (attrib-inherited? w)))
  (test-assert (not (attrib-inherited? x)))
  (test-assert (not (attrib-inherited? y)))
  (test-assert (attrib-inherited? z)))

(test-end "attrib-inherited?")


(test-begin "set-attrib-value!")

(let ((a (make-text '(1 . 2) 'lower-left 0 "name=value" 10 #t 'both)))
  (set-attrib-value! a "foo")
  (test-equal "name" (attrib-name a))
  (test-equal "foo" (attrib-value a)))

(test-end "set-attrib-value!")
