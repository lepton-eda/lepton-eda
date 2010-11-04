;; Test Scheme procedures related to component objects.

(use-modules (unit-test))
(use-modules (geda object))
(use-modules (geda page))
(use-modules (geda attrib))

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

    (assert-equal x (component-append! A x))
    (assert-equal (list x) (component-contents A))

    (assert-equal x (component-append! A x))
    (assert-equal (list x) (component-contents A))

    (assert-equal y (component-append! A y))
    (assert-equal (list x y) (component-contents A))

    (assert-thrown 'object-state
                   (component-append! B x))

    (assert-thrown 'object-state
      (let* ((P (make-page "/test/page/A"))
             (z (page-append! P (make-line '(1 . 0) '(2 . 2)))))
        (component-append! A z)))))

(begin-test 'component-remove
  (let ((A (make-component "test component" '(1 . 2) 0 #t #f))
        (B (make-component "test component" '(1 . 2) 0 #t #f))
        (P (make-page "/test/page/A"))
        (x (make-line '(0 . 0) '(2 . 0)))
        (y (make-line '(0 . 0) '(0 . 2)))
        (z (make-line '(1 . 0) '(2 . 2))))

    (component-append! A x)
    (assert-equal x (component-remove! A x))
    (assert-equal '() (component-contents A))
    (assert-equal x (component-remove! A x))
    (assert-equal x (component-remove! B x))

    (component-append! A x)
    (component-append! A y)
    (assert-equal x (component-remove! A x))
    (assert-equal (list y) (component-contents A))

    (assert-thrown 'object-state
                   (component-remove! B y))

    (page-append! P z)
    (assert-thrown 'object-state
        (component-remove! A z))
    ))

(begin-test 'component-remove-attrib
  (let ((comp (make-component "test component" '(1 . 2) 0 #t #f))
        (pin (make-net-pin '(0 . 0) '(100 . 0)))
        (attrib (make-text '(0 . 0) 'lower-left 0 "name=x" 10 #t 'both)))
    (for-each (lambda (x) (component-append! comp x)) (list pin attrib))
    (attach-attrib! pin attrib)
    (assert-thrown 'object-state (component-remove! comp pin))
    (assert-thrown 'object-state (component-remove! comp attrib))))
