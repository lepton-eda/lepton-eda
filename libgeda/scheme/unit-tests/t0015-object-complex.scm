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
    (attach-attrib! pin attrib)
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
