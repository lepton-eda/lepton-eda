;; Test Scheme procedures related to pages.

(use-modules (unit-test))
(use-modules (geda page))
(use-modules (geda object))
(use-modules (geda attrib))

(begin-test 'page
   (let ((page-a (make-page "/test/page/A"))
         (page-b (make-page "/test/page/B")))
     (assert-equal "/test/page/A" (page-filename page-a))
     (assert-equal (list page-a page-b) (active-pages))

     (assert-equal page-a (set-page-filename! page-a "/test/page/C"))
     (assert-equal "/test/page/C" (page-filename page-a))

     (close-page! page-a)
     (assert-equal (list page-b) (active-pages))
     (close-page! page-b)))

(begin-test 'page-append
  (let ((A (make-page "/test/page/C"))
        (B (make-page "/test/page/D"))
        (x (make-line '(0 . 0) '(1 . 2)))
        (y (make-line '(0 . 1) '(2 . 2))))

    (dynamic-wind ; Make sure pages are cleaned up
        (lambda () #f)
        (lambda ()
          (assert-equal '() (page-contents A))

          (assert-equal x (page-append! A x))
          (assert-equal (list x) (page-contents A))

          (assert-equal x (page-append! A x))
          (assert-equal (list x) (page-contents A))

          (assert-equal y (page-append! A y))
          (assert-equal (list x y) (page-contents A))

          (assert-thrown 'object-state
             (page-append! B x))

          (assert-thrown 'object-state
            (let* ((C (make-component "test component" '(1 . 2) 0 #t #f))
                   (z (component-append! C (make-line '(1 . 0) '(2 . 2)))))
              (page-append! A z))))

        (lambda ()
          (close-page! A)
          (close-page! B)))))

(begin-test 'page-remove
  (let ((A (make-page "/test/page/E"))
        (B (make-page "/test/page/F"))
        (C (make-component "test component" '(1 . 2) 0 #t #f))
        (x (make-line '(0 . 0) '(2 . 0)))
        (y (make-line '(0 . 0) '(0 . 2)))
        (z (make-line '(1 . 0) '(2 . 2))))

    (dynamic-wind ; Make sure pages are cleaned up
        (lambda () #f)
        (lambda ()
          (page-append! A x)
          (assert-equal x (page-remove! A x))
          (assert-equal '() (page-contents A))
          (assert-equal x (page-remove! A x))
          (assert-equal x (page-remove! B x))

          (page-append! A x)
          (page-append! A y)
          (assert-equal x (page-remove! A x))
          (assert-equal (list y) (page-contents A))

          (assert-thrown 'object-state
                         (page-remove! B y))

          (component-append! C z)
          (assert-thrown 'object-state
                         (page-remove! A z)))

        (lambda ()
          (close-page! A)
          (close-page! B)))))
