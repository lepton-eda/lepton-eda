;; Test Scheme procedures related to pages.

(use-modules (unit-test))
(use-modules (geda page))
(use-modules (geda object))

(begin-test 'page
   (let ((page-a (make-page "/test/page/A"))
         (page-b (make-page "/test/page/B")))
     (assert-equal "/test/page/A" (page-filename page-a))
     (assert-equal (list page-a page-b) (active-pages))))

(begin-test 'page-append
  (let ((A (make-page "/test/page/A"))
        (B (make-page "/test/page/B"))
        (x (make-line '(0 . 0) '(1 . 2)))
        (y (make-line '(0 . 1) '(2 . 2))))

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
        (page-append! A z)))))

(begin-test 'page-remove
  (let ((A (make-page "/test/page/A"))
        (B (make-page "/test/page/B"))
        (x (make-line '(0 . 0) '(2 . 0)))
        (y (make-line '(0 . 0) '(0 . 2))))

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

    (assert-thrown 'object-state
      (let* ((C (make-component "test component" '(1 . 2) 0 #t #f))
             (z (component-append! C (make-line '(1 . 0) '(2 . 2)))))
        (page-remove! A z))) ))
