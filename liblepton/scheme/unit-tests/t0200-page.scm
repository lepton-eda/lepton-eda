;; Test Scheme procedures related to pages.

(use-modules (unit-test)
             (geda attrib)
             (lepton object)
             ((geda page) #:renamer (symbol-prefix-proc 'geda:))
             (lepton page))

(begin-test 'page
   (let ((page-a (make-page "/test/page/A"))
         (page-b (make-page "/test/page/B")))
     (assert-true (page? page-a))

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

          (assert-equal A (page-append! A x))
          (assert-equal (list x) (page-contents A))

          (assert-equal A (page-append! A x y))
          (assert-equal (list x y) (page-contents A))

          (assert-thrown 'object-state
             (page-append! B x))

          (assert-thrown 'object-state
            (let* ((C (make-component "test component" '(1 . 2) 0 #t #f))
                   (z (make-line '(1 . 0) '(2 . 2))))
              (component-append! C z)
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
          (assert-equal A (page-remove! A x))
          (assert-equal '() (page-contents A))
          (assert-equal A (page-remove! A x))
          (assert-equal B (page-remove! B x))

          (page-append! A x y)
          (assert-equal A (page-remove! A x))
          (assert-equal (list y) (page-contents A))

          (assert-thrown 'object-state
                         (page-remove! B y))

          (component-append! C z)
          (assert-thrown 'object-state
                         (page-remove! A z)))

        (lambda ()
          (close-page! A)
          (close-page! B)))))


;;; The same tests for the deprecated (geda page) module
;;; functions.

(begin-test 'geda:page
   (let ((page-a (geda:make-page "/test/page/A"))
         (page-b (geda:make-page "/test/page/B")))
     (assert-true (geda:page? page-a))

     (assert-equal "/test/page/A" (geda:page-filename page-a))
     (assert-equal (list page-a page-b) (geda:active-pages))

     (assert-equal page-a (geda:set-page-filename! page-a "/test/page/C"))
     (assert-equal "/test/page/C" (geda:page-filename page-a))

     (geda:close-page! page-a)
     (assert-equal (list page-b) (geda:active-pages))
     (geda:close-page! page-b)))

(begin-test 'geda:page-append
  (let ((A (geda:make-page "/test/page/C"))
        (B (geda:make-page "/test/page/D"))
        (x (make-line '(0 . 0) '(1 . 2)))
        (y (make-line '(0 . 1) '(2 . 2))))

    (dynamic-wind ; Make sure pages are cleaned up
        (lambda () #f)
        (lambda ()
          (assert-equal '() (geda:page-contents A))

          (assert-equal A (geda:page-append! A x))
          (assert-equal (list x) (geda:page-contents A))

          (assert-equal A (geda:page-append! A x y))
          (assert-equal (list x y) (geda:page-contents A))

          (assert-thrown 'object-state
             (geda:page-append! B x))

          (assert-thrown 'object-state
            (let* ((C (make-component "test component" '(1 . 2) 0 #t #f))
                   (z (make-line '(1 . 0) '(2 . 2))))
              (component-append! C z)
              (geda:page-append! A z))))

        (lambda ()
          (geda:close-page! A)
          (geda:close-page! B)))))

(begin-test 'geda:page-remove
  (let ((A (geda:make-page "/test/page/E"))
        (B (geda:make-page "/test/page/F"))
        (C (make-component "test component" '(1 . 2) 0 #t #f))
        (x (make-line '(0 . 0) '(2 . 0)))
        (y (make-line '(0 . 0) '(0 . 2)))
        (z (make-line '(1 . 0) '(2 . 2))))

    (dynamic-wind ; Make sure pages are cleaned up
        (lambda () #f)
        (lambda ()
          (geda:page-append! A x)
          (assert-equal A (geda:page-remove! A x))
          (assert-equal '() (geda:page-contents A))
          (assert-equal A (geda:page-remove! A x))
          (assert-equal B (geda:page-remove! B x))

          (geda:page-append! A x y)
          (assert-equal A (geda:page-remove! A x))
          (assert-equal (list y) (geda:page-contents A))

          (assert-thrown 'object-state
                         (geda:page-remove! B y))

          (component-append! C z)
          (assert-thrown 'object-state
                         (geda:page-remove! A z)))

        (lambda ()
          (geda:close-page! A)
          (geda:close-page! B)))))
