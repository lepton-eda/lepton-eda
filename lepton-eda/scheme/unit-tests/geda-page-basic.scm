;;; Test Scheme procedures related to pages.

(use-modules ((geda page) #:renamer (symbol-prefix-proc 'geda:))
             (lepton attrib)
             (lepton object))

(test-begin "geda:page")

(let ((page-a (geda:make-page "/test/page/A"))
      (page-b (geda:make-page "/test/page/B")))
  (test-assert (geda:page? page-a))

  (test-equal "/test/page/A" (geda:page-filename page-a))
  (test-equal (list page-a page-b) (geda:active-pages))

  (test-equal page-a (geda:set-page-filename! page-a "/test/page/C"))
  (test-equal "/test/page/C" (geda:page-filename page-a))

  (geda:close-page! page-a)
  (test-equal (list page-b) (geda:active-pages))
  (geda:close-page! page-b))

(test-end "geda:page")


(test-begin "geda:page-append")

(let ((A (geda:make-page "/test/page/C"))
      (B (geda:make-page "/test/page/D"))
      (x (make-line '(0 . 0) '(1 . 2)))
      (y (make-line '(0 . 1) '(2 . 2))))

  (dynamic-wind                   ; Make sure pages are cleaned up
    (lambda () #f)
    (lambda ()
      (test-equal '() (geda:page-contents A))

      (test-equal A (geda:page-append! A x))
      (test-equal (list x) (geda:page-contents A))

      (test-equal A (geda:page-append! A x y))
      (test-equal (list x y) (geda:page-contents A))

      (test-assert-thrown 'object-state
                          (geda:page-append! B x))

      (test-assert-thrown 'object-state
                          (let* ((C (make-component "test component" '(1 . 2) 0 #t #f))
                                 (z (make-line '(1 . 0) '(2 . 2))))
                            (component-append! C z)
                            (geda:page-append! A z))))

    (lambda ()
      (geda:close-page! A)
      (geda:close-page! B))))

(test-end "geda:page-append")


(test-begin "geda:page-remove")

(let ((A (geda:make-page "/test/page/E"))
      (B (geda:make-page "/test/page/F"))
      (C (make-component "test component" '(1 . 2) 0 #t #f))
      (x (make-line '(0 . 0) '(2 . 0)))
      (y (make-line '(0 . 0) '(0 . 2)))
      (z (make-line '(1 . 0) '(2 . 2))))

  (dynamic-wind                   ; Make sure pages are cleaned up
    (lambda () #f)
    (lambda ()
      (geda:page-append! A x)
      (test-equal A (geda:page-remove! A x))
      (test-equal '() (geda:page-contents A))
      (test-equal A (geda:page-remove! A x))
      (test-equal B (geda:page-remove! B x))

      (geda:page-append! A x y)
      (test-equal A (geda:page-remove! A x))
      (test-equal (list y) (geda:page-contents A))

      (test-assert-thrown 'object-state
                          (geda:page-remove! B y))

      (component-append! C z)
      (test-assert-thrown 'object-state
                          (geda:page-remove! A z)))

    (lambda ()
      (geda:close-page! A)
      (geda:close-page! B))))

(test-end "geda:page-remove")
