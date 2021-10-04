;;; Test Scheme procedures related to pages.

(use-modules (lepton attrib)
             (lepton object)
             (lepton page))

(test-begin "page")

(let ((page-a (make-page "/test/page/A"))
      (page-b (make-page "/test/page/B")))
  (test-assert (page? page-a))

  (test-equal "/test/page/A" (page-filename page-a))
  (test-equal (list page-a page-b) (active-pages))

  (test-equal page-a (set-page-filename! page-a "/test/page/C"))
  (test-equal "/test/page/C" (page-filename page-a))

  (close-page! page-a)
  (test-equal (list page-b) (active-pages))
  (close-page! page-b))

(test-end "page")


(test-begin "page-append")

(let ((A (make-page "/test/page/C"))
      (B (make-page "/test/page/D"))
      (x (make-line '(0 . 0) '(1 . 2)))
      (y (make-line '(0 . 1) '(2 . 2))))

  (dynamic-wind                   ; Make sure pages are cleaned up
    (lambda () #f)
    (lambda ()
      (test-equal '() (page-contents A))

      (test-equal A (page-append! A x))
      (test-equal (list x) (page-contents A))

      (test-equal A (page-append! A x y))
      (test-equal (list x y) (page-contents A))

      (test-assert-thrown 'object-state
                          (page-append! B x))

      (test-assert-thrown 'object-state
                          (let* ((C (make-component "test component" '(1 . 2) 0 #t #f))
                                 (z (make-line '(1 . 0) '(2 . 2))))
                            (component-append! C z)
                            (page-append! A z))))

    (lambda ()
      (close-page! A)
      (close-page! B))))

(test-end "page-append")


(test-begin "page-remove")

(let ((A (make-page "/test/page/E"))
      (B (make-page "/test/page/F"))
      (C (make-component "test component" '(1 . 2) 0 #t #f))
      (x (make-line '(0 . 0) '(2 . 0)))
      (y (make-line '(0 . 0) '(0 . 2)))
      (z (make-line '(1 . 0) '(2 . 2))))

  (dynamic-wind                   ; Make sure pages are cleaned up
    (lambda () #f)
    (lambda ()
      (page-append! A x)
      (test-equal A (page-remove! A x))
      (test-equal '() (page-contents A))
      (test-equal A (page-remove! A x))
      (test-equal B (page-remove! B x))

      (page-append! A x y)
      (test-equal A (page-remove! A x))
      (test-equal (list y) (page-contents A))

      (test-assert-thrown 'object-state
                          (page-remove! B y))

      (component-append! C z)
      (test-assert-thrown 'object-state
                          (page-remove! A z)))

    (lambda ()
      (close-page! A)
      (close-page! B))))

(test-end "page-remove")


(test-begin "page wrong-type-arg")

(test-assert (not (page? 'x)))

(test-assert-thrown 'wrong-type-arg (object-page 'x))
(test-assert-thrown 'wrong-type-arg (make-page 'x))
(test-assert-thrown 'wrong-type-arg (close-page! 'x))
(test-assert-thrown 'wrong-type-arg (page-filename 'x))
(test-assert-thrown 'wrong-type-arg (set-page-filename! 'x "filename"))
(test-assert-thrown 'wrong-type-arg
                    (set-page-filename! (make-page "filename") 'x))
(test-assert-thrown 'wrong-type-arg (page-contents 'x))
(test-assert-thrown 'wrong-type-arg (page-dirty? 'x))
(test-assert-thrown 'wrong-type-arg (set-page-dirty! 'x))
(test-assert-thrown 'wrong-type-arg (page->string 'x))
(test-assert-thrown 'wrong-type-arg (string->page "filename" 'x))
(test-assert-thrown 'wrong-type-arg (string->page 'x "string"))
(test-assert-thrown 'wrong-type-arg (file->page 'x))
(test-assert-thrown 'wrong-type-arg
                    (page-append! 'x (make-line '(0 . 0) '(1 . 1))))
(test-assert-thrown 'wrong-type-arg
                    (page-append! (make-page "filename") 'x))
(test-assert-thrown 'wrong-type-arg
                    (page-remove! 'x (make-line '(0 . 0) '(1 . 1))))
(test-assert-thrown 'wrong-type-arg
                    (page-remove! (make-page "filename") 'x))

(test-end "page wrong-type-arg")
