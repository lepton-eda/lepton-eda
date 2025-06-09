;;; Test Scheme procedures working with page wrapped foreign
;;; pointers.

(use-modules (system foreign)
             (lepton page foreign)
             (lepton page))

(define page (make-page "dummy"))
(define *page-cp (check-page page 1))
(define *page-pp (page->pointer page))

(test-begin "page-pointer")

;;; Test that all the variables are not #f.
(test-assert page)
(test-assert *page-cp)
(test-assert *page-pp)

;;; Test their type.
(test-assert (is-page? page))
(test-assert (not (pointer? page)))

(test-assert (not (is-page? *page-cp)))
(test-assert (pointer? *page-cp))

(test-assert (not (is-page? *page-pp)))
(test-assert (pointer? *page-pp))

;;; Test that the pointers are not NULL.
(test-assert (not (null-pointer? *page-cp)))
(test-assert (not (null-pointer? *page-pp)))

;;; Tests for equality.
(test-eq *page-cp *page-pp)
(test-assert (not (eq? page *page-cp)))
(test-eq page (pointer->page *page-cp))
(test-eq page (pointer->page *page-pp))
(test-eq page (pointer->page (page->pointer page)))

;;; Tests for wrong type arguments in check-page().
(test-assert-thrown 'wrong-type-arg (check-page 'anything 1))
(test-assert-thrown 'wrong-type-arg (check-page #f 1))
(test-assert-thrown 'wrong-type-arg (check-page %null-pointer 1))

;;; Tests for wrong type arguments in pointer->page().
(test-assert-thrown 'wrong-type-arg (pointer->page 'anything))
(test-assert-thrown 'wrong-type-arg (pointer->page #f))
(test-assert-thrown 'misc-error (pointer->page %null-pointer))

;;; Tests for wrong type arguments in page->pointer().
(test-assert-thrown 'wrong-type-arg (page->pointer 'anything))
(test-assert-thrown 'wrong-type-arg (page->pointer #f))
(test-assert-thrown 'wrong-type-arg (page->pointer %null-pointer))

(test-end "page-pointer")
