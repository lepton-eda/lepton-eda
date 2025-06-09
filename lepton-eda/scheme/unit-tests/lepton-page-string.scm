;;; Test Scheme procedures related to converting pages to & from
;;; strings.

(use-modules (lepton object)
             (lepton page))

;;; This is a very roundabout test.  We don't want to test string->page
;;; at this point, and we don't want to hardcode any assumptions about
;;; file format into this part of the testsuite, so we just make sure
;;; that pages with identical contents have identical string
;;; representation, and that pages with different contents have
;;; different string representation.
(test-begin "page->string")

(let ((A (make-page "/test/page/A"))
      (B (make-page "/test/page/B"))
      (x (make-line '(0 . 0) '(1 . 1)))
      (y (make-line '(0 . 0) '(1 . 1))))
  (page-append! A x)
  (page-append! B y)

  ;; Pages with identical content
  (test-equal (page->string A) (page->string B))

  ;; Pages with different content
  (set-line! y '(0 . 0) '(2 . 2))
  (test-assert (not (equal? (page->string A) (page->string B))))
  )

(test-end "page->string")


;;; We test string->page by round-tripping a page through a string back
;;; to a page.  Note that this test is deliberately designed to avoid
;;; issues related to different gafrc read options.
(test-begin "string->page")

(let ((A (make-page "/test/page/A"))
      (x (make-line '(0 . 0) '(1 . 1))))
  (page-append! A x)
  (let* ((B (string->page "/test/page/B" (page->string A))))
    (test-equal "/test/page/B" (page-filename B))
    (test-equal 1 (length (page-contents B)))
    (test-equal (line-info x) (line-info (car (page-contents B))))))

(test-end "string->page")


(test-begin "string->page: syntax")

(test-assert-thrown 'string-format (string->page "/test/page/A" "__GARBAGE__"))

(test-end "string->page: syntax")
