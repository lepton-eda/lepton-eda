;;; Test Scheme procedures related to converting pages to & from
;;; strings.

(use-modules ((geda page) #:renamer (symbol-prefix-proc 'geda:))
             (lepton object))

;;; This is a very roundabout test.  We don't want to test string->page
;;; at this point, and we don't want to hardcode any assumptions about
;;; file format into this part of the testsuite, so we just make sure
;;; that pages with identical contents have identical string
;;; representation, and that pages with different contents have
;;; different string representation.
(test-begin "geda:page->string")

(let ((A (geda:make-page "/test/page/A"))
      (B (geda:make-page "/test/page/B"))
      (x (make-line '(0 . 0) '(1 . 1)))
      (y (make-line '(0 . 0) '(1 . 1))))
  (geda:page-append! A x)
  (geda:page-append! B y)

  ;; Pages with identical content
  (test-equal (geda:page->string A) (geda:page->string B))

  ;; Pages with different content
  (set-line! y '(0 . 0) '(2 . 2))
  (test-assert (not (equal? (geda:page->string A) (geda:page->string B))))
  )

(test-end "geda:page->string")


;; We test string->page by round-tripping a page through a string back
;; to a page.  Note that this test is deliberately designed to avoid
;; issues related to different gafrc read options.
(test-begin "geda:string->page")

(let ((A (geda:make-page "/test/page/A"))
      (x (make-line '(0 . 0) '(1 . 1))))
  (geda:page-append! A x)
  (let* ((B (geda:string->page "/test/page/B" (geda:page->string A))))
    (test-equal "/test/page/B" (geda:page-filename B))
    (test-equal 1 (length (geda:page-contents B)))
    (test-equal (line-info x) (line-info (car (geda:page-contents B))))))

(test-end "geda:string->page")


(test-begin "geda:string->page: syntax")

(test-assert-thrown 'string-format (geda:string->page "/test/page/A" "__GARBAGE__"))

(test-end "geda:string->page: syntax")
