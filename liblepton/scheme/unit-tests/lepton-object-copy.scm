;;; Test object copying

(use-modules (srfi srfi-1)
             (lepton attrib)
             (lepton object)
             (lepton page))

;;; This test verifies that if an object is copied, any links to
;;; containing pages, containing components are removed, and any
;;; attribute attachments are broken.
(test-begin "copy-object-breaks-links")

(let ((P (make-page "/test/page/A"))
      (A (make-component "test component" '(0 . 0) 0 #t #f))
      (p (make-net-pin '(0 . 0) '(100 . 0)))
      (x (make-text '(1 . 2) 'lower-left 0 "name=x" 10 #t 'both)))

  (page-append! P A x)

  (test-equal P (object-page x))
  (test-equal #f (object-page (copy-object x)))

  (attach-attribs! A x)

  (test-equal A (attrib-attachment x))
  (test-equal #f (attrib-attachment (copy-object x)))

  (detach-attribs! A x)
  (page-remove! P x)
  (component-append! A p x)

  (test-equal A (object-component x))
  (test-equal #f (object-component (copy-object x)))

  (attach-attribs! p x)
  (test-equal p (attrib-attachment x))
  (test-equal #f (attrib-attachment (copy-object x))))

(test-end "copy-object-breaks-links")


;;; This test checks that copies of components are deep copies.
(test-begin "copy-object-deep-component")

(let* ((A (make-component "test component" '(0 . 0) 0 #t #f))
       (p (make-net-pin '(0 . 0) '(100 . 0))))
  (component-append! A p)
  (test-equal (list p) (member p (component-contents A)))
  (test-equal #f (member p (component-contents (copy-object A)))))

(test-end "copy-object-deep-component")

(test-begin "copy-object-wrong-argument")
(test-assert-thrown 'wrong-type-arg (copy-object 'a))
(test-end "copy-object-wrong-argument")
