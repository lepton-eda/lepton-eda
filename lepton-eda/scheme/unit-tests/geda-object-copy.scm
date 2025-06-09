;;; Test object copying

(use-modules (srfi srfi-1)
             ((geda object) #:renamer (symbol-prefix-proc 'geda:))
             (lepton attrib)
             (lepton page))

;;; This test verifies that if an object is copied, any links to
;;; containing pages, containing components are removed, and any
;;; attribute attachments are broken.
(test-begin "geda:copy-object-breaks-links")

(let ((P (make-page "/test/page/A"))
      (A (geda:make-component "test component" '(0 . 0) 0 #t #f))
      (p (geda:make-net-pin '(0 . 0) '(100 . 0)))
      (x (geda:make-text '(1 . 2) 'lower-left 0 "name=x" 10 #t 'both)))

  (page-append! P A x)

  (test-equal P (object-page x))
  (test-equal #f (object-page (geda:copy-object x)))

  (attach-attribs! A x)

  (test-equal A (attrib-attachment x))
  (test-equal #f (attrib-attachment (geda:copy-object x)))

  (detach-attribs! A x)
  (page-remove! P x)
  (geda:component-append! A p x)

  (test-equal A (geda:object-component x))
  (test-equal #f (geda:object-component (geda:copy-object x)))

  (attach-attribs! p x)
  (test-equal p (attrib-attachment x))
  (test-equal #f (attrib-attachment (geda:copy-object x))))

(test-end "geda:copy-object-breaks-links")


;;; This test checks that copies of components are deep copies.
(test-begin "geda:copy-object-deep-component")

(let* ((A (geda:make-component "test component" '(0 . 0) 0 #t #f))
       (p (geda:make-net-pin '(0 . 0) '(100 . 0))))
  (geda:component-append! A p)
  (test-equal (list p) (member p (geda:component-contents A)))
  (test-equal #f (member p (geda:component-contents (geda:copy-object A)))))

(test-end "geda:copy-object-deep-component")
