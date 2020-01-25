;; Test object copying

(use-modules (unit-test)
             (srfi srfi-1)
             (geda attrib)
             (lepton object)
             ((geda object) #:renamer (symbol-prefix-proc 'geda:))
             (lepton page))


;; This test verifies that if an object is copied, any links to
;; containing pages, containing components are removed, and any
;; attribute attachments are broken.
(begin-test 'copy-object-breaks-links
  (let ((P (make-page "/test/page/A"))
        (A (make-component "test component" '(0 . 0) 0 #t #f))
        (p (make-net-pin '(0 . 0) '(100 . 0)))
        (x (make-text '(1 . 2) 'lower-left 0 "name=x" 10 #t 'both)))

    (page-append! P A x)

    (assert-equal P (object-page x))
    (assert-equal #f (object-page (copy-object x)))

    (attach-attribs! A x)

    (assert-equal A (attrib-attachment x))
    (assert-equal #f (attrib-attachment (copy-object x)))

    (detach-attribs! A x)
    (page-remove! P x)
    (component-append! A p x)

    (assert-equal A (object-component x))
    (assert-equal #f (object-component (copy-object x)))

    (attach-attribs! p x)
    (assert-equal p (attrib-attachment x))
    (assert-equal #f (attrib-attachment (copy-object x)))))

;; This test checks that copies of components are deep copies.
(begin-test 'copy-object-deep-component
  (let* ((A (make-component "test component" '(0 . 0) 0 #t #f))
         (p (make-net-pin '(0 . 0) '(100 . 0))))
    (component-append! A p)
    (assert-equal (list p) (member p (component-contents A)))
    (assert-equal #f (member p (component-contents (copy-object A))))))

;;; The same tests for the deprecated (geda object) module
;;; functions.

;; This test verifies that if an object is copied, any links to
;; containing pages, containing components are removed, and any
;; attribute attachments are broken.
(begin-test 'geda:copy-object-breaks-links
  (let ((P (make-page "/test/page/A"))
        (A (geda:make-component "test component" '(0 . 0) 0 #t #f))
        (p (geda:make-net-pin '(0 . 0) '(100 . 0)))
        (x (geda:make-text '(1 . 2) 'lower-left 0 "name=x" 10 #t 'both)))

    (page-append! P A x)

    (assert-equal P (object-page x))
    (assert-equal #f (object-page (geda:copy-object x)))

    (attach-attribs! A x)

    (assert-equal A (attrib-attachment x))
    (assert-equal #f (attrib-attachment (geda:copy-object x)))

    (detach-attribs! A x)
    (page-remove! P x)
    (geda:component-append! A p x)

    (assert-equal A (geda:object-component x))
    (assert-equal #f (geda:object-component (geda:copy-object x)))

    (attach-attribs! p x)
    (assert-equal p (attrib-attachment x))
    (assert-equal #f (attrib-attachment (geda:copy-object x)))))

;; This test checks that copies of components are deep copies.
(begin-test 'geda:copy-object-deep-component
  (let* ((A (geda:make-component "test component" '(0 . 0) 0 #t #f))
         (p (geda:make-net-pin '(0 . 0) '(100 . 0))))
    (geda:component-append! A p)
    (assert-equal (list p) (member p (geda:component-contents A)))
    (assert-equal #f (member p (geda:component-contents (geda:copy-object A))))))
