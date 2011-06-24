;; Test object copying

(use-modules (unit-test))
(use-modules (geda object))
(use-modules (geda page))
(use-modules (geda attrib))
(use-modules (srfi srfi-1))

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
