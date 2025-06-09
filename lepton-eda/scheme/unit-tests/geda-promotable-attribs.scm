;;; Test promotable-attributes function.

(use-modules ((geda attrib) #:renamer (symbol-prefix-proc 'geda:))
             (lepton object)
             (lepton page))

;; (test-begin "geda:promotable-attributes")
;; (throw 'missing-unit-test "We can't test this at the moment")
;; (test-end "geda:promotable-attributes")

;; (test-begin "geda:promote-attribs!")
;; (throw 'missing-unit-test "We can't test this at the moment")
;; (test-end "geda:promote-attribs!")

(test-begin "geda:promote-attribs!/not-in-page")
(let ((p (make-net-pin '(0 . 0) '(100 . 0))))
  (test-assert-thrown 'object-state (geda:promote-attribs! p)))
(test-end "geda:promote-attribs!/not-in-page")

(test-begin "geda:promote-attribs!/non-component")
(let ((P (make-page "/test/page/A"))
      (p (make-net-pin '(0 . 0) '(100 . 0))))
  (page-append! P p)
  (test-equal '() (geda:promote-attribs! p)))
(test-end "geda:promote-attribs!/non-component")
