;;; Test promotable-attributes function.

(use-modules ((geda attrib) #:renamer (symbol-prefix-proc 'geda:))
             (lepton object)
             (lepton page))

;;; Unfortunately, we can't test this at the moment, because the
;;; default list of promotable attribute names is empty.  We suppress
;;; config file loading when running the unit tests, and even though we
;;; could call the (always-promote-attributes ...) config file
;;; procedure, but it wouldn't do us any good because we can't call
;;; i_vars_libgeda_set() from Scheme [1].  So instead, we just fail.
;;;
;;; [1] This is a good thing -- it shouldn't be necessary!

(test-skip "geda:promotable-attributes")
(test-skip "geda:promote-attribs!")

(test-begin "geda:promotable-attributes")
(throw 'missing-unit-test "We can't test this at the moment")
(test-end "geda:promotable-attributes")

(test-begin "geda:promote-attribs!")
(throw 'missing-unit-test "We can't test this at the moment")
(test-end "geda:promote-attribs!")

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
