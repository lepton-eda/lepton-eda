;;; Test promotable-attributes function.

(use-modules (lepton attrib)
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

(test-skip "promotable-attributes")
(test-skip "promote-attribs!")

(test-begin "promotable-attributes")
(throw 'missing-unit-test "We can't test this at the moment")
(test-end "promotable-attributes")

(test-begin "promote-attribs!")
(throw 'missing-unit-test "We can't test this at the moment")
(test-end "promote-attribs!")

(test-begin "promote-attribs!/not-in-page")
(let ((p (make-net-pin '(0 . 0) '(100 . 0))))
  (test-assert-thrown 'object-state (promote-attribs! p)))
(test-end "promote-attribs!/not-in-page")

(test-begin "promote-attribs!/non-component")
(let ((P (make-page "/test/page/A"))
      (p (make-net-pin '(0 . 0) '(100 . 0))))
  (page-append! P p)
  (test-equal '() (promote-attribs! p)))
(test-end "promote-attribs!/non-component")
