;; Test promotable-attributes function

(use-modules (unit-test)
             (lepton attrib)
             ((geda attrib) #:renamer (symbol-prefix-proc 'geda:))
             (lepton object)
             (lepton page))

;; Unfortunately, we can't test this at the moment, because the
;; default list of promotable attribute names is empty.  We suppress
;; config file loading when running the unit tests, and even though we
;; could call the (always-promote-attributes ...) config file
;; procedure, but it wouldn't do us any good because we can't call
;; i_vars_libgeda_set() from Scheme [1].  So instead, we just fail.
;;
;; [1] This is a good thing -- it shouldn't be necessary!
(begin-test 'promotable-attributes
            (throw 'missing-unit-test "We can't test this at the moment"))

(begin-test 'promote-attribs!
            (throw 'missing-unit-test "We can't test this at the moment"))

(begin-test 'promote-attribs!/not-in-page
  (let ((p (make-net-pin '(0 . 0) '(100 . 0))))
    (assert-thrown 'object-state (promote-attribs! p))))

(begin-test 'promote-attribs!/non-component
  (let ((P (make-page "/test/page/A"))
        (p (make-net-pin '(0 . 0) '(100 . 0))))
    (page-append! P p)
    (assert-equal '() (promote-attribs! p))))

;;; The same tests for the deprecated (geda attrib) module
;;; functions.
(begin-test 'geda:promotable-attributes
            (throw 'missing-unit-test "We can't test this at the moment"))

(begin-test 'geda:promote-attribs!
            (throw 'missing-unit-test "We can't test this at the moment"))

(begin-test 'geda:promote-attribs!/not-in-page
  (let ((p (make-net-pin '(0 . 0) '(100 . 0))))
    (assert-thrown 'object-state (geda:promote-attribs! p))))

(begin-test 'geda:promote-attribs!/non-component
  (let ((P (make-page "/test/page/A"))
        (p (make-net-pin '(0 . 0) '(100 . 0))))
    (page-append! P p)
    (assert-equal '() (geda:promote-attribs! p))))
