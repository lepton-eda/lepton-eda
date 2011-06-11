;; Test promotable-attributes function

(use-modules (unit-test))

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
