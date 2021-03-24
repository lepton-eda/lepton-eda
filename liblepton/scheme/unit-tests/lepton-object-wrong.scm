;;; Test basic object procedures with wrong arguments.

(use-modules (lepton object))

(test-begin "non-object")

(test-assert (not (object? 'a)))
(test-assert-thrown 'wrong-type-arg (object-type 'a))
(test-assert (not (object-type? 'a 'a)))

(test-assert-thrown 'wrong-type-arg (object-color 'a))
(test-assert-thrown 'wrong-type-arg (set-object-color! 'a 3))

(test-assert-thrown 'wrong-type-arg (mirror-objects! 0 'a))
(test-assert-thrown 'wrong-type-arg (rotate-objects! '(0 . 0) 90 'a))
(test-assert-thrown 'wrong-type-arg (translate-objects! '(100 . 100) 'a))

(test-end "non-object")
