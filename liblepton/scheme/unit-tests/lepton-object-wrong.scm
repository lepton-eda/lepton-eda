;;; Test basic object procedures with wrong arguments.

(use-modules (lepton object))

(test-begin "non-object")

(test-assert (not (object? 'a)))
(test-assert-thrown 'wrong-type-arg (object-type 'a))
(test-assert (not (object-type? 'a 'a)))

(test-end "non-object")
