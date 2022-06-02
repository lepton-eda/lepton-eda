;;; Test Scheme procedures working with toplevel wrapped foreign
;;; pointers.

(use-modules (system foreign)
             (lepton toplevel foreign)
             (lepton toplevel))

(define toplevel (make-toplevel))
(define *toplevel-cp (check-toplevel toplevel 1))
(define *toplevel-pp (toplevel->pointer toplevel))

(test-begin "toplevel-pointer")

;;; Test that all the variables are not #f.
(test-assert toplevel)
(test-assert *toplevel-cp)
(test-assert *toplevel-pp)

;;; Test their type.
(test-assert (is-toplevel? toplevel))
(test-assert (not (pointer? toplevel)))

(test-assert (not (is-toplevel? *toplevel-cp)))
(test-assert (pointer? *toplevel-cp))

(test-assert (not (is-toplevel? *toplevel-pp)))
(test-assert (pointer? *toplevel-pp))

;;; Test that the pointers are not NULL.
(test-assert (not (null-pointer? *toplevel-cp)))
(test-assert (not (null-pointer? *toplevel-pp)))

;;; Tests for equality.
(test-eq *toplevel-cp *toplevel-pp)
(test-assert (not (eq? toplevel *toplevel-cp)))
(test-eq toplevel (pointer->toplevel *toplevel-cp))
(test-eq toplevel (pointer->toplevel *toplevel-pp))
(test-eq toplevel (pointer->toplevel (toplevel->pointer toplevel)))

;;; Tests for wrong type arguments in check-toplevel().
(test-assert-thrown 'wrong-type-arg (check-toplevel 'anything 1))
(test-assert-thrown 'wrong-type-arg (check-toplevel #f 1))
(test-assert-thrown 'wrong-type-arg (check-toplevel %null-pointer 1))

;;; Tests for wrong type arguments in pointer->toplevel().
(test-assert-thrown 'wrong-type-arg (pointer->toplevel 'anything))
(test-assert-thrown 'wrong-type-arg (pointer->toplevel #f))
(test-assert-thrown 'misc-error (pointer->toplevel %null-pointer))

;;; Tests for wrong type arguments in toplevel->pointer().
(test-assert-thrown 'wrong-type-arg (toplevel->pointer 'anything))
(test-assert-thrown 'wrong-type-arg (toplevel->pointer #f))
(test-assert-thrown 'wrong-type-arg (toplevel->pointer %null-pointer))

(test-end "toplevel-pointer")
