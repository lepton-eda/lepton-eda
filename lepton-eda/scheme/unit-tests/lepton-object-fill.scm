;;; Test Scheme procedures for object fill properties.

(use-modules (lepton object))

(test-begin "fill" 21)

(let ((a (make-box '(1 . 2) '(3 . 4))))

  (test-equal a (set-object-fill! a 'hollow))
  (test-equal '(hollow) (object-fill a))
  (test-equal a (apply set-object-fill! a (object-fill a)))

  (test-equal a (set-object-fill! a 'solid))
  (test-equal '(solid) (object-fill a))
  (test-equal a (apply set-object-fill! a (object-fill a)))

  (test-equal a (set-object-fill! a 'hatch 1 2 3))
  (test-equal '(hatch 1 2 3) (object-fill a))
  (test-equal a (apply set-object-fill! a (object-fill a)))

  (test-equal a (set-object-fill! a 'mesh 4 5 6 7 8))
  (test-equal '(mesh 4 5 6 7 8) (object-fill a))
  (test-equal a (apply set-object-fill! a (object-fill a)))
  )

(test-end "fill")


(test-begin "object-fill")

;;; Test allowable objects.
(test-equal (object-fill (make-box '(1 . 2) '(3 . 4))) '(hollow))
(test-equal (object-fill (make-circle '(1 . 2) 100)) '(hollow))
(test-equal (object-fill (make-path)) '(hollow))

(test-end "object-fill")


(test-begin "object-fill-wrong-arguments")

(test-assert-thrown 'wrong-type-arg (object-fill 'a))
(test-assert-thrown 'wrong-type-arg (object-fill 1))
(test-assert-thrown 'wrong-number-of-args (object-fill))
(test-assert-thrown 'wrong-number-of-args (object-fill (make-path) 1))

(test-end "object-fill-wrong-arguments")


(test-begin "set-object-fill-wrong-arguments")

(let ((a (make-box '(1 . 2) '(3 . 4))))
  ;; Invalid symbol arguments
  (test-assert-thrown 'misc-error
                      (set-object-fill! a 'BAD-VALUE))
  ;; Missing fill width/angle/space arguments
  (test-assert-thrown 'misc-error
                      (set-object-fill! a 'hatch))
  (test-assert-thrown 'misc-error
                      (set-object-fill! a 'hatch 1))
  (test-assert-thrown 'misc-error
                      (set-object-fill! a 'hatch 1 2))
  (test-assert-thrown 'misc-error
                      (set-object-fill! a 'mesh))
  (test-assert-thrown 'misc-error
                      (set-object-fill! a 'mesh 1))
  (test-assert-thrown 'misc-error
                      (set-object-fill! a 'mesh 1 2))
  (test-assert-thrown 'misc-error
                      (set-object-fill! a 'mesh 1 2 3))
  (test-assert-thrown 'misc-error
                      (set-object-fill! a 'mesh 1 2 3 4))

  ;; Invalid argument type.
  (test-assert-thrown 'wrong-type-arg
                      (set-object-fill! a 'mesh 'a 100 30 50 120))
  (test-assert-thrown 'wrong-type-arg
                      (set-object-fill! a 'mesh 10 'a 30 50 120))
  (test-assert-thrown 'wrong-type-arg
                      (set-object-fill! a 'mesh 10 100 'a 50 120))
  (test-assert-thrown 'wrong-type-arg
                      (set-object-fill! a 'mesh 10 100 30 'a 120))
  (test-assert-thrown 'wrong-type-arg
                      (set-object-fill! a 'mesh 10 100 30 50 'a))
  ;; Invalid number of arguments.
  (test-assert-thrown 'wrong-number-of-args
                      (set-object-fill! a 'mesh 2 3 4 5 6 7)))

(test-end "set-object-fill-wrong-arguments")
