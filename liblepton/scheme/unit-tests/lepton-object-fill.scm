;;; Test Scheme procedures for object fill properties.

(use-modules (lepton object))

(use-modules ((geda object) #:renamer (symbol-prefix-proc 'geda:)))

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
  )

(test-end "fill")
