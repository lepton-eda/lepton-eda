;;; Test Scheme procedures for object fill properties.

(use-modules ((geda object) #:renamer (symbol-prefix-proc 'geda:)))

(test-begin "geda:fill" 21)

(let ((a (geda:make-box '(1 . 2) '(3 . 4))))

  (test-equal a (geda:set-object-fill! a 'hollow))
  (test-equal '(hollow) (geda:object-fill a))
  (test-equal a (apply geda:set-object-fill! a (geda:object-fill a)))

  (test-equal a (geda:set-object-fill! a 'solid))
  (test-equal '(solid) (geda:object-fill a))
  (test-equal a (apply geda:set-object-fill! a (geda:object-fill a)))

  (test-equal a (geda:set-object-fill! a 'hatch 1 2 3))
  (test-equal '(hatch 1 2 3) (geda:object-fill a))
  (test-equal a (apply geda:set-object-fill! a (geda:object-fill a)))

  (test-equal a (geda:set-object-fill! a 'mesh 4 5 6 7 8))
  (test-equal '(mesh 4 5 6 7 8) (geda:object-fill a))
  (test-equal a (apply geda:set-object-fill! a (geda:object-fill a)))

  ;; Invalid symbol arguments
  (test-assert-thrown 'misc-error
                 (geda:set-object-fill! a 'BAD-VALUE))
  ;; Missing fill width/angle/space arguments
  (test-assert-thrown 'misc-error
                 (geda:set-object-fill! a 'hatch))
  (test-assert-thrown 'misc-error
                 (geda:set-object-fill! a 'hatch 1))
  (test-assert-thrown 'misc-error
                 (geda:set-object-fill! a 'hatch 1 2))
  (test-assert-thrown 'misc-error
                 (geda:set-object-fill! a 'mesh))
  (test-assert-thrown 'misc-error
                 (geda:set-object-fill! a 'mesh 1))
  (test-assert-thrown 'misc-error
                 (geda:set-object-fill! a 'mesh 1 2))
  (test-assert-thrown 'misc-error
                 (geda:set-object-fill! a 'mesh 1 2 3))
  (test-assert-thrown 'misc-error
                 (geda:set-object-fill! a 'mesh 1 2 3 4))
  )

(test-end "geda:fill")
