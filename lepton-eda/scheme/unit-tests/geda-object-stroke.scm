;;; Test Scheme procedures for object stroke properties.

(use-modules ((geda object) #:renamer (symbol-prefix-proc 'geda:)))

(test-begin "geda:stroke" 24)

(let ((a (geda:make-line '(1 . 2) '(3 . 4))))

  (test-equal a (geda:set-object-stroke! a 1 'none 'solid 'foo 'bar))
  (test-equal 1 (geda:object-stroke-width a))
  (test-equal 'none (geda:object-stroke-cap a))
  (test-equal '(solid) (geda:object-stroke-dash a))
  (test-equal a (apply geda:set-object-stroke! a (geda:object-stroke a)))

  (geda:set-object-stroke! a 1 'square 'dotted 2 'bar)
  (test-equal 'square (geda:object-stroke-cap a))
  (test-equal '(dotted 2) (geda:object-stroke-dash a))
  (test-equal a (apply geda:set-object-stroke! a (geda:object-stroke a)))

  (geda:set-object-stroke! a 1 'round 'dashed 3 4)
  (test-equal 'round (geda:object-stroke-cap a))
  (test-equal '(dashed 3 4) (geda:object-stroke-dash a))
  (test-equal a (apply geda:set-object-stroke! a (geda:object-stroke a)))

  (geda:set-object-stroke! a 1 'round 'center 5 6)
  (test-equal '(center 5 6) (geda:object-stroke-dash a))
  (test-equal a (apply geda:set-object-stroke! a (geda:object-stroke a)))

  (geda:set-object-stroke! a 1 'round 'phantom 7 8)
  (test-equal '(phantom 7 8) (geda:object-stroke-dash a))
  (test-equal a (apply geda:set-object-stroke! a (geda:object-stroke a)))

  ;; Invalid symbol arguments
  (test-assert-thrown 'misc-error
                 (geda:set-object-stroke! a 1 'BAD-VALUE 'solid))
  (test-assert-thrown 'misc-error
                 (geda:set-object-stroke! a 1 'none 'BAD-VALUE))
  ;; Missing dash length/space arguments
  (test-assert-thrown 'misc-error
                 (geda:set-object-stroke! a 1 'none 'dashed 5))
  (test-assert-thrown 'misc-error
                 (geda:set-object-stroke! a 1 'none 'dashed))
  (test-assert-thrown 'misc-error
                 (geda:set-object-stroke! a 1 'none 'center 5))
  (test-assert-thrown 'misc-error
                 (geda:set-object-stroke! a 1 'none 'center))
  (test-assert-thrown 'misc-error
                 (geda:set-object-stroke! a 1 'none 'phantom 5))
  (test-assert-thrown 'misc-error
                 (geda:set-object-stroke! a 1 'none 'phantom))
  (test-assert-thrown 'misc-error
                 (geda:set-object-stroke! a 1 'none 'dotted))
  )

(test-end "geda:stroke")
