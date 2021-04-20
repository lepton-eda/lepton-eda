;;; Test Scheme procedures for object stroke properties.

(use-modules (lepton object))


(test-begin "stroke" 24)

(let ((a (make-line '(1 . 2) '(3 . 4))))

  (test-equal a (set-object-stroke! a 1 'none 'solid 'foo 'bar))
  (test-equal 1 (object-stroke-width a))
  (test-equal 'none (object-stroke-cap a))
  (test-equal '(solid) (object-stroke-dash a))
  (test-equal a (apply set-object-stroke! a (object-stroke a)))

  (set-object-stroke! a 1 'square 'dotted 2 'bar)
  (test-equal 'square (object-stroke-cap a))
  (test-equal '(dotted 2) (object-stroke-dash a))
  (test-equal a (apply set-object-stroke! a (object-stroke a)))

  (set-object-stroke! a 1 'round 'dashed 3 4)
  (test-equal 'round (object-stroke-cap a))
  (test-equal '(dashed 3 4) (object-stroke-dash a))
  (test-equal a (apply set-object-stroke! a (object-stroke a)))

  (set-object-stroke! a 1 'round 'center 5 6)
  (test-equal '(center 5 6) (object-stroke-dash a))
  (test-equal a (apply set-object-stroke! a (object-stroke a)))

  (set-object-stroke! a 1 'round 'phantom 7 8)
  (test-equal '(phantom 7 8) (object-stroke-dash a))
  (test-equal a (apply set-object-stroke! a (object-stroke a)))

  ;; Invalid symbol arguments
  (test-assert-thrown 'misc-error
                 (set-object-stroke! a 1 'BAD-VALUE 'solid))
  (test-assert-thrown 'misc-error
                 (set-object-stroke! a 1 'none 'BAD-VALUE))
  ;; Missing dash length/space arguments
  (test-assert-thrown 'misc-error
                 (set-object-stroke! a 1 'none 'dashed 5))
  (test-assert-thrown 'misc-error
                 (set-object-stroke! a 1 'none 'dashed))
  (test-assert-thrown 'misc-error
                 (set-object-stroke! a 1 'none 'center 5))
  (test-assert-thrown 'misc-error
                 (set-object-stroke! a 1 'none 'center))
  (test-assert-thrown 'misc-error
                 (set-object-stroke! a 1 'none 'phantom 5))
  (test-assert-thrown 'misc-error
                 (set-object-stroke! a 1 'none 'phantom))
  (test-assert-thrown 'misc-error
                 (set-object-stroke! a 1 'none 'dotted))
  )

(test-end "stroke")
