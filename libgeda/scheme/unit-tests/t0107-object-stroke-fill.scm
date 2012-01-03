;; Test Scheme procedures for object stroke properties.

(use-modules (unit-test))
(use-modules (geda object))

(begin-test 'stroke
  (let ((a (make-line '(1 . 2) '(3 . 4))))

    (assert-equal a (set-object-stroke! a 1 'none 'solid 'foo 'bar))
    (assert-equal 1 (object-stroke-width a))
    (assert-equal 'none (object-stroke-cap a))
    (assert-equal '(solid) (object-stroke-dash a))
    (assert-equal a (apply set-object-stroke! a (object-stroke a)))

    (set-object-stroke! a 1 'square 'dotted 2 'bar)
    (assert-equal 'square (object-stroke-cap a))
    (assert-equal '(dotted 2) (object-stroke-dash a))
    (assert-equal a (apply set-object-stroke! a (object-stroke a)))

    (set-object-stroke! a 1 'round 'dashed 3 4)
    (assert-equal 'round (object-stroke-cap a))
    (assert-equal '(dashed 3 4) (object-stroke-dash a))
    (assert-equal a (apply set-object-stroke! a (object-stroke a)))

    (set-object-stroke! a 1 'round 'center 5 6)
    (assert-equal '(center 5 6) (object-stroke-dash a))
    (assert-equal a (apply set-object-stroke! a (object-stroke a)))

    (set-object-stroke! a 1 'round 'phantom 7 8)
    (assert-equal '(phantom 7 8) (object-stroke-dash a))
    (assert-equal a (apply set-object-stroke! a (object-stroke a)))

    ;; Invalid symbol arguments
    (assert-thrown 'misc-error
                   (set-object-stroke! a 1 'BAD-VALUE 'solid))
    (assert-thrown 'misc-error
                   (set-object-stroke! a 1 'none 'BAD-VALUE))
    ;; Missing dash length/space arguments
    (assert-thrown 'misc-error
                   (set-object-stroke! a 1 'none 'dashed 5))
    (assert-thrown 'misc-error
                   (set-object-stroke! a 1 'none 'dashed))
    (assert-thrown 'misc-error
                   (set-object-stroke! a 1 'none 'center 5))
    (assert-thrown 'misc-error
                   (set-object-stroke! a 1 'none 'center))
    (assert-thrown 'misc-error
                   (set-object-stroke! a 1 'none 'phantom 5))
    (assert-thrown 'misc-error
                   (set-object-stroke! a 1 'none 'phantom))
    (assert-thrown 'misc-error
                   (set-object-stroke! a 1 'none 'dotted))
    ))

(begin-test 'fill
  (let ((a (make-box '(1 . 2) '(3 . 4))))

    (assert-equal a (set-object-fill! a 'hollow))
    (assert-equal '(hollow) (object-fill a))
    (assert-equal a (apply set-object-fill! a (object-fill a)))

    (assert-equal a (set-object-fill! a 'solid))
    (assert-equal '(solid) (object-fill a))
    (assert-equal a (apply set-object-fill! a (object-fill a)))

    (assert-equal a (set-object-fill! a 'hatch 1 2 3))
    (assert-equal '(hatch 1 2 3) (object-fill a))
    (assert-equal a (apply set-object-fill! a (object-fill a)))

    (assert-equal a (set-object-fill! a 'mesh 4 5 6 7 8))
    (assert-equal '(mesh 4 5 6 7 8) (object-fill a))
    (assert-equal a (apply set-object-fill! a (object-fill a)))

    ;; Invalid symbol arguments
    (assert-thrown 'misc-error
                   (set-object-fill! a 'BAD-VALUE))
    ;; Missing fill width/angle/space arguments
    (assert-thrown 'misc-error
                   (set-object-fill! a 'hatch))
    (assert-thrown 'misc-error
                   (set-object-fill! a 'hatch 1))
    (assert-thrown 'misc-error
                   (set-object-fill! a 'hatch 1 2))
    (assert-thrown 'misc-error
                   (set-object-fill! a 'mesh))
    (assert-thrown 'misc-error
                   (set-object-fill! a 'mesh 1))
    (assert-thrown 'misc-error
                   (set-object-fill! a 'mesh 1 2))
    (assert-thrown 'misc-error
                   (set-object-fill! a 'mesh 1 2 3))
    (assert-thrown 'misc-error
                   (set-object-fill! a 'mesh 1 2 3 4))
    ))
