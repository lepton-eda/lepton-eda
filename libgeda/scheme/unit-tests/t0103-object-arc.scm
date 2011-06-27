;; Test Scheme procedures related to arc objects.

(use-modules (unit-test))
(use-modules (geda object))

(begin-test 'arcs
  (let* ((a (make-arc '(1 . 2) 3 45 90 21))
         (b (copy-object a)))

    (assert-equal 'arc (object-type a))

    (assert-true (arc? a))
    (assert-true (arc? b))

    (assert-equal '(1 . 2) (arc-center a))
    (assert-equal 3 (arc-radius a))
    (assert-equal 45 (arc-start-angle a))
    (assert-equal 90 (arc-end-angle a))
    (assert-equal (arc-center a) (arc-center b))
    (assert-equal (arc-radius a) (arc-radius b))
    (assert-equal (arc-start-angle a) (arc-start-angle b))
    (assert-equal (arc-end-angle a) (arc-end-angle b))
    (assert-equal 21 (object-color a))
    (assert-equal (list (arc-center a) (arc-radius a)
                        (arc-start-angle a) (arc-end-angle a)
                        (object-color a))
                  (arc-info a))

    (set-arc! a '(5 . 6) 7 180 270)
    (assert-equal '(5 . 6) (arc-center a))
    (assert-equal 7 (arc-radius a))
    (assert-equal 180 (arc-start-angle a))
    (assert-equal 270 (arc-end-angle a))
    (assert-equal 21 (object-color a))
    (set-arc! a '(5 . 6) 7 180 270 22)
    (assert-equal 22 (object-color a))

    (set-object-color! a 21)
    (assert-equal 21 (list-ref (arc-info a) 4))
))
