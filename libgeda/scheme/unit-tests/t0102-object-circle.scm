;; Test Scheme procedures related to circle objects.

(use-modules (unit-test))
(use-modules (geda object))

(begin-test 'circles
  (let* ((a (make-circle '(1 . 2) 3 21))
         (b (copy-object a)))

    (assert-equal 'circle (object-type a))

    (assert-true (circle? a))
    (assert-true (circle? b))

    (assert-equal '(1 . 2) (circle-center a))
    (assert-equal 3 (circle-radius a))
    (assert-equal (circle-center a) (circle-center b))
    (assert-equal (circle-radius a) (circle-radius b))
    (assert-equal 21 (object-color a))
    (assert-equal (list (circle-center a) (circle-radius a) (object-color a))
                  (circle-info a))

    (set-circle! a '(5 . 6) 7)
    (assert-equal '(5 . 6) (circle-center a))
    (assert-equal 7 (circle-radius a))
    (assert-equal 21 (object-color a))
    (set-circle! a '(5 . 6) 7 22)
    (assert-equal 22 (object-color a))

    (set-object-color! a 21)
    (assert-equal 21 (list-ref (circle-info a) 2))))
