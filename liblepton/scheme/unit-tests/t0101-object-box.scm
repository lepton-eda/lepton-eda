;; Test Scheme procedures related to box objects.

(use-modules (unit-test))
(use-modules (geda object))

(begin-test 'boxes
  (let* ((a (make-box '(1 . 4) '(3 . 2) 21))
         (b (copy-object a)))

    (assert-equal 'box (object-type a))

    (assert-true (box? a))
    (assert-true (box? b))

    (assert-equal '(1 . 4) (box-top-left a))
    (assert-equal '(3 . 2) (box-bottom-right a))
    (assert-equal (box-top-left a) (box-top-left b))
    (assert-equal (box-bottom-right a) (box-bottom-right b))
    (assert-equal 21 (object-color a))
    (assert-equal (list (box-top-left a) (box-bottom-right a) (object-color a)) (box-info a))

    ; Check that set-box! swaps corners around correctly
    (set-box! a '(5 . 6) '(7 . 8))
    (assert-equal '(5 . 8) (box-top-left a))
    (assert-equal '(7 . 6) (box-bottom-right a))
    (set-box! a '(7 . 6) '(5 . 8))
    (assert-equal '(5 . 8) (box-top-left a))
    (assert-equal '(7 . 6) (box-bottom-right a))
    (assert-equal 21 (object-color a))

    (set-box! a '(5 . 6) '(7 . 8) 22)
    (assert-equal 22 (object-color a))

    (set-object-color! a 21)
    (assert-equal 21 (list-ref (box-info a) 2))
))
