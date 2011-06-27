;; Test Scheme procedures related to line objects.

(use-modules (unit-test))
(use-modules (geda object))

(begin-test 'lines
  (let ((a (make-line '(1 . 2) '(3 . 4) 21))
        (b (make-line '(1 . 2) '(3 . 4))))

    (assert-equal 'line (object-type a))

    (assert-true (line? a))

    (assert-equal '(1 . 2) (line-start a))
    (assert-equal '(3 . 4) (line-end a))
    (assert-equal (line-start a) (line-start b))
    (assert-equal (line-end a) (line-end b))
    (assert-equal 21 (object-color a))
    (assert-equal (list (line-start a) (line-end a) (object-color a)) (line-info a))

    (set-line! a '(5 . 6) '(7 . 8))
    (assert-equal '(5 . 6) (line-start a))
    (assert-equal '(7 . 8) (line-end a))
    (assert-equal 21 (object-color a))

    (set-line! a '(5 . 6) '(7 . 8) 22)
    (assert-equal 22 (object-color a))

    (set-object-color! a 21)
    (assert-equal 21 (list-ref (line-info a) 2))))

(make-net '(1 . 2) '(3 . 4))

(begin-test 'nets
  (let ((a (make-net '(1 . 2) '(3 . 4) 21))
        (b (make-net '(1 . 2) '(3 . 4))))

    (assert-equal 'net (object-type a))

    (assert-true (net? a))

    (assert-equal '(1 . 2) (line-start a))
    (assert-equal '(3 . 4) (line-end a))
    (assert-equal (line-start a) (line-start b))
    (assert-equal (line-end a) (line-end b))
    (assert-equal 21 (object-color a))
    (assert-equal (list (line-start a) (line-end a) (object-color a)) (line-info a))

    (set-line! a '(5 . 6) '(7 . 8))
    (assert-equal '(5 . 6) (line-start a))
    (assert-equal '(7 . 8) (line-end a))
    (assert-equal 21 (object-color a))

    (set-line! a '(5 . 6) '(7 . 8) 22)
    (assert-equal 22 (object-color a))

    (set-object-color! a 21)
    (assert-equal 21 (list-ref (line-info a) 2))))

(begin-test 'buses
  (let ((a (make-bus '(1 . 2) '(3 . 4) 21))
        (b (make-bus '(1 . 2) '(3 . 4))))

    (assert-equal 'bus (object-type a))

    (assert-true (bus? a))

    (assert-equal '(1 . 2) (line-start a))
    (assert-equal '(3 . 4) (line-end a))
    (assert-equal (line-start a) (line-start b))
    (assert-equal (line-end a) (line-end b))
    (assert-equal 21 (object-color a))
    (assert-equal (list (line-start a) (line-end a) (object-color a)) (line-info a))

    (set-line! a '(5 . 6) '(7 . 8))
    (assert-equal '(5 . 6) (line-start a))
    (assert-equal '(7 . 8) (line-end a))
    (assert-equal 21 (object-color a))

    (set-line! a '(5 . 6) '(7 . 8) 22)
    (assert-equal 22 (object-color a))

    (set-object-color! a 21)
    (assert-equal 21 (list-ref (line-info a) 2))))

(begin-test 'net-pins
  (let ((a (make-net-pin '(1 . 2) '(3 . 4) 21))
        (b (make-net-pin '(1 . 2) '(3 . 4))))

    (assert-equal 'pin (object-type a))

    (assert-true (pin? a))
    (assert-true (net-pin? a))
    (assert-true (not (bus-pin? a)))

    (assert-equal '(1 . 2) (line-start a))
    (assert-equal '(3 . 4) (line-end a))
    (assert-equal (line-start a) (line-start b))
    (assert-equal (line-end a) (line-end b))
    (assert-equal 21 (object-color a))
    (assert-equal (list (line-start a) (line-end a) (object-color a)) (line-info a))

    (set-line! a '(5 . 6) '(7 . 8))
    (assert-equal '(5 . 6) (line-start a))
    (assert-equal '(7 . 8) (line-end a))
    (assert-equal 21 (object-color a))

    (set-line! a '(5 . 6) '(7 . 8) 22)
    (assert-equal 22 (object-color a))

    (set-object-color! a 21)
    (assert-equal 21 (list-ref (line-info a) 2))))

(begin-test 'bus-pins
  (let ((a (make-bus-pin '(1 . 2) '(3 . 4) 21))
        (b (make-bus-pin '(1 . 2) '(3 . 4))))

    (assert-equal 'pin (object-type a))

    (assert-true (pin? a))
    (assert-true (bus-pin? a))
    (assert-true (not (net-pin? a)))

    (assert-equal '(1 . 2) (line-start a))
    (assert-equal '(3 . 4) (line-end a))
    (assert-equal (line-start a) (line-start b))
    (assert-equal (line-end a) (line-end b))
    (assert-equal 21 (object-color a))
    (assert-equal (list (line-start a) (line-end a) (object-color a)) (line-info a))

    (set-line! a '(5 . 6) '(7 . 8))
    (assert-equal '(5 . 6) (line-start a))
    (assert-equal '(7 . 8) (line-end a))
    (assert-equal 21 (object-color a))

    (set-line! a '(5 . 6) '(7 . 8) 22)
    (assert-equal 22 (object-color a))

    (set-object-color! a 21)
    (assert-equal 21 (list-ref (line-info a) 2))))
