;;; Test Scheme procedures for object stroke properties.

(use-modules (lepton object))

(define (new-line) (make-line '(1 . 2) '(3 . 4)))
(define (new-box) (make-box '(1 . 2) '(3 . 4)))
(define (new-circle) (make-circle '(1 . 2) 100))
(define (new-arc) (make-arc '(1 . 2) 100 0 90))
(define new-path make-path)

(test-begin "stroke" 24)

(let ((a (new-line)))

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
  )

(test-end "stroke")


(test-begin "object-stroke")

;;; Test allowable objects.
(test-equal (object-stroke (new-box))
  '(10 square solid))
(test-equal (object-stroke (new-circle))
  '(10 square solid))
(test-equal (object-stroke (new-arc))
  '(10 square solid))
(test-equal (object-stroke (new-path))
  '(10 square solid))
(test-equal (object-stroke (new-line))
  '(10 square solid))

;;; Test combinations of stroke parameters.
(test-equal (object-stroke (set-object-stroke! (new-line) 10 'square 'solid))
  '(10 square solid))
(test-equal (object-stroke (set-object-stroke! (new-line) 15 'square 'solid))
  '(15 square solid))
(test-equal (object-stroke (set-object-stroke! (new-line) 15 'round 'solid))
  '(15 round solid))
(test-equal (object-stroke (set-object-stroke! (new-line) 15 'none 'solid))
  '(15 none solid))
(test-equal (object-stroke (set-object-stroke! (new-line) 15 'none 'dotted 10))
  '(15 none dotted 10))
(test-equal (object-stroke (set-object-stroke! (new-line) 15 'none 'dotted 15))
  '(15 none dotted 15))
(test-equal (object-stroke (set-object-stroke! (new-line) 15 'none 'dotted 15 20))
  '(15 none dotted 15))
(test-equal (object-stroke (set-object-stroke! (new-line) 15 'none 'dashed 15 20))
  '(15 none dashed 15 20))
(test-equal (object-stroke (set-object-stroke! (new-line) 15 'none 'dashed 10 20))
  '(15 none dashed 10 20))
(test-equal (object-stroke (set-object-stroke! (new-line) 15 'none 'center 10 20))
  '(15 none center 10 20))
(test-equal (object-stroke (set-object-stroke! (new-line) 15 'none 'phantom 10 20))
  '(15 none phantom 10 20))

(test-end "object-stroke")

(test-begin "object-stroke-wrong-arguments")

(test-assert-thrown 'wrong-type-arg (object-stroke 'a))
(test-assert-thrown 'wrong-type-arg (object-stroke 1))
(test-assert-thrown 'wrong-number-of-args (object-stroke))
(test-assert-thrown 'wrong-number-of-args (object-stroke (new-line) 1))

(test-end "object-stroke-wrong-arguments")

(test-begin "set-object-stroke-wrong-arguments")

(let ((a (new-line)))
  ;; Invalid symbol arguments.
  (test-assert-thrown 'misc-error
                      (set-object-stroke! a 1 'BAD-VALUE 'solid))
  (test-assert-thrown 'misc-error
                      (set-object-stroke! a 1 'none 'BAD-VALUE))
  ;; Invalid argument type.
  (test-assert-thrown 'wrong-type-arg
                      (set-object-stroke! (new-line) 1 'none 'dotted 'a))
  (test-assert-thrown 'wrong-type-arg
                      (set-object-stroke! (new-line) 1 'none 'dashed 'a 10))
  (test-assert-thrown 'wrong-type-arg
                      (set-object-stroke! (new-line) 1 'none 'dashed 10 'a))

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

  ;; Invalid number of arguments.
  (test-assert-thrown 'wrong-number-of-args
                      (set-object-stroke! a 1 'none 'phantom 15 20 30))
  )

(test-end "set-object-stroke-wrong-arguments")


(test-begin "object-stroke-param-wrong-arguments")

(test-assert-thrown 'wrong-type-arg (object-stroke-width 'a))
(test-assert-thrown 'wrong-type-arg (object-stroke-cap 'a))
(test-assert-thrown 'wrong-type-arg (object-stroke-dash 'a))

(test-end "object-stroke-param-wrong-arguments")
