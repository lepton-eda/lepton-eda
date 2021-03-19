;;; Test Scheme procedures related to circle objects.

(use-modules (lepton object))

(test-begin "circles" 14)

(let* ((a (make-circle '(1 . 2) 3 21))
       (b (copy-object a)))

  (test-equal 'circle (object-type a))
  (test-assert (object-type? a 'circle))

  (test-assert (circle? a))
  (test-assert (circle? b))

  (test-equal '(1 . 2) (circle-center a))
  (test-equal 3 (circle-radius a))
  (test-equal (circle-center a) (circle-center b))
  (test-equal (circle-radius a) (circle-radius b))
  (test-equal 21 (object-color a))
  (test-equal (list (circle-center a) (circle-radius a) (object-color a))
                (circle-info a))

  (set-circle! a '(5 . 6) 7)
  (test-equal '(5 . 6) (circle-center a))
  (test-equal 7 (circle-radius a))
  (test-equal 21 (object-color a))
  (set-circle! a '(5 . 6) 7 22)
  (test-equal 22 (object-color a))

  (set-object-color! a 21)
  (test-equal 21 (list-ref (circle-info a) 2)))

(test-end "circles")
