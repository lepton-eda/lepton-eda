;;; Test Scheme procedures related to arc objects.

(use-modules (lepton object))

(test-begin "arcs" 20)

(let* ((a (make-arc '(1 . 2) 3 45 90 21))
       (b (copy-object a)))

  (test-equal 'arc (object-type a))
  (test-assert (object-type? a 'arc))

  (test-assert (arc? a))
  (test-assert (arc? b))

  (test-equal '(1 . 2) (arc-center a))
  (test-equal 3 (arc-radius a))
  (test-equal 45 (arc-start-angle a))
  (test-equal 90 (arc-sweep-angle a))
  (test-equal (arc-center a) (arc-center b))
  (test-equal (arc-radius a) (arc-radius b))
  (test-equal (arc-start-angle a) (arc-start-angle b))
  (test-equal (arc-sweep-angle a) (arc-sweep-angle b))
  (test-equal 21 (object-color a))
  (test-equal (list (arc-center a) (arc-radius a)
                    (arc-start-angle a) (arc-sweep-angle a)
                    (object-color a))
    (arc-info a))

  (set-arc! a '(5 . 6) 7 180 270)
  (test-equal '(5 . 6) (arc-center a))
  (test-equal 7 (arc-radius a))
  (test-equal 180 (arc-start-angle a))
  (test-equal 270 (arc-sweep-angle a))
  (test-equal 21 (object-color a))
  (set-arc! a '(5 . 6) 7 180 270 22)
  (test-equal 22 (object-color a))

  (set-object-color! a 21)
  (test-equal 21 (list-ref (arc-info a) 4))
  )

(test-end "arcs")
