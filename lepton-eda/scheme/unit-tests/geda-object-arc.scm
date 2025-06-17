;;; Test Scheme procedures related to arc objects.

(use-modules ((geda object) #:renamer (symbol-prefix-proc 'geda:)))

(test-begin "geda:arcs" 20)

(let* ((a (geda:make-arc '(1 . 2) 3 45 90 21))
       (b (geda:copy-object a)))

  (test-equal 'arc (geda:object-type a))

  (test-assert (geda:arc? a))
  (test-assert (geda:arc? b))

  (test-equal '(1 . 2) (geda:arc-center a))
  (test-equal 3 (geda:arc-radius a))
  (test-equal 45 (geda:arc-start-angle a))
  (test-equal 90 (geda:arc-sweep-angle a))
  (test-equal (geda:arc-center a) (geda:arc-center b))
  (test-equal (geda:arc-radius a) (geda:arc-radius b))
  (test-equal (geda:arc-start-angle a) (geda:arc-start-angle b))
  (test-equal (geda:arc-sweep-angle a) (geda:arc-sweep-angle b))
  (test-equal 21 (geda:object-color a))
  (test-equal (list (geda:arc-center a) (geda:arc-radius a)
                    (geda:arc-start-angle a) (geda:arc-sweep-angle a)
                    (geda:object-color a))
    (geda:arc-info a))

  (geda:set-arc! a '(5 . 6) 7 180 270)
  (test-equal '(5 . 6) (geda:arc-center a))
  (test-equal 7 (geda:arc-radius a))
  (test-equal 180 (geda:arc-start-angle a))
  (test-equal 270 (geda:arc-sweep-angle a))
  (test-equal 21 (geda:object-color a))
  (geda:set-arc! a '(5 . 6) 7 180 270 22)
  (test-equal 22 (geda:object-color a))

  (geda:set-object-color! a 21)
  (test-equal 21 (list-ref (geda:arc-info a) 4))
  )

(test-end "geda:arcs")
