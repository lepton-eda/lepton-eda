;; Test Scheme procedures related to arc objects.

(use-modules (unit-test)
             (lepton object)
             ((geda object) #:renamer (symbol-prefix-proc 'geda:)))

(begin-test 'arcs
  (let* ((a (make-arc '(1 . 2) 3 45 90 21))
         (b (copy-object a)))

    (assert-equal 'arc (object-type a))

    (assert-true (arc? a))
    (assert-true (arc? b))

    (assert-equal '(1 . 2) (arc-center a))
    (assert-equal 3 (arc-radius a))
    (assert-equal 45 (arc-start-angle a))
    (assert-equal 90 (arc-sweep-angle a))
    (assert-equal (arc-center a) (arc-center b))
    (assert-equal (arc-radius a) (arc-radius b))
    (assert-equal (arc-start-angle a) (arc-start-angle b))
    (assert-equal (arc-sweep-angle a) (arc-sweep-angle b))
    (assert-equal 21 (object-color a))
    (assert-equal (list (arc-center a) (arc-radius a)
                        (arc-start-angle a) (arc-sweep-angle a)
                        (object-color a))
                  (arc-info a))

    (set-arc! a '(5 . 6) 7 180 270)
    (assert-equal '(5 . 6) (arc-center a))
    (assert-equal 7 (arc-radius a))
    (assert-equal 180 (arc-start-angle a))
    (assert-equal 270 (arc-sweep-angle a))
    (assert-equal 21 (object-color a))
    (set-arc! a '(5 . 6) 7 180 270 22)
    (assert-equal 22 (object-color a))

    (set-object-color! a 21)
    (assert-equal 21 (list-ref (arc-info a) 4))
))

;;; The same tests for the deprecated (geda object) module
;;; functions.

(begin-test 'geda:arcs
  (let* ((a (geda:make-arc '(1 . 2) 3 45 90 21))
         (b (geda:copy-object a)))

    (assert-equal 'arc (geda:object-type a))

    (assert-true (geda:arc? a))
    (assert-true (geda:arc? b))

    (assert-equal '(1 . 2) (geda:arc-center a))
    (assert-equal 3 (geda:arc-radius a))
    (assert-equal 45 (geda:arc-start-angle a))
    (assert-equal 90 (geda:arc-sweep-angle a))
    (assert-equal (geda:arc-center a) (geda:arc-center b))
    (assert-equal (geda:arc-radius a) (geda:arc-radius b))
    (assert-equal (geda:arc-start-angle a) (geda:arc-start-angle b))
    (assert-equal (geda:arc-sweep-angle a) (geda:arc-sweep-angle b))
    (assert-equal 21 (geda:object-color a))
    (assert-equal (list (geda:arc-center a) (geda:arc-radius a)
                        (geda:arc-start-angle a) (geda:arc-sweep-angle a)
                        (geda:object-color a))
                  (geda:arc-info a))

    (geda:set-arc! a '(5 . 6) 7 180 270)
    (assert-equal '(5 . 6) (geda:arc-center a))
    (assert-equal 7 (geda:arc-radius a))
    (assert-equal 180 (geda:arc-start-angle a))
    (assert-equal 270 (geda:arc-sweep-angle a))
    (assert-equal 21 (geda:object-color a))
    (geda:set-arc! a '(5 . 6) 7 180 270 22)
    (assert-equal 22 (geda:object-color a))

    (geda:set-object-color! a 21)
    (assert-equal 21 (list-ref (geda:arc-info a) 4))
))
