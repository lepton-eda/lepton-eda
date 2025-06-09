;;; Test Scheme procedures related to circle objects.

(use-modules ((geda object) #:renamer (symbol-prefix-proc 'geda:)))

(test-begin "geda:circles" 14)

(let* ((a (geda:make-circle '(1 . 2) 3 21))
       (b (geda:copy-object a)))

  (test-equal 'circle (geda:object-type a))

  (test-assert (geda:circle? a))
  (test-assert (geda:circle? b))

  (test-equal '(1 . 2) (geda:circle-center a))
  (test-equal 3 (geda:circle-radius a))
  (test-equal (geda:circle-center a) (geda:circle-center b))
  (test-equal (geda:circle-radius a) (geda:circle-radius b))
  (test-equal 21 (geda:object-color a))
  (test-equal (list (geda:circle-center a) (geda:circle-radius a) (geda:object-color a))
                (geda:circle-info a))

  (geda:set-circle! a '(5 . 6) 7)
  (test-equal '(5 . 6) (geda:circle-center a))
  (test-equal 7 (geda:circle-radius a))
  (test-equal 21 (geda:object-color a))
  (geda:set-circle! a '(5 . 6) 7 22)
  (test-equal 22 (geda:object-color a))

  (geda:set-object-color! a 21)
  (test-equal 21 (list-ref (geda:circle-info a) 2)))

(test-end "geda:circles")
