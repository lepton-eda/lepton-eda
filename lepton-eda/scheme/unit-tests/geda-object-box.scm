;;; Test Scheme procedures related to box objects.

(use-modules ((geda object) #:renamer (symbol-prefix-proc 'geda:)))


(test-begin "geda:boxes" 16)

(let* ((a (geda:make-box '(1 . 4) '(3 . 2) 21))
       (b (geda:copy-object a)))

  (test-equal 'box (geda:object-type a))

  (test-assert (geda:box? a))
  (test-assert (geda:box? b))

  (test-equal '(1 . 4) (geda:box-top-left a))
  (test-equal '(3 . 2) (geda:box-bottom-right a))
  (test-equal (geda:box-top-left a) (geda:box-top-left b))
  (test-equal (geda:box-bottom-right a) (geda:box-bottom-right b))
  (test-equal 21 (geda:object-color a))
  (test-equal (list (geda:box-top-left a) (geda:box-bottom-right a) (geda:object-color a)) (geda:box-info a))

                                        ; Check that geda:set-box! swaps corners around correctly
  (geda:set-box! a '(5 . 6) '(7 . 8))
  (test-equal '(5 . 8) (geda:box-top-left a))
  (test-equal '(7 . 6) (geda:box-bottom-right a))
  (geda:set-box! a '(7 . 6) '(5 . 8))
  (test-equal '(5 . 8) (geda:box-top-left a))
  (test-equal '(7 . 6) (geda:box-bottom-right a))
  (test-equal 21 (geda:object-color a))

  (geda:set-box! a '(5 . 6) '(7 . 8) 22)
  (test-equal 22 (geda:object-color a))

  (geda:set-object-color! a 21)
  (test-equal 21 (list-ref (geda:box-info a) 2))
  )

(test-end "geda:boxes")
