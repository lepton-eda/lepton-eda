;;; Test Scheme procedures related to box objects.

(use-modules (lepton object))

(test-begin "boxes" 16)

(let* ((a (make-box '(1 . 4) '(3 . 2) 21))
       (b (copy-object a)))

  (test-equal 'box (object-type a))
  (test-assert (object-type? a 'box))

  (test-assert (box? a))
  (test-assert (box? b))

  (test-equal '(1 . 4) (box-top-left a))
  (test-equal '(3 . 2) (box-bottom-right a))
  (test-equal (box-top-left a) (box-top-left b))
  (test-equal (box-bottom-right a) (box-bottom-right b))
  (test-equal 21 (object-color a))
  (test-equal (list (box-top-left a) (box-bottom-right a) (object-color a)) (box-info a))

                                        ; Check that set-box! swaps corners around correctly
  (set-box! a '(5 . 6) '(7 . 8))
  (test-equal '(5 . 8) (box-top-left a))
  (test-equal '(7 . 6) (box-bottom-right a))
  (set-box! a '(7 . 6) '(5 . 8))
  (test-equal '(5 . 8) (box-top-left a))
  (test-equal '(7 . 6) (box-bottom-right a))
  (test-equal 21 (object-color a))

  (set-box! a '(5 . 6) '(7 . 8) 22)
  (test-equal 22 (object-color a))

  (set-object-color! a 21)
  (test-equal 21 (list-ref (box-info a) 2))
  )

(test-end "boxes")
