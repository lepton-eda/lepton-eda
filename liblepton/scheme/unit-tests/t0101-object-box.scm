;; Test Scheme procedures related to box objects.

(use-modules (unit-test)
             (lepton object)
             ((geda object) #:renamer (symbol-prefix-proc 'geda:)))

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

;;; The same tests for the deprecated (geda object) module
;;; functions.

(begin-test 'geda:boxes
  (let* ((a (geda:make-box '(1 . 4) '(3 . 2) 21))
         (b (geda:copy-object a)))

    (assert-equal 'box (geda:object-type a))

    (assert-true (geda:box? a))
    (assert-true (geda:box? b))

    (assert-equal '(1 . 4) (geda:box-top-left a))
    (assert-equal '(3 . 2) (geda:box-bottom-right a))
    (assert-equal (geda:box-top-left a) (geda:box-top-left b))
    (assert-equal (geda:box-bottom-right a) (geda:box-bottom-right b))
    (assert-equal 21 (geda:object-color a))
    (assert-equal (list (geda:box-top-left a) (geda:box-bottom-right a) (geda:object-color a)) (geda:box-info a))

    ; Check that geda:set-box! swaps corners around correctly
    (geda:set-box! a '(5 . 6) '(7 . 8))
    (assert-equal '(5 . 8) (geda:box-top-left a))
    (assert-equal '(7 . 6) (geda:box-bottom-right a))
    (geda:set-box! a '(7 . 6) '(5 . 8))
    (assert-equal '(5 . 8) (geda:box-top-left a))
    (assert-equal '(7 . 6) (geda:box-bottom-right a))
    (assert-equal 21 (geda:object-color a))

    (geda:set-box! a '(5 . 6) '(7 . 8) 22)
    (assert-equal 22 (geda:object-color a))

    (geda:set-object-color! a 21)
    (assert-equal 21 (list-ref (geda:box-info a) 2))
))
