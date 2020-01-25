;; Test Scheme procedures related to text objects.

(use-modules (unit-test)
             (lepton object)
             ((geda object) #:renamer (symbol-prefix-proc 'geda:)))

(begin-test 'text
  (let ((a (make-text '(1 . 2) 'lower-left 0 "test text" 10 #t 'both 21))
        (b (make-text '(1 . 2) 'lower-left 0 "test text" 10 #t 'both)))

    (assert-equal 'text (object-type a))

    (assert-true (text? a))
    (assert-true (text? b))

    (assert-equal '(1 . 2) (text-anchor a))
    (assert-equal 'lower-left (text-align a))
    (assert-equal 0 (text-angle a))
    (assert-equal "test text" (text-string a))
    (assert-equal 10 (text-size a))
    (assert-true (text-visible? a))
    (assert-equal 'both (text-attribute-mode a))
    (assert-equal 21 (object-color a))

    (assert-equal (text-anchor a) (text-anchor b))
    (assert-equal (text-align a) (text-align b))
    (assert-equal (text-angle a) (text-angle b))
    (assert-equal (text-string a) (text-string b))
    (assert-equal (text-size a) (text-size b))
    (assert-equal (text-visible? a) (text-visible? b))
    (assert-equal (text-attribute-mode a) (text-attribute-mode b))

    (assert-equal (list (text-anchor a) (text-align a) (text-angle a)
                        (text-string a) (text-size a) (text-visible? a)
                        (text-attribute-mode a) (object-color a))
                  (text-info a))

    (set-text! a '(3 . 4) 'upper-right 180 "more text" 20 #f 'name)
    (assert-equal '(3 . 4) (text-anchor a))
    (assert-equal 'upper-right (text-align a))
    (assert-equal 180 (text-angle a))
    (assert-equal "more text" (text-string a))
    (assert-equal 20 (text-size a))
    (assert-true (not (text-visible? a)))
    (assert-equal 'name (text-attribute-mode a))
    (assert-equal 21 (object-color a))

    (set-text! a '(3 . 4) 'upper-right 180 "more text" 20 #f 'name 22)
    (assert-equal 22 (object-color a))

    (assert-thrown 'misc-error
      (set-text! a '(3 . 4) 'fnord 180 "more text" 20 #f 'name))
    (assert-thrown 'misc-error
      (set-text! a '(3 . 4) 'upper-right 180 "more text" 20 #f 'fnord))
    (assert-thrown 'misc-error
      (set-text! a '(3 . 4) 'upper-right 1 "more text" 20 #f 'name))
    ))

(begin-test 'set-text-visibility!
  (let ((a (make-text '(1 . 2) 'lower-left 0 "test text" 10 #t 'both 21))
        (b (make-text '(1 . 2) 'lower-left 0 "test text" 10 #t 'both 21)))
    (assert-true (text-visible? a))

    (set-text-visibility! a #f)
    (assert-true (not (text-visible? a)))

    (set-text-visibility! a #t)
    (assert-true (text-visible? a))
    (assert-equal (text-info a) (text-info b))

    (set-text-visibility! a 'bork)
    (assert-true (text-visible? a))
    (assert-equal (text-info a) (text-info b))))

(begin-test 'set-text-string!
  (let ((a (make-text '(1 . 2) 'lower-left 0 "test text" 10 #t 'both 21))
        (b (make-text '(1 . 2) 'lower-left 0 "test text" 10 #t 'both 21)))
    (assert-equal "test text" (text-string a))

    (set-text-string! a "new test text")
    (assert-equal "new test text" (text-string a))

    (set-text-string! a "test text")
    (assert-equal "test text" (text-string a))
    (assert-equal (text-info a) (text-info b))))

;;; The same tests for the deprecated (geda object) module
;;; functions.

(begin-test 'geda:text
  (let ((a (geda:make-text '(1 . 2) 'lower-left 0 "test text" 10 #t 'both 21))
        (b (geda:make-text '(1 . 2) 'lower-left 0 "test text" 10 #t 'both)))

    (assert-equal 'text (geda:object-type a))

    (assert-true (geda:text? a))
    (assert-true (geda:text? b))

    (assert-equal '(1 . 2) (geda:text-anchor a))
    (assert-equal 'lower-left (geda:text-align a))
    (assert-equal 0 (geda:text-angle a))
    (assert-equal "test text" (geda:text-string a))
    (assert-equal 10 (geda:text-size a))
    (assert-true (geda:text-visible? a))
    (assert-equal 'both (geda:text-attribute-mode a))
    (assert-equal 21 (geda:object-color a))

    (assert-equal (geda:text-anchor a) (geda:text-anchor b))
    (assert-equal (geda:text-align a) (geda:text-align b))
    (assert-equal (geda:text-angle a) (geda:text-angle b))
    (assert-equal (geda:text-string a) (geda:text-string b))
    (assert-equal (geda:text-size a) (geda:text-size b))
    (assert-equal (geda:text-visible? a) (geda:text-visible? b))
    (assert-equal (geda:text-attribute-mode a) (geda:text-attribute-mode b))

    (assert-equal (list (geda:text-anchor a) (geda:text-align a) (geda:text-angle a)
                        (geda:text-string a) (geda:text-size a) (geda:text-visible? a)
                        (geda:text-attribute-mode a) (geda:object-color a))
                  (geda:text-info a))

    (geda:set-text! a '(3 . 4) 'upper-right 180 "more text" 20 #f 'name)
    (assert-equal '(3 . 4) (geda:text-anchor a))
    (assert-equal 'upper-right (geda:text-align a))
    (assert-equal 180 (geda:text-angle a))
    (assert-equal "more text" (geda:text-string a))
    (assert-equal 20 (geda:text-size a))
    (assert-true (not (geda:text-visible? a)))
    (assert-equal 'name (geda:text-attribute-mode a))
    (assert-equal 21 (geda:object-color a))

    (geda:set-text! a '(3 . 4) 'upper-right 180 "more text" 20 #f 'name 22)
    (assert-equal 22 (geda:object-color a))

    (assert-thrown 'misc-error
      (geda:set-text! a '(3 . 4) 'fnord 180 "more text" 20 #f 'name))
    (assert-thrown 'misc-error
      (geda:set-text! a '(3 . 4) 'upper-right 180 "more text" 20 #f 'fnord))
    (assert-thrown 'misc-error
      (geda:set-text! a '(3 . 4) 'upper-right 1 "more text" 20 #f 'name))
    ))

(begin-test 'geda:set-text-visibility!
  (let ((a (geda:make-text '(1 . 2) 'lower-left 0 "test text" 10 #t 'both 21))
        (b (geda:make-text '(1 . 2) 'lower-left 0 "test text" 10 #t 'both 21)))
    (assert-true (geda:text-visible? a))

    (geda:set-text-visibility! a #f)
    (assert-true (not (geda:text-visible? a)))

    (geda:set-text-visibility! a #t)
    (assert-true (geda:text-visible? a))
    (assert-equal (geda:text-info a) (geda:text-info b))

    (geda:set-text-visibility! a 'bork)
    (assert-true (geda:text-visible? a))
    (assert-equal (geda:text-info a) (geda:text-info b))))

(begin-test 'geda:set-text-string!
  (let ((a (geda:make-text '(1 . 2) 'lower-left 0 "test text" 10 #t 'both 21))
        (b (geda:make-text '(1 . 2) 'lower-left 0 "test text" 10 #t 'both 21)))
    (assert-equal "test text" (geda:text-string a))

    (geda:set-text-string! a "new test text")
    (assert-equal "new test text" (geda:text-string a))

    (geda:set-text-string! a "test text")
    (assert-equal "test text" (geda:text-string a))
    (assert-equal (geda:text-info a) (geda:text-info b))))
