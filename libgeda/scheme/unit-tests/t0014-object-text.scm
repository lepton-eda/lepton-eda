;; Test Scheme procedures related to text objects.

(use-modules (unit-test))
(use-modules (geda object))

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
