;;; Test Scheme procedures related to text objects.

(use-modules (lepton object))

(test-begin "text" 31)

(let ((a (make-text '(1 . 2) 'lower-left 0 "test text" 10 #t 'both 21))
      (b (make-text '(1 . 2) 'lower-left 0 "test text" 10 #t 'both)))

  (test-equal 'text (object-type a))
  (test-assert (object-type? a 'text))

  (test-assert (text? a))
  (test-assert (text? b))

  (test-equal '(1 . 2) (text-anchor a))
  (test-equal 'lower-left (text-align a))
  (test-equal 0 (text-angle a))
  (test-equal "test text" (text-string a))
  (test-equal 10 (text-size a))
  (test-assert (text-visible? a))
  (test-equal 'both (text-attribute-mode a))
  (test-equal 21 (object-color a))

  (test-equal (text-anchor a) (text-anchor b))
  (test-equal (text-align a) (text-align b))
  (test-equal (text-angle a) (text-angle b))
  (test-equal (text-string a) (text-string b))
  (test-equal (text-size a) (text-size b))
  (test-equal (text-visible? a) (text-visible? b))
  (test-equal (text-attribute-mode a) (text-attribute-mode b))

  (test-equal (list (text-anchor a) (text-align a) (text-angle a)
                    (text-string a) (text-size a) (text-visible? a)
                    (text-attribute-mode a) (object-color a))
    (text-info a))

  (set-text! a '(3 . 4) 'upper-right 180 "more text" 20 #f 'name)
  (test-equal '(3 . 4) (text-anchor a))
  (test-equal 'upper-right (text-align a))
  (test-equal 180 (text-angle a))
  (test-equal "more text" (text-string a))
  (test-equal 20 (text-size a))
  (test-assert (not (text-visible? a)))
  (test-equal 'name (text-attribute-mode a))
  (test-equal 21 (object-color a))

  (set-text! a '(3 . 4) 'upper-right 180 "more text" 20 #f 'name 22)
  (test-equal 22 (object-color a))

  (test-assert-thrown 'misc-error
                      (set-text! a '(3 . 4) 'fnord 180 "more text" 20 #f 'name))
  (test-assert-thrown 'misc-error
                      (set-text! a '(3 . 4) 'upper-right 180 "more text" 20 #f 'fnord))
  (test-assert-thrown 'misc-error
                      (set-text! a '(3 . 4) 'upper-right 1 "more text" 20 #f 'name))
  )

(test-end "text")


(test-begin "set-text-visibility!" 6)

(let ((a (make-text '(1 . 2) 'lower-left 0 "test text" 10 #t 'both 21))
      (b (make-text '(1 . 2) 'lower-left 0 "test text" 10 #t 'both 21)))
  (test-assert (text-visible? a))

  (set-text-visibility! a #f)
  (test-assert (not (text-visible? a)))

  (set-text-visibility! a #t)
  (test-assert (text-visible? a))
  (test-equal (text-info a) (text-info b))

  (set-text-visibility! a 'bork)
  (test-assert (text-visible? a))
  (test-equal (text-info a) (text-info b)))

(test-end "set-text-visibility!")


(test-begin "set-text-string!" 4)

(let ((a (make-text '(1 . 2) 'lower-left 0 "test text" 10 #t 'both 21))
      (b (make-text '(1 . 2) 'lower-left 0 "test text" 10 #t 'both 21)))
  (test-equal "test text" (text-string a))

  (set-text-string! a "new test text")
  (test-equal "new test text" (text-string a))

  (set-text-string! a "test text")
  (test-equal "test text" (text-string a))
  (test-equal (text-info a) (text-info b)))

(test-end "set-text-string!")
