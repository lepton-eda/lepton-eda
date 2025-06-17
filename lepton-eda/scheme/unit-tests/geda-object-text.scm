;;; Test Scheme procedures related to text objects.

(use-modules ((geda object) #:renamer (symbol-prefix-proc 'geda:)))

(test-begin "geda:text" 31)

(let ((a (geda:make-text '(1 . 2) 'lower-left 0 "test text" 10 #t 'both 21))
      (b (geda:make-text '(1 . 2) 'lower-left 0 "test text" 10 #t 'both)))

  (test-equal 'text (geda:object-type a))

  (test-assert (geda:text? a))
  (test-assert (geda:text? b))

  (test-equal '(1 . 2) (geda:text-anchor a))
  (test-equal 'lower-left (geda:text-align a))
  (test-equal 0 (geda:text-angle a))
  (test-equal "test text" (geda:text-string a))
  (test-equal 10 (geda:text-size a))
  (test-assert (geda:text-visible? a))
  (test-equal 'both (geda:text-attribute-mode a))
  (test-equal 21 (geda:object-color a))

  (test-equal (geda:text-anchor a) (geda:text-anchor b))
  (test-equal (geda:text-align a) (geda:text-align b))
  (test-equal (geda:text-angle a) (geda:text-angle b))
  (test-equal (geda:text-string a) (geda:text-string b))
  (test-equal (geda:text-size a) (geda:text-size b))
  (test-equal (geda:text-visible? a) (geda:text-visible? b))
  (test-equal (geda:text-attribute-mode a) (geda:text-attribute-mode b))

  (test-equal (list (geda:text-anchor a) (geda:text-align a) (geda:text-angle a)
                    (geda:text-string a) (geda:text-size a) (geda:text-visible? a)
                    (geda:text-attribute-mode a) (geda:object-color a))
    (geda:text-info a))

  (geda:set-text! a '(3 . 4) 'upper-right 180 "more text" 20 #f 'name)
  (test-equal '(3 . 4) (geda:text-anchor a))
  (test-equal 'upper-right (geda:text-align a))
  (test-equal 180 (geda:text-angle a))
  (test-equal "more text" (geda:text-string a))
  (test-equal 20 (geda:text-size a))
  (test-assert (not (geda:text-visible? a)))
  (test-equal 'name (geda:text-attribute-mode a))
  (test-equal 21 (geda:object-color a))

  (geda:set-text! a '(3 . 4) 'upper-right 180 "more text" 20 #f 'name 22)
  (test-equal 22 (geda:object-color a))

  (test-assert-thrown 'misc-error
                      (geda:set-text! a '(3 . 4) 'fnord 180 "more text" 20 #f 'name))
  (test-assert-thrown 'misc-error
                      (geda:set-text! a '(3 . 4) 'upper-right 180 "more text" 20 #f 'fnord))
  (test-assert-thrown 'misc-error
                      (geda:set-text! a '(3 . 4) 'upper-right 1 "more text" 20 #f 'name))
  )

(test-end "geda:text")


(test-begin "geda:set-text-visibility!" 6)

(let ((a (geda:make-text '(1 . 2) 'lower-left 0 "test text" 10 #t 'both 21))
      (b (geda:make-text '(1 . 2) 'lower-left 0 "test text" 10 #t 'both 21)))
  (test-assert (geda:text-visible? a))

  (geda:set-text-visibility! a #f)
  (test-assert (not (geda:text-visible? a)))

  (geda:set-text-visibility! a #t)
  (test-assert (geda:text-visible? a))
  (test-equal (geda:text-info a) (geda:text-info b))

  (geda:set-text-visibility! a 'bork)
  (test-assert (geda:text-visible? a))
  (test-equal (geda:text-info a) (geda:text-info b)))

(test-end "geda:set-text-visibility!")


(test-begin "geda:set-text-string!" 4)

(let ((a (geda:make-text '(1 . 2) 'lower-left 0 "test text" 10 #t 'both 21))
      (b (geda:make-text '(1 . 2) 'lower-left 0 "test text" 10 #t 'both 21)))
  (test-equal "test text" (geda:text-string a))

  (geda:set-text-string! a "new test text")
  (test-equal "new test text" (geda:text-string a))

  (geda:set-text-string! a "test text")
  (test-equal "test text" (geda:text-string a))
  (test-equal (geda:text-info a) (geda:text-info b)))

(test-end "geda:set-text-string!")
