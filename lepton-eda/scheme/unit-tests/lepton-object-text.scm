;;; Test Scheme procedures related to text objects.

(use-modules (srfi srfi-1)
             (srfi srfi-26)
             (lepton object))

(test-begin "text" 31)

(let ((a (make-text '(1 . 2) 'lower-left 0 "test text" 10 #t 'both 21))
      (b (make-text '(1 . 2) 'lower-left 0 "test text" 10 #t 'both)))

  (test-equal 'text (object-type a))
  (test-assert (object-type? a 'text))
  (test-assert (not (object-type? a 'x)))

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

  (test-equal 'lower-left
    (text-align (make-text '(10 . 20) 'lower-left 0 "text" 10 #t 'name)))
  (test-equal 'middle-left
    (text-align (make-text '(10 . 20) 'middle-left 0 "text" 10 #t 'name)))
  (test-equal 'upper-left
    (text-align (make-text '(10 . 20) 'upper-left 0 "text" 10 #t 'name)))
  (test-equal 'lower-center
    (text-align (make-text '(10 . 20) 'lower-center 0 "text" 10 #t 'name)))
  (test-equal 'middle-center
    (text-align (make-text '(10 . 20) 'middle-center 0 "text" 10 #t 'name)))
  (test-equal 'upper-center
    (text-align (make-text '(10 . 20) 'upper-center 0 "text" 10 #t 'name)))
  (test-equal 'lower-right
    (text-align (make-text '(10 . 20) 'lower-right 0 "text" 10 #t 'name)))
  (test-equal 'middle-right
    (text-align (make-text '(10 . 20) 'middle-right 0 "text" 10 #t 'name)))
  (test-equal 'upper-right
    (text-align (make-text '(10 . 20) 'upper-right 0 "text" 10 #t 'name)))

  (test-equal 'name
    (text-attribute-mode (make-text '(10 . 20) 'upper-right 0 "text" 10 #t 'name)))
  (test-equal 'value
    (text-attribute-mode (make-text '(10 . 20) 'upper-right 0 "text" 10 #t 'value)))
  (test-equal 'both
    (text-attribute-mode (make-text '(10 . 20) 'upper-right 0 "text" 10 #t 'both)))

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


(test-begin "set-text-attribute-mode!")

(let* ((t (make-text '(1 . 2) 'lower-left 0 "test text" 10 #t 'both))
       (info (text-info t)))

  (test-eq (text-attribute-mode t) 'both)
  (set-text-attribute-mode! t 'name)
  (test-eq (text-attribute-mode t) 'name)
  (set-text-attribute-mode! t 'value)
  (test-eq (text-attribute-mode t) 'value)

  (test-equal (text-info (set-text-attribute-mode! t 'both)) info)

  (test-assert-thrown 'misc-error
                      (set-text-attribute-mode! t 'unknown))
  (test-assert-thrown 'wrong-type-arg
                      (set-text-attribute-mode! t #f))
  (test-assert-thrown 'wrong-type-arg
                      (set-text-attribute-mode! t "both")))

(test-end "set-text-attribute-mode!")


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


;;; Make the same text every time.
(define (new-text)
  (make-text '(1 . 2) 'lower-left 0 "test text" 10 #t 'both 21))

(test-begin "text-wrong-argument")

(test-assert-thrown 'wrong-type-arg (text-info 't))
(test-assert-thrown 'wrong-type-arg (text-anchor 't))
(test-assert-thrown 'wrong-type-arg (text-align 't))
(test-assert-thrown 'wrong-type-arg (text-angle 't))
(test-assert-thrown 'wrong-type-arg (text-string 't))
(test-assert-thrown 'wrong-type-arg (text-size 't))
(test-assert-thrown 'wrong-type-arg (text-visible? 't))
(test-assert-thrown 'wrong-type-arg (text-attribute-mode 't))
(test-assert-thrown 'wrong-type-arg (set-text-string! 't "string"))
(test-assert-thrown 'wrong-type-arg (set-text-string! (new-text) 't))
(test-assert-thrown 'wrong-type-arg (set-text-visibility! 't #t))
;;; Wrong visibility.
;;; We cannot test it.  In Scheme all values are boolean: all
;;; values but #f are considered to return #t in boolean
;;; expressions.  So the following won't work:
;;; (test-assert-thrown 'wrong-type-arg (set-text-visibility! (new-text) 't))

(let ((t (new-text)))
  ;; Wrong object.
  (test-assert-thrown 'wrong-type-arg (set-text! 't '(1 . 2) 'lower-left 0 "text" 10 #t 'both 21))
  ;; Wrong anchor.
  (test-assert-thrown 'wrong-type-arg (make-text 'anchor 'lower-left 0 "text" 10 #t 'both 21))
  (test-assert-thrown 'wrong-type-arg (set-text! t 'anchor 'lower-left 0 "text" 10 #t 'both 21))
  ;; Wrong x.
  (test-assert-thrown 'wrong-type-arg (make-text '(x . 2) 'lower-left 0 "text" 10 #t 'both 21))
  (test-assert-thrown 'wrong-type-arg (set-text! t '(x . 2) 'lower-left 0 "text" 10 #t 'both 21))
  ;; Wrong y.
  (test-assert-thrown 'wrong-type-arg (make-text '(1 . y) 'lower-left 0 "text" 10 #t 'both 21))
  (test-assert-thrown 'wrong-type-arg (set-text! t '(1 . y) 'lower-left 0 "text" 10 #t 'both 21))
  ;; Wrong align.
  (test-assert-thrown 'misc-error (make-text '(1 . 2) 'lower 0 "text" 10 #t 'both 21))
  (test-assert-thrown 'misc-error (set-text! t '(1 . 2) 'lower 0 "text" 10 #t 'both 21))
  (test-assert-thrown 'wrong-type-arg (make-text '(1 . 2) #t 0 "text" 10 #t 'both 21))
  (test-assert-thrown 'wrong-type-arg (set-text! t '(1 . 2) #t 0 "text" 10 #t 'both 21))
  ;; Wrong angle.
  (test-assert-thrown 'misc-error (make-text '(1 . 2) 'lower-left 360 "text" 10 #t 'both 21))
  (test-assert-thrown 'misc-error (set-text! t '(1 . 2) 'lower-left 360 "text" 10 #t 'both 21))
  (test-assert-thrown 'wrong-type-arg (make-text '(1 . 2) 'lower-left 'angle "text" 10 #t 'both 21))
  (test-assert-thrown 'wrong-type-arg (set-text! t '(1 . 2) 'lower-left 'angle "text" 10 #t 'both 21))
  ;; Wrong string.
  (test-assert-thrown 'wrong-type-arg (make-text '(1 . 2) 'lower-left 0 'text 10 #t 'both 21))
  (test-assert-thrown 'wrong-type-arg (set-text! t '(1 . 2) 'lower-left 0 'text 10 #t 'both 21))
  ;; Wrong size.
  (test-assert-thrown 'wrong-type-arg (make-text '(1 . 2) 'lower-left 0 "text" 's #t 'both 21))
  (test-assert-thrown 'wrong-type-arg (set-text! t '(1 . 2) 'lower-left 0 "text" 's #t 'both 21))
  ;; Wrong visibility.  We cannot test it.  See comment for
  ;; set-text-visibility! above.

  ;; Wrong attribute mode.
  (test-assert-thrown 'misc-error (make-text '(1 . 2) 'lower-left 0 "text" 10 #t 'mode 21))
  (test-assert-thrown 'misc-error (set-text! t '(1 . 2) 'lower-left 0 "text" 10 #t 'mode 21))
  (test-assert-thrown 'wrong-type-arg (make-text '(1 . 2) 'lower-left 0 "text" 10 #t #t 21))
  (test-assert-thrown 'wrong-type-arg (set-text! t '(1 . 2) 'lower-left 0 "text" 10 #t #t 21))
  ;; Wrong color.
  (test-assert-thrown 'wrong-type-arg (make-text '(1 . 2) 'lower-left 0 "text" 10 #t 'both 'color))
  (test-assert-thrown 'wrong-type-arg (set-text! t '(1 . 2) 'lower-left 0 "text" 10 #t 'both 'color)))

(test-end "text-wrong-argument")

;;; Redefine the function with other coordinates.
(define (new-text)
  (make-text '(100 . 100) 'lower-left 0 "text" 10 #t 'both))

;;; Text info without unrelated colors.
(define (stripped-info text)
  (define (strip-color info)
    (reverse (cdr (reverse info))))
  (strip-color (text-info text)))

(test-begin "text-translation")

(test-equal (stripped-info (car (translate-objects! '(500 . 500) (new-text))))
  '((600 . 600) lower-left 0 "text" 10 #t both))
(test-equal (stripped-info (car (translate-objects! '(-500 . 500) (new-text))))
  '((-400 . 600) lower-left 0 "text" 10 #t both))
(test-equal (stripped-info (car (translate-objects! '(500 . -500) (new-text))))
  '((600 . -400) lower-left 0 "text" 10 #t both))
(test-equal (stripped-info (car (translate-objects! '(-500 . -500) (new-text))))
  '((-400 . -400) lower-left 0 "text" 10 #t both))

(test-end "text-translation")


(test-begin "text-mirror")

;;; Mirrorring changes text alignment.  Let's test all variants.
(let* ((x-ls '(-400 -300 -200 -100 0 100 200 300 400))
       (y-ls '(100 200 300 400 -400 -300 -200 -100 0))
       (anchors (map cons x-ls y-ls))
       (aligns '(lower-left
                 middle-left
                 upper-left
                 lower-center
                 middle-center
                 upper-center
                 lower-right
                 middle-right
                 upper-right))
       (angles '(0 90 180 270 360))
       (attribute-modes '(name value both value both name both name value))
       (str "text")
       (sizes '(5 10 15 20 25 30 35 40 45))
       (combination-func (cut list <> <> <> str <> #t <>))
       (angles '(0 90 180 270))
       (info-ls (append-map (cut map
                                 combination-func
                                 anchors
                                 aligns
                                 <>
                                 sizes
                                 attribute-modes)
                            (make-list 9 angles)))
       ;; Mirror around these coordinates.
       (x-point-1 0)
       (x-point-2 -500)
       (x-point-3 500))

  (test-equal
      (map
       (lambda (elem)
         (stripped-info (car (mirror-objects! x-point-1 (apply make-text elem)))))
       info-ls)

    '(((400 . 100) lower-right 0 "text" 5 #t name)
      ((300 . 200) middle-left 90 "text" 10 #t value)
      ((200 . 300) upper-right 180 "text" 15 #t both)
      ((100 . 400) upper-center 270 "text" 20 #t value)
      ((400 . 100) lower-right 0 "text" 5 #t name)
      ((300 . 200) middle-left 90 "text" 10 #t value)
      ((200 . 300) upper-right 180 "text" 15 #t both)
      ((100 . 400) upper-center 270 "text" 20 #t value)
      ((400 . 100) lower-right 0 "text" 5 #t name)
      ((300 . 200) middle-left 90 "text" 10 #t value)
      ((200 . 300) upper-right 180 "text" 15 #t both)
      ((100 . 400) upper-center 270 "text" 20 #t value)
      ((400 . 100) lower-right 0 "text" 5 #t name)
      ((300 . 200) middle-left 90 "text" 10 #t value)
      ((200 . 300) upper-right 180 "text" 15 #t both)
      ((100 . 400) upper-center 270 "text" 20 #t value)
      ((400 . 100) lower-right 0 "text" 5 #t name)
      ((300 . 200) middle-left 90 "text" 10 #t value)
      ((200 . 300) upper-right 180 "text" 15 #t both)
      ((100 . 400) upper-center 270 "text" 20 #t value)
      ((400 . 100) lower-right 0 "text" 5 #t name)
      ((300 . 200) middle-left 90 "text" 10 #t value)
      ((200 . 300) upper-right 180 "text" 15 #t both)
      ((100 . 400) upper-center 270 "text" 20 #t value)
      ((400 . 100) lower-right 0 "text" 5 #t name)
      ((300 . 200) middle-left 90 "text" 10 #t value)
      ((200 . 300) upper-right 180 "text" 15 #t both)
      ((100 . 400) upper-center 270 "text" 20 #t value)
      ((400 . 100) lower-right 0 "text" 5 #t name)
      ((300 . 200) middle-left 90 "text" 10 #t value)
      ((200 . 300) upper-right 180 "text" 15 #t both)
      ((100 . 400) upper-center 270 "text" 20 #t value)
      ((400 . 100) lower-right 0 "text" 5 #t name)
      ((300 . 200) middle-left 90 "text" 10 #t value)
      ((200 . 300) upper-right 180 "text" 15 #t both)
      ((100 . 400) upper-center 270 "text" 20 #t value)))

  (test-equal
      (map
       (lambda (elem)
         (stripped-info (car (mirror-objects! x-point-2 (apply make-text elem)))))
       info-ls)
    '(((-600 . 100) lower-right 0 "text" 5 #t name)
      ((-700 . 200) middle-left 90 "text" 10 #t value)
      ((-800 . 300) upper-right 180 "text" 15 #t both)
      ((-900 . 400) upper-center 270 "text" 20 #t value)
      ((-600 . 100) lower-right 0 "text" 5 #t name)
      ((-700 . 200) middle-left 90 "text" 10 #t value)
      ((-800 . 300) upper-right 180 "text" 15 #t both)
      ((-900 . 400) upper-center 270 "text" 20 #t value)
      ((-600 . 100) lower-right 0 "text" 5 #t name)
      ((-700 . 200) middle-left 90 "text" 10 #t value)
      ((-800 . 300) upper-right 180 "text" 15 #t both)
      ((-900 . 400) upper-center 270 "text" 20 #t value)
      ((-600 . 100) lower-right 0 "text" 5 #t name)
      ((-700 . 200) middle-left 90 "text" 10 #t value)
      ((-800 . 300) upper-right 180 "text" 15 #t both)
      ((-900 . 400) upper-center 270 "text" 20 #t value)
      ((-600 . 100) lower-right 0 "text" 5 #t name)
      ((-700 . 200) middle-left 90 "text" 10 #t value)
      ((-800 . 300) upper-right 180 "text" 15 #t both)
      ((-900 . 400) upper-center 270 "text" 20 #t value)
      ((-600 . 100) lower-right 0 "text" 5 #t name)
      ((-700 . 200) middle-left 90 "text" 10 #t value)
      ((-800 . 300) upper-right 180 "text" 15 #t both)
      ((-900 . 400) upper-center 270 "text" 20 #t value)
      ((-600 . 100) lower-right 0 "text" 5 #t name)
      ((-700 . 200) middle-left 90 "text" 10 #t value)
      ((-800 . 300) upper-right 180 "text" 15 #t both)
      ((-900 . 400) upper-center 270 "text" 20 #t value)
      ((-600 . 100) lower-right 0 "text" 5 #t name)
      ((-700 . 200) middle-left 90 "text" 10 #t value)
      ((-800 . 300) upper-right 180 "text" 15 #t both)
      ((-900 . 400) upper-center 270 "text" 20 #t value)
      ((-600 . 100) lower-right 0 "text" 5 #t name)
      ((-700 . 200) middle-left 90 "text" 10 #t value)
      ((-800 . 300) upper-right 180 "text" 15 #t both)
      ((-900 . 400) upper-center 270 "text" 20 #t value)))

  (test-equal
      (map
       (lambda (elem)
         (stripped-info (car (mirror-objects! x-point-3 (apply make-text elem)))))
       info-ls)

    '(((1400 . 100) lower-right 0 "text" 5 #t name)
      ((1300 . 200) middle-left 90 "text" 10 #t value)
      ((1200 . 300) upper-right 180 "text" 15 #t both)
      ((1100 . 400) upper-center 270 "text" 20 #t value)
      ((1400 . 100) lower-right 0 "text" 5 #t name)
      ((1300 . 200) middle-left 90 "text" 10 #t value)
      ((1200 . 300) upper-right 180 "text" 15 #t both)
      ((1100 . 400) upper-center 270 "text" 20 #t value)
      ((1400 . 100) lower-right 0 "text" 5 #t name)
      ((1300 . 200) middle-left 90 "text" 10 #t value)
      ((1200 . 300) upper-right 180 "text" 15 #t both)
      ((1100 . 400) upper-center 270 "text" 20 #t value)
      ((1400 . 100) lower-right 0 "text" 5 #t name)
      ((1300 . 200) middle-left 90 "text" 10 #t value)
      ((1200 . 300) upper-right 180 "text" 15 #t both)
      ((1100 . 400) upper-center 270 "text" 20 #t value)
      ((1400 . 100) lower-right 0 "text" 5 #t name)
      ((1300 . 200) middle-left 90 "text" 10 #t value)
      ((1200 . 300) upper-right 180 "text" 15 #t both)
      ((1100 . 400) upper-center 270 "text" 20 #t value)
      ((1400 . 100) lower-right 0 "text" 5 #t name)
      ((1300 . 200) middle-left 90 "text" 10 #t value)
      ((1200 . 300) upper-right 180 "text" 15 #t both)
      ((1100 . 400) upper-center 270 "text" 20 #t value)
      ((1400 . 100) lower-right 0 "text" 5 #t name)
      ((1300 . 200) middle-left 90 "text" 10 #t value)
      ((1200 . 300) upper-right 180 "text" 15 #t both)
      ((1100 . 400) upper-center 270 "text" 20 #t value)
      ((1400 . 100) lower-right 0 "text" 5 #t name)
      ((1300 . 200) middle-left 90 "text" 10 #t value)
      ((1200 . 300) upper-right 180 "text" 15 #t both)
      ((1100 . 400) upper-center 270 "text" 20 #t value)
      ((1400 . 100) lower-right 0 "text" 5 #t name)
      ((1300 . 200) middle-left 90 "text" 10 #t value)
      ((1200 . 300) upper-right 180 "text" 15 #t both)
      ((1100 . 400) upper-center 270 "text" 20 #t value)))

  ;; Double mirror around the same point returns initial result.
  (test-equal
      (map
       (lambda (elem)
         (stripped-info
          (car (mirror-objects! 500
                                (car (mirror-objects! 500
                                                      (apply make-text elem)))))))
       info-ls)

    '(((-400 . 100) lower-left 0 "text" 5 #t name)
      ((-300 . 200) middle-left 90 "text" 10 #t value)
      ((-200 . 300) upper-left 180 "text" 15 #t both)
      ((-100 . 400) lower-center 270 "text" 20 #t value)
      ((-400 . 100) lower-left 0 "text" 5 #t name)
      ((-300 . 200) middle-left 90 "text" 10 #t value)
      ((-200 . 300) upper-left 180 "text" 15 #t both)
      ((-100 . 400) lower-center 270 "text" 20 #t value)
      ((-400 . 100) lower-left 0 "text" 5 #t name)
      ((-300 . 200) middle-left 90 "text" 10 #t value)
      ((-200 . 300) upper-left 180 "text" 15 #t both)
      ((-100 . 400) lower-center 270 "text" 20 #t value)
      ((-400 . 100) lower-left 0 "text" 5 #t name)
      ((-300 . 200) middle-left 90 "text" 10 #t value)
      ((-200 . 300) upper-left 180 "text" 15 #t both)
      ((-100 . 400) lower-center 270 "text" 20 #t value)
      ((-400 . 100) lower-left 0 "text" 5 #t name)
      ((-300 . 200) middle-left 90 "text" 10 #t value)
      ((-200 . 300) upper-left 180 "text" 15 #t both)
      ((-100 . 400) lower-center 270 "text" 20 #t value)
      ((-400 . 100) lower-left 0 "text" 5 #t name)
      ((-300 . 200) middle-left 90 "text" 10 #t value)
      ((-200 . 300) upper-left 180 "text" 15 #t both)
      ((-100 . 400) lower-center 270 "text" 20 #t value)
      ((-400 . 100) lower-left 0 "text" 5 #t name)
      ((-300 . 200) middle-left 90 "text" 10 #t value)
      ((-200 . 300) upper-left 180 "text" 15 #t both)
      ((-100 . 400) lower-center 270 "text" 20 #t value)
      ((-400 . 100) lower-left 0 "text" 5 #t name)
      ((-300 . 200) middle-left 90 "text" 10 #t value)
      ((-200 . 300) upper-left 180 "text" 15 #t both)
      ((-100 . 400) lower-center 270 "text" 20 #t value)
      ((-400 . 100) lower-left 0 "text" 5 #t name)
      ((-300 . 200) middle-left 90 "text" 10 #t value)
      ((-200 . 300) upper-left 180 "text" 15 #t both)
      ((-100 . 400) lower-center 270 "text" 20 #t value))))

(test-end "text-mirror")


(test-begin "text-rotation")

(define degree-ls
  '(-900 -360 -270 -180 -90 0 90 180 270 360 900))

(define (rotate-at+500+500 angle)
  (stripped-info (car (rotate-objects! '(500 . 500) angle (new-text)))))
(define (rotate-at-500+500 angle)
  (stripped-info (car (rotate-objects! '(-500 . 500) angle (new-text)))))
(define (rotate-at+500-500 angle)
  (stripped-info (car (rotate-objects! '(500 . -500) angle (new-text)))))
(define (rotate-at-500-500 angle)
  (stripped-info (car (rotate-objects! '(-500 . -500) angle (new-text)))))

;;; The output format is
;;; '((anchor-x . anchor-y) size angle sweep-)
;;; Radius and sweep angle should never change.
(test-equal (map rotate-at+500+500 degree-ls)
  '(((900 . 900) lower-left 180 "text" 10 #t both)
    ((100 . 100) lower-left 0 "text" 10 #t both)
    ((900 . 100) lower-left 90 "text" 10 #t both)
    ((900 . 900) lower-left 180 "text" 10 #t both)
    ((100 . 900) lower-left 270 "text" 10 #t both)
    ((100 . 100) lower-left 0 "text" 10 #t both)
    ((900 . 100) lower-left 90 "text" 10 #t both)
    ((900 . 900) lower-left 180 "text" 10 #t both)
    ((100 . 900) lower-left 270 "text" 10 #t both)
    ((100 . 100) lower-left 0 "text" 10 #t both)
    ((900 . 900) lower-left 180 "text" 10 #t both)))

(test-equal (map rotate-at-500+500 degree-ls)
  '(((-1100 . 900) lower-left 180 "text" 10 #t both)
    ((100 . 100) lower-left 0 "text" 10 #t both)
    ((-100 . 1100) lower-left 90 "text" 10 #t both)
    ((-1100 . 900) lower-left 180 "text" 10 #t both)
    ((-900 . -100) lower-left 270 "text" 10 #t both)
    ((100 . 100) lower-left 0 "text" 10 #t both)
    ((-100 . 1100) lower-left 90 "text" 10 #t both)
    ((-1100 . 900) lower-left 180 "text" 10 #t both)
    ((-900 . -100) lower-left 270 "text" 10 #t both)
    ((100 . 100) lower-left 0 "text" 10 #t both)
    ((-1100 . 900) lower-left 180 "text" 10 #t both)))

(test-equal (map rotate-at+500-500 degree-ls)
  '(((900 . -1100) lower-left 180 "text" 10 #t both)
    ((100 . 100) lower-left 0 "text" 10 #t both)
    ((-100 . -900) lower-left 90 "text" 10 #t both)
    ((900 . -1100) lower-left 180 "text" 10 #t both)
    ((1100 . -100) lower-left 270 "text" 10 #t both)
    ((100 . 100) lower-left 0 "text" 10 #t both)
    ((-100 . -900) lower-left 90 "text" 10 #t both)
    ((900 . -1100) lower-left 180 "text" 10 #t both)
    ((1100 . -100) lower-left 270 "text" 10 #t both)
    ((100 . 100) lower-left 0 "text" 10 #t both)
    ((900 . -1100) lower-left 180 "text" 10 #t both)))

(test-equal (map rotate-at-500-500 degree-ls)
  '(((-1100 . -1100) lower-left 180 "text" 10 #t both)
    ((100 . 100) lower-left 0 "text" 10 #t both)
    ((-1100 . 100) lower-left 90 "text" 10 #t both)
    ((-1100 . -1100) lower-left 180 "text" 10 #t both)
    ((100 . -1100) lower-left 270 "text" 10 #t both)
    ((100 . 100) lower-left 0 "text" 10 #t both)
    ((-1100 . 100) lower-left 90 "text" 10 #t both)
    ((-1100 . -1100) lower-left 180 "text" 10 #t both)
    ((100 . -1100) lower-left 270 "text" 10 #t both)
    ((100 . 100) lower-left 0 "text" 10 #t both)
    ((-1100 . -1100) lower-left 180 "text" 10 #t both)))

;;; Invalid rotation angles, not multiple of 90 degree.
(test-assert-thrown 'misc-error (rotate-at+500+500 100))
(test-assert-thrown 'misc-error (rotate-at+500+500 -100))
(test-assert-thrown 'misc-error (rotate-at+500+500 3000))
(test-assert-thrown 'misc-error (rotate-at+500+500 -3000))

(test-end "text-rotation")
