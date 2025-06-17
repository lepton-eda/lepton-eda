;;; Test Scheme procedures for working with object bounds.

(use-modules (lepton object))

(test-begin "bounds" 7)

(let ((x (make-box '(0 . 1) '(1 . 0)))
      (y (make-box '(2 . 3) '(3 . 2)))
      (t (make-text '(1 . 2) 'lower-left 0 "test text" 10 #t 'both))
      (C (make-component "test component" '(0 . 0) 0 #t #f)))

  ;; No arguments
  (test-equal #f (object-bounds))

  ;; Single argument
  (test-equal '((-5 . 6) . (6 . -5)) (object-bounds x))

  ;; Multiple arguments
  (test-equal '((-5 . 8) . (8 . -5)) (object-bounds x y))

  ;; We do not know the size of resulting text, so it is enough
  ;; to check that text object bounds are not #f.
  (test-assert (object-bounds t))

  ;; Empty components should have no bounds...
  (test-equal '() (component-contents C))
  (test-assert (not (object-bounds C)))

  ;; ... but they should get bounds when you add stuff to them.
  (component-append! C x)
  (test-equal '((-5 . 6) . (6 . -5)) (object-bounds x))
  )

(test-end "bounds")

(test-begin "fold-bounds" 7)

(let ((x (make-box '(0 . 1) '(1 . 0)))
      (y (make-box '(2 . 3) '(3 . 2))))

  ;; No arguments
  (test-equal #f (fold-bounds #f))

  ;; One argument
  (let ((a (object-bounds x)))
    (test-equal a (fold-bounds a))
    (test-equal #f (fold-bounds #f)))

  ;; > 1 argument
  (let ((a (object-bounds x))
        (b (object-bounds y)))
    (test-equal '((-5 . 8) . (8 . -5))
      (fold-bounds a b))
    (test-equal a (fold-bounds #f a))
    (test-equal a (fold-bounds a #f))
    (test-equal #f (fold-bounds #f #f))))

(test-end "fold-bounds")


(test-begin "object-bounds-wrong-argument")

(test-assert-thrown 'wrong-type-arg (object-bounds 'a))

(test-end "object-bounds-wrong-argument")
