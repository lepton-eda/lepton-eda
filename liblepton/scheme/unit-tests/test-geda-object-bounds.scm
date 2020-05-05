;;; Test Scheme procedures for working with object bounds.

(use-modules ((geda object) #:renamer (symbol-prefix-proc 'geda:)))

(test-begin "geda:bounds" 7)

(let ((x (geda:make-box '(0 . 1) '(1 . 0)))
      (y (geda:make-box '(2 . 3) '(3 . 2)))
      (t (geda:make-text '(1 . 2) 'lower-left 0 "test text" 10 #t 'both))
      (C (geda:make-component "test component" '(0 . 0) 0 #t #f)))

  ;; No arguments
  (test-equal #f (geda:object-bounds))

  ;; Single argument
  (test-equal '((-5 . 6) . (6 . -5)) (geda:object-bounds x))

  ;; Multiple arguments
  (test-equal '((-5 . 8) . (8 . -5)) (geda:object-bounds x y))

  ;; We do not know the size of resulting text, so it is enough
  ;; to check that text object bounds are not #f.
  (test-assert (geda:object-bounds t))

  ;; Empty components should have no bounds...
  (test-equal '() (geda:component-contents C))
  (test-assert (not (geda:object-bounds C)))

  ;; ... but they should get bounds when you add stuff to them.
  (geda:component-append! C x)
  (test-equal '((-5 . 6) . (6 . -5)) (geda:object-bounds x))
  )

(test-end "geda:bounds")


(test-begin "geda:fold-bounds" 7)

(let ((x (geda:make-box '(0 . 1) '(1 . 0)))
      (y (geda:make-box '(2 . 3) '(3 . 2))))

  ;; No arguments
  (test-equal #f (geda:fold-bounds #f))

  ;; One argument
  (let ((a (geda:object-bounds x)))
    (test-equal a (geda:fold-bounds a))
    (test-equal #f (geda:fold-bounds #f)))

  ;; > 1 argument
  (let ((a (geda:object-bounds x))
        (b (geda:object-bounds y)))
    (test-equal '((-5 . 8) . (8 . -5))
      (geda:fold-bounds a b))
    (test-equal a (geda:fold-bounds #f a))
    (test-equal a (geda:fold-bounds a #f))
    (test-equal #f (geda:fold-bounds #f #f))))

(test-end "geda:fold-bounds")
