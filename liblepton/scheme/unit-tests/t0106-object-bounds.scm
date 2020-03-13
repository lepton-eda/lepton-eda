;; Test Scheme procedures for working with object bounds.

(use-modules (unit-test)
             (lepton object)
             ((geda object) #:renamer (symbol-prefix-proc 'geda:)))

(begin-test 'bounds
  (let ((x (make-box '(0 . 1) '(1 . 0)))
        (y (make-box '(2 . 3) '(3 . 2)))
        (t (make-text '(1 . 2) 'lower-left 0 "test text" 10 #t 'both))
        (C (make-component "test component" '(0 . 0) 0 #t #f)))

    ;; No arguments
    (assert-equal #f (object-bounds))

    ;; Single argument
    (assert-equal '((-5 . 6) . (6 . -5)) (object-bounds x))

    ;; Multiple arguments
    (assert-equal '((-5 . 8) . (8 . -5)) (object-bounds x y))

    ;; We do not know the size of resulting text, so it is enough
    ;; to check that text object bounds are not #f.
    (assert-true (object-bounds t))

    ;; Empty components should have no bounds...
    (assert-equal '() (component-contents C))
    (assert-true (not (object-bounds C)))

    ;; ... but they should get bounds when you add stuff to them.
    (component-append! C x)
    (assert-equal '((-5 . 6) . (6 . -5)) (object-bounds x))
    ))

(begin-test 'fold-bounds
  (let ((x (make-box '(0 . 1) '(1 . 0)))
        (y (make-box '(2 . 3) '(3 . 2))))

    ;; No arguments
    (assert-equal #f (fold-bounds #f))

    ;; One argument
    (let ((a (object-bounds x)))
      (assert-equal a (fold-bounds a))
      (assert-equal #f (fold-bounds #f)))

    ;; > 1 argument
    (let ((a (object-bounds x))
          (b (object-bounds y)))
      (assert-equal '((-5 . 8) . (8 . -5))
                    (fold-bounds a b))
      (assert-equal a (fold-bounds #f a))
      (assert-equal a (fold-bounds a #f))
      (assert-equal #f (fold-bounds #f #f)))))

;;; The same tests for the deprecated (geda object) module
;;; functions.

(begin-test 'geda:bounds
  (let ((x (geda:make-box '(0 . 1) '(1 . 0)))
        (y (geda:make-box '(2 . 3) '(3 . 2)))
        (t (geda:make-text '(1 . 2) 'lower-left 0 "test text" 10 #t 'both))
        (C (geda:make-component "test component" '(0 . 0) 0 #t #f)))

    ;; No arguments
    (assert-equal #f (geda:object-bounds))

    ;; Single argument
    (assert-equal '((-5 . 6) . (6 . -5)) (geda:object-bounds x))

    ;; Multiple arguments
    (assert-equal '((-5 . 8) . (8 . -5)) (geda:object-bounds x y))

    ;; We do not know the size of resulting text, so it is enough
    ;; to check that text object bounds are not #f.
    (assert-true (geda:object-bounds t))

    ;; Empty components should have no bounds...
    (assert-equal '() (geda:component-contents C))
    (assert-true (not (geda:object-bounds C)))

    ;; ... but they should get bounds when you add stuff to them.
    (geda:component-append! C x)
    (assert-equal '((-5 . 6) . (6 . -5)) (geda:object-bounds x))
    ))

(begin-test 'geda:fold-bounds
  (let ((x (geda:make-box '(0 . 1) '(1 . 0)))
        (y (geda:make-box '(2 . 3) '(3 . 2))))

    ;; No arguments
    (assert-equal #f (geda:fold-bounds #f))

    ;; One argument
    (let ((a (geda:object-bounds x)))
      (assert-equal a (geda:fold-bounds a))
      (assert-equal #f (geda:fold-bounds #f)))

    ;; > 1 argument
    (let ((a (geda:object-bounds x))
          (b (geda:object-bounds y)))
      (assert-equal '((-5 . 8) . (8 . -5))
                    (geda:fold-bounds a b))
      (assert-equal a (geda:fold-bounds #f a))
      (assert-equal a (geda:fold-bounds a #f))
      (assert-equal #f (geda:fold-bounds #f #f)))))
