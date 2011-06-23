;; Test Scheme procedures for working with object bounds.

(use-modules (unit-test))
(use-modules (geda object))

(begin-test 'bounds
  (let ((x (make-box '(0 . 1) '(1 . 0)))
        (y (make-box '(2 . 3) '(3 . 2)))
        (t (make-text '(1 . 2) 'lower-left 0 "test text" 10 #t 'both))
        (C (make-component "test component" '(0 . 0) 0 #t #f)))

    ;; No arguments
    (assert-equal #f (object-bounds))

    ;; Single argument
    (assert-equal '((0 . 1) . (1 . 0)) (object-bounds x))

    ;; Multiple arguments
    (assert-equal '((0 . 3) . (3 . 0)) (object-bounds x y))

    ;; Unfortunately, libgeda has no text renderer, so text never has
    ;; any bounds.  What a shame.
    (assert-true (not (object-bounds t)))

    ;; Empty components should have no bounds...
    (assert-equal '() (component-contents C))
    (assert-true (not (object-bounds C)))

    ;; ... but they should get bounds when you add stuff to them.
    (component-append! C x)
    (assert-equal '((0 . 1) . (1 . 0)) (object-bounds x))
    ))
