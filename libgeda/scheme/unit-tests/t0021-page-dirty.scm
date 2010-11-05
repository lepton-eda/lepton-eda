;; Test Scheme procedures related to pages' changed flags.

(use-modules (unit-test))
(use-modules (geda page))
(use-modules (geda object))

(begin-test 'page-dirty
  (let ((P (make-page "/test/page/A"))
        (C (make-component "test component" '(1 . 2) 0 #t #f)))

    (dynamic-wind ; Make sure pages are cleaned up
        (lambda () #f)
        (lambda ()
          (assert-true (not (page-dirty? P)))

          (set-page-dirty! P)
          (assert-true (page-dirty? P))

          (set-page-dirty! P #f)
          (assert-true (not (page-dirty? P)))

          (set-page-dirty! P #t)
          (assert-true (page-dirty? P))

          (set-page-dirty! P #f)
          (page-append! P C)
          (assert-true (page-dirty? P))

          (set-page-dirty! P #f)
          (page-remove! P C)
          (assert-true (page-dirty? P)))

        (lambda ()
          (close-page! P)))))
