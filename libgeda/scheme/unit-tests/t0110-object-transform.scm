;; Test Scheme procedures for transforming objects

(use-modules (unit-test))
(use-modules (geda object))

(begin-test 'translate-objects!
  (let ((C (make-component "test component" '(1 . 2) 0 #t #f))
        (a (make-line '(1 . 2) '(3 . 4)))
        (b (make-line '(1 . 2) '(3 . 4))))

    ;; Translate nothing
    (assert-equal '() (translate-objects! '(1 . 2)))

    ;; Translate a line
    (assert-equal (list a) (translate-objects! '(1 . 2) a))
    (assert-equal '(2 . 4) (line-start a))
    (assert-equal '(4 . 6) (line-end a))

    ;; Translate a component
    (component-append! C b)
    (assert-equal (list C) (translate-objects! '(1 . 2) C))
    (assert-equal '(2 . 4) (component-position C))
    (assert-equal '(2 . 4) (line-start b))
    (assert-equal '(4 . 6) (line-end b))

    ;; Translate multiple objects
    (assert-equal (list a C) (translate-objects! '(-1 . -2) a C))
    (assert-equal '(1 . 2) (line-start a))
    (assert-equal '(3 . 4) (line-end a))
    (assert-equal '(1 . 2) (component-position C))
    (assert-equal '(1 . 2) (line-start b))
    (assert-equal '(3 . 4) (line-end b)) ))
