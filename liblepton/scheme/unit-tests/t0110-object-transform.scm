;; Test Scheme procedures for transforming objects

(use-modules (unit-test)
             (lepton object)
             ((geda object) #:renamer (symbol-prefix-proc 'geda:)))

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

(begin-test 'rotate-objects!
  (let ((C (make-component "test component" '(1 . 2) 0 #t #f))
        (a (make-line '(1 . 2) '(3 . 4)))
        (b (make-line '(1 . 2) '(3 . 4))))

    ;; Rotate nothing
    (assert-equal '() (rotate-objects! '(1 . 2) 90))

    ;; Rotate a line
    (assert-equal (list a) (rotate-objects! '(1 . 2) 90 a))
    (assert-equal '(1 . 2) (line-start a))
    (assert-equal '(-1 . 4) (line-end a))

    ;; Rotate a component
    (component-append! C b)
    (assert-equal (list C) (rotate-objects! '(1 . 2) -270 C))
    (assert-equal '(1 . 2) (component-position C))
    (assert-equal 90 (component-angle C))
    (assert-equal '(1 . 2) (line-start b))
    (assert-equal '(-1 . 4) (line-end b))

    ;; Rotate multiple objects
    (assert-equal (list a C) (rotate-objects! '(1 . 2) -90 a C))
    (assert-equal '(1 . 2) (line-start a))
    (assert-equal '(3 . 4) (line-end a))
    (assert-equal 0 (component-angle C))
    (assert-equal '(1 . 2) (line-start b))
    (assert-equal '(3 . 4) (line-end b)) ))

(begin-test 'mirror-objects!
  (let ((C (make-component "test component" '(1 . 2) 0 #f #f))
        (a (make-line '(1 . 2) '(3 . 4)))
        (b (make-line '(1 . 2) '(3 . 4))))

    ;; Mirror nothing
    (assert-equal '() (mirror-objects! 2))

    ;; Mirror a line
    (assert-equal (list a) (mirror-objects! 2 a))
    (assert-equal '(3 . 2) (line-start a))
    (assert-equal '(1 . 4) (line-end a))

    ;; Mirror a component
    (component-append! C b)
    (assert-equal (list C) (mirror-objects! 2 C))
    (assert-equal '(3 . 2) (component-position C))
    (assert-true (component-mirror? C))
    (assert-equal '(3 . 2) (line-start b))
    (assert-equal '(1 . 4) (line-end b))

    ;; Mirror multiple objects
    (assert-equal (list a C) (mirror-objects! 2 a C))
    (assert-equal '(1 . 2) (line-start a))
    (assert-equal '(3 . 4) (line-end a))
    (assert-true (not (component-mirror? C)))
    (assert-equal '(1 . 2) (line-start b))
    (assert-equal '(3 . 4) (line-end b)) ))

;;; The same tests for the deprecated (geda object) module
;;; functions.

(begin-test 'geda:translate-objects!
  (let ((C (geda:make-component "test component" '(1 . 2) 0 #t #f))
        (a (geda:make-line '(1 . 2) '(3 . 4)))
        (b (geda:make-line '(1 . 2) '(3 . 4))))

    ;; Translate nothing
    (assert-equal '() (geda:translate-objects! '(1 . 2)))

    ;; Translate a line
    (assert-equal (list a) (geda:translate-objects! '(1 . 2) a))
    (assert-equal '(2 . 4) (geda:line-start a))
    (assert-equal '(4 . 6) (geda:line-end a))

    ;; Translate a component
    (geda:component-append! C b)
    (assert-equal (list C) (geda:translate-objects! '(1 . 2) C))
    (assert-equal '(2 . 4) (geda:component-position C))
    (assert-equal '(2 . 4) (geda:line-start b))
    (assert-equal '(4 . 6) (geda:line-end b))

    ;; Translate multiple objects
    (assert-equal (list a C) (geda:translate-objects! '(-1 . -2) a C))
    (assert-equal '(1 . 2) (geda:line-start a))
    (assert-equal '(3 . 4) (geda:line-end a))
    (assert-equal '(1 . 2) (geda:component-position C))
    (assert-equal '(1 . 2) (geda:line-start b))
    (assert-equal '(3 . 4) (geda:line-end b)) ))

(begin-test 'geda:rotate-objects!
  (let ((C (geda:make-component "test component" '(1 . 2) 0 #t #f))
        (a (geda:make-line '(1 . 2) '(3 . 4)))
        (b (geda:make-line '(1 . 2) '(3 . 4))))

    ;; Rotate nothing
    (assert-equal '() (geda:rotate-objects! '(1 . 2) 90))

    ;; Rotate a line
    (assert-equal (list a) (geda:rotate-objects! '(1 . 2) 90 a))
    (assert-equal '(1 . 2) (geda:line-start a))
    (assert-equal '(-1 . 4) (geda:line-end a))

    ;; Rotate a component
    (geda:component-append! C b)
    (assert-equal (list C) (geda:rotate-objects! '(1 . 2) -270 C))
    (assert-equal '(1 . 2) (geda:component-position C))
    (assert-equal 90 (geda:component-angle C))
    (assert-equal '(1 . 2) (geda:line-start b))
    (assert-equal '(-1 . 4) (geda:line-end b))

    ;; Rotate multiple objects
    (assert-equal (list a C) (geda:rotate-objects! '(1 . 2) -90 a C))
    (assert-equal '(1 . 2) (geda:line-start a))
    (assert-equal '(3 . 4) (geda:line-end a))
    (assert-equal 0 (geda:component-angle C))
    (assert-equal '(1 . 2) (geda:line-start b))
    (assert-equal '(3 . 4) (geda:line-end b)) ))

(begin-test 'geda:mirror-objects!
  (let ((C (geda:make-component "test component" '(1 . 2) 0 #f #f))
        (a (geda:make-line '(1 . 2) '(3 . 4)))
        (b (geda:make-line '(1 . 2) '(3 . 4))))

    ;; Mirror nothing
    (assert-equal '() (geda:mirror-objects! 2))

    ;; Mirror a line
    (assert-equal (list a) (geda:mirror-objects! 2 a))
    (assert-equal '(3 . 2) (geda:line-start a))
    (assert-equal '(1 . 4) (geda:line-end a))

    ;; Mirror a component
    (geda:component-append! C b)
    (assert-equal (list C) (geda:mirror-objects! 2 C))
    (assert-equal '(3 . 2) (geda:component-position C))
    (assert-true (geda:component-mirror? C))
    (assert-equal '(3 . 2) (geda:line-start b))
    (assert-equal '(1 . 4) (geda:line-end b))

    ;; Mirror multiple objects
    (assert-equal (list a C) (geda:mirror-objects! 2 a C))
    (assert-equal '(1 . 2) (geda:line-start a))
    (assert-equal '(3 . 4) (geda:line-end a))
    (assert-true (not (geda:component-mirror? C)))
    (assert-equal '(1 . 2) (geda:line-start b))
    (assert-equal '(3 . 4) (geda:line-end b)) ))
