;;; Test Scheme procedures for transforming objects.

(use-modules ((geda object) #:renamer (symbol-prefix-proc 'geda:)))

(test-begin "geda:translate-objects!" 14)

(let ((C (geda:make-component "test component" '(1 . 2) 0 #t #f))
      (a (geda:make-line '(1 . 2) '(3 . 4)))
      (b (geda:make-line '(1 . 2) '(3 . 4))))

  ;; Translate nothing
  (test-equal '() (geda:translate-objects! '(1 . 2)))

  ;; Translate a line
  (test-equal (list a) (geda:translate-objects! '(1 . 2) a))
  (test-equal '(2 . 4) (geda:line-start a))
  (test-equal '(4 . 6) (geda:line-end a))

  ;; Translate a component
  (geda:component-append! C b)
  (test-equal (list C) (geda:translate-objects! '(1 . 2) C))
  (test-equal '(2 . 4) (geda:component-position C))
  (test-equal '(2 . 4) (geda:line-start b))
  (test-equal '(4 . 6) (geda:line-end b))

  ;; Translate multiple objects
  (test-equal (list a C) (geda:translate-objects! '(-1 . -2) a C))
  (test-equal '(1 . 2) (geda:line-start a))
  (test-equal '(3 . 4) (geda:line-end a))
  (test-equal '(1 . 2) (geda:component-position C))
  (test-equal '(1 . 2) (geda:line-start b))
  (test-equal '(3 . 4) (geda:line-end b)) )

(test-end "geda:translate-objects!")


(test-begin "geda:rotate-objects!" 15)

(let ((C (geda:make-component "test component" '(1 . 2) 0 #t #f))
      (a (geda:make-line '(1 . 2) '(3 . 4)))
      (b (geda:make-line '(1 . 2) '(3 . 4))))

  ;; Rotate nothing
  (test-equal '() (geda:rotate-objects! '(1 . 2) 90))

  ;; Rotate a line
  (test-equal (list a) (geda:rotate-objects! '(1 . 2) 90 a))
  (test-equal '(1 . 2) (geda:line-start a))
  (test-equal '(-1 . 4) (geda:line-end a))

  ;; Rotate a component
  (geda:component-append! C b)
  (test-equal (list C) (geda:rotate-objects! '(1 . 2) -270 C))
  (test-equal '(1 . 2) (geda:component-position C))
  (test-equal 90 (geda:component-angle C))
  (test-equal '(1 . 2) (geda:line-start b))
  (test-equal '(-1 . 4) (geda:line-end b))

  ;; Rotate multiple objects
  (test-equal (list a C) (geda:rotate-objects! '(1 . 2) -90 a C))
  (test-equal '(1 . 2) (geda:line-start a))
  (test-equal '(3 . 4) (geda:line-end a))
  (test-equal 0 (geda:component-angle C))
  (test-equal '(1 . 2) (geda:line-start b))
  (test-equal '(3 . 4) (geda:line-end b)) )

(test-end "geda:rotate-objects!")


(test-begin "geda:mirror-objects!" 15)

(let ((C (geda:make-component "test component" '(1 . 2) 0 #f #f))
      (a (geda:make-line '(1 . 2) '(3 . 4)))
      (b (geda:make-line '(1 . 2) '(3 . 4))))

  ;; Mirror nothing
  (test-equal '() (geda:mirror-objects! 2))

  ;; Mirror a line
  (test-equal (list a) (geda:mirror-objects! 2 a))
  (test-equal '(3 . 2) (geda:line-start a))
  (test-equal '(1 . 4) (geda:line-end a))

  ;; Mirror a component
  (geda:component-append! C b)
  (test-equal (list C) (geda:mirror-objects! 2 C))
  (test-equal '(3 . 2) (geda:component-position C))
  (test-assert (geda:component-mirror? C))
  (test-equal '(3 . 2) (geda:line-start b))
  (test-equal '(1 . 4) (geda:line-end b))

  ;; Mirror multiple objects
  (test-equal (list a C) (geda:mirror-objects! 2 a C))
  (test-equal '(1 . 2) (geda:line-start a))
  (test-equal '(3 . 4) (geda:line-end a))
  (test-assert (not (geda:component-mirror? C)))
  (test-equal '(1 . 2) (geda:line-start b))
  (test-equal '(3 . 4) (geda:line-end b)) )

(test-end "geda:mirror-objects!")
