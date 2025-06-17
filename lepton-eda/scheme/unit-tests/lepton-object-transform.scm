;;; Test Scheme procedures for transforming objects.

(use-modules (lepton object))

(test-begin "translate-objects!" 14)

(let ((C (make-component "test component" '(1 . 2) 0 #t #f))
      (a (make-line '(1 . 2) '(3 . 4)))
      (b (make-line '(1 . 2) '(3 . 4))))

  ;; Translate nothing
  (test-equal '() (translate-objects! '(1 . 2)))

  ;; Translate a line
  (test-equal (list a) (translate-objects! '(1 . 2) a))
  (test-equal '(2 . 4) (line-start a))
  (test-equal '(4 . 6) (line-end a))

  ;; Translate a component
  (component-append! C b)
  (test-equal (list C) (translate-objects! '(1 . 2) C))
  (test-equal '(2 . 4) (component-position C))
  (test-equal '(2 . 4) (line-start b))
  (test-equal '(4 . 6) (line-end b))

  ;; Translate multiple objects
  (test-equal (list a C) (translate-objects! '(-1 . -2) a C))
  (test-equal '(1 . 2) (line-start a))
  (test-equal '(3 . 4) (line-end a))
  (test-equal '(1 . 2) (component-position C))
  (test-equal '(1 . 2) (line-start b))
  (test-equal '(3 . 4) (line-end b)) )

(test-end "translate-objects!")


(test-begin "rotate-objects!" 15)

(let ((C (make-component "test component" '(1 . 2) 0 #t #f))
      (a (make-line '(1 . 2) '(3 . 4)))
      (b (make-line '(1 . 2) '(3 . 4))))

  ;; Rotate nothing
  (test-equal '() (rotate-objects! '(1 . 2) 90))

  ;; Rotate a line
  (test-equal (list a) (rotate-objects! '(1 . 2) 90 a))
  (test-equal '(1 . 2) (line-start a))
  (test-equal '(-1 . 4) (line-end a))

  ;; Rotate a component
  (component-append! C b)
  (test-equal (list C) (rotate-objects! '(1 . 2) -270 C))
  (test-equal '(1 . 2) (component-position C))
  (test-equal 90 (component-angle C))
  (test-equal '(1 . 2) (line-start b))
  (test-equal '(-1 . 4) (line-end b))

  ;; Rotate multiple objects
  (test-equal (list a C) (rotate-objects! '(1 . 2) -90 a C))
  (test-equal '(1 . 2) (line-start a))
  (test-equal '(3 . 4) (line-end a))
  (test-equal 0 (component-angle C))
  (test-equal '(1 . 2) (line-start b))
  (test-equal '(3 . 4) (line-end b)) )

(test-end "rotate-objects!")


(test-begin "mirror-objects!" 15)

(let ((C (make-component "test component" '(1 . 2) 0 #f #f))
      (a (make-line '(1 . 2) '(3 . 4)))
      (b (make-line '(1 . 2) '(3 . 4))))

  ;; Mirror nothing
  (test-equal '() (mirror-objects! 2))

  ;; Mirror a line
  (test-equal (list a) (mirror-objects! 2 a))
  (test-equal '(3 . 2) (line-start a))
  (test-equal '(1 . 4) (line-end a))

  ;; Mirror a component
  (component-append! C b)
  (test-equal (list C) (mirror-objects! 2 C))
  (test-equal '(3 . 2) (component-position C))
  (test-assert (component-mirror? C))
  (test-equal '(3 . 2) (line-start b))
  (test-equal '(1 . 4) (line-end b))

  ;; Mirror multiple objects
  (test-equal (list a C) (mirror-objects! 2 a C))
  (test-equal '(1 . 2) (line-start a))
  (test-equal '(3 . 4) (line-end a))
  (test-assert (not (component-mirror? C)))
  (test-equal '(1 . 2) (line-start b))
  (test-equal '(3 . 4) (line-end b)) )

(test-end "mirror-objects!")
