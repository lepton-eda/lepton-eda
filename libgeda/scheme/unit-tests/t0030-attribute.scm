;; Test Scheme procedures related to attributes.

(use-modules (unit-test))
(use-modules (geda attrib))
(use-modules (geda page))
(use-modules (geda object))

(begin-test 'parse-attrib
  (let ((good (make-text '(1 . 2) 'lower-left 0 "name=value" 10 #t 'both))
        (bad (make-text '(1 . 2) 'lower-left 0 "name value" 10 #t 'both)))

    (assert-equal "name" (attrib-name good))
    (assert-equal "value" (attrib-value good))
    (assert-equal (cons (attrib-name good) (attrib-value good))
                  (parse-attrib good))

    (assert-true (not (parse-attrib bad)))
    (assert-true (not (attrib-name bad)))
    (assert-true (not (attrib-value bad))) ))

(begin-test 'attach-attrib
  (let ((page1 (make-page "/test/page/1"))
        (page2 (make-page "/test/page/2"))
        (comp1 (make-component "testcomponent1" '(0 . 0) 0 #f #f))
        (comp2 (make-component "testcomponent2" '(0 . 0) 0 #f #f))
        (pin (make-net-pin '(0 . 0) '(100 . 0)))
        (x (make-text '(0 . 0) 'lower-left 0 "name=x" 10 #t 'both))
        (y (make-text '(0 . 0) 'lower-left 0 "name=y" 10 #t 'both)))

    ;; This test is particularly long-winded because it tries to
    ;; exhaustively test every possible reason for attach-attrib! to
    ;; fail.

    (assert-thrown 'object-state
      (attach-attrib! pin x))

    (component-append! comp1 pin)
    (assert-thrown 'object-state
      (attach-attrib! pin x))

    (component-append! comp1 x)
    (assert-equal x (attach-attrib! pin x))
    (assert-equal (list x) (object-attribs pin))

    (assert-thrown 'object-state
      (attach-attrib! x y))

    (assert-thrown 'object-state
      (attach-attrib! y x))

    (component-append! comp2 y)
    (assert-thrown 'object-state
      (attach-attrib! pin y))

    (component-remove! comp2 y)

    (page-append! page1 comp1)
    (assert-thrown 'object-state
      (attach-attrib! comp1 y))

    (page-append! page1 y)
    (assert-thrown 'object-state
      (attach-attrib! pin y))

    (page-remove! page1 y)
    (page-append! page2 y)
    (assert-thrown 'object-state
      (attach-attrib! comp1 y))

    (page-remove! page2 y)
    (page-append! page1 y)
    (assert-equal y (attach-attrib! comp1 y))
    (assert-equal (list y) (object-attribs comp1)) ))

(begin-test 'detach-attrib
  (let ((page (make-page "/test/page/1"))
        (pin1 (make-net-pin '(0 . 0) '(100 . 0)))
        (pin2 (make-net-pin '(0 . 100) '(100 . 100)))
        (x (make-text '(0 . 0) 'lower-left 0 "name=x" 10 #t 'both)))

    (for-each (lambda (x) (page-append! page x)) (list pin1 pin2 x))

    (attach-attrib! pin1 x)

    (assert-thrown 'object-state
      (detach-attrib! pin2 x))

    (assert-equal x (detach-attrib! pin1 x))
    (assert-equal '() (object-attribs pin1)) ))
