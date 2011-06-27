; Test Scheme procedures for getting connections.

(use-modules (unit-test))
(use-modules (geda object))
(use-modules (geda page))

(define P (make-page "/test/page/A"))

(begin-test 'object-connections
  (let ((C (make-component "test component" '(1 . 2) 0 #t #f))
        (np (make-net-pin '(100 . 0) '(0 . 0)))
        (bp (make-bus-pin '(100 . 200) '(0 . 200)))
        (n1 (make-net '(100 . 0) '(100 . 100)))
        (n2 (make-net '(100 . 100) '(200 . 100)))
        (b1 (make-bus '(100 . 200) '(200 . 200)))
        (b2 (make-bus '(200 . 100) '(200 . 200))))

    (assert-thrown 'object-state (object-connections np))

    ;; Build component
    (component-append! C np bp)
    (assert-thrown 'object-state (object-connections np))

    ;; Build page
    (page-append! P C n1 n2 b1 b2)

    ;; Test initial connections
    (assert-equal (list n1 b1) (object-connections C))

    (assert-equal (list n1)    (object-connections np))
    (assert-equal (list np n2) (object-connections n1))
    (assert-equal (list n1)    (object-connections n2))

    (assert-equal (list b1)    (object-connections bp))
    (assert-equal (list bp b2) (object-connections b1))
    (assert-equal (list b1)    (object-connections b2))

    ;; Break some stuff
    (page-remove! P n1)
    (component-remove! C bp)

    ;; Test modified connections
    (assert-equal '()            (object-connections np))
    (assert-thrown 'object-state (object-connections n1))
    (assert-equal '()            (object-connections n2))

    (assert-thrown 'object-state (object-connections bp))
    (assert-equal (list b2)      (object-connections b1))
    (assert-equal (list b1)      (object-connections b2))

    ;; Change stuff back
    (page-append! P n1)
    (component-append! C bp)

    ;; Test modified connections
    (assert-equal (list n1 b1) (object-connections C))

    (assert-equal (list n1)    (object-connections np))
    (assert-equal (list np n2) (object-connections n1))
    (assert-equal (list n1)    (object-connections n2))

    (assert-equal (list b1)    (object-connections bp))
    (assert-equal (list b2 bp) (object-connections b1))
    (assert-equal (list b1)    (object-connections b2))
    ))

(close-page! P)
