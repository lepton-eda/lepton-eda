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

(define Q (make-page "/test/page/B"))

;; Test what happens when you connect to a net (incorrectly) placed in
;; a component.
;;
;; The "right thing" is probably to be as permissive as possible.
(begin-test 'net-in-component-connections
  (let ((C1 (make-component "test component" '(0 . 0) 0 #t #f))
        (C2 (make-component "test component" '(0 . 0) 0 #t #f))
        (p1 (make-net-pin '(100 . 0) '(0 . 0)))
        (p2 (make-net-pin '(100 . 0) '(200 . 0)))
        (n1 (make-net '(100 . 100) '(100 . 0)))
        (n2 (make-net '(100 . 100) '(0 . 100)))
        (n3 (make-net '(100 . 100) '(200 . 100)))
        (n4 (make-net '(100 . 100) '(100 . 200))))

    (page-append! Q C1 C2)

    ;; Connections within the same component are fine
    (component-append! C1 n1 n2 p1)
    (assert-equal (list n1) (object-connections p1))

    ;; Connections between objects in different components are only
    ;; permitted if both objects are pins.
    (component-append! C2 n3 p2)
    (assert-equal (list p1) (object-connections p2))
    (assert-equal '() (object-connections n3))

    ;; Connections between nets in the page and nets in components are
    ;; forbidden.
    (page-append! Q n4)
    (assert-equal '() (object-connections n4))))

(close-page! Q)
