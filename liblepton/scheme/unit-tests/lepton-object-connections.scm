;;; Test Scheme procedures for getting connections.

(use-modules (lepton object)
             (lepton page))

(test-begin "object-connection-functions")

(let ((P (make-page "/test/page/A"))
      (C (make-component "test component" '(1 . 2) 0 #t #f))
      (np (make-net-pin '(100 . 0) '(0 . 0)))
      (bp (make-bus-pin '(100 . 200) '(0 . 200)))
      (n1 (make-net '(100 . 0) '(100 . 100)))
      (n2 (make-net '(100 . 100) '(200 . 100)))
      (b1 (make-bus '(100 . 200) '(200 . 200)))
      (b2 (make-bus '(200 . 100) '(200 . 200))))

  (test-group-with-cleanup "object-connections"

    (test-assert-thrown 'object-state (object-connections np))

    ;; Build component
    (component-append! C np bp)
    (test-assert-thrown 'object-state (object-connections np))

    ;; Build page
    (page-append! P C n1 n2 b1 b2)

    ;; Test initial connections
    (test-equal (list n1 b1) (object-connections C))

    (test-equal (list n1)    (object-connections np))
    (test-equal (list np n2) (object-connections n1))
    (test-equal (list n1)    (object-connections n2))

    (test-equal (list b1)    (object-connections bp))
    (test-equal (list bp b2) (object-connections b1))
    (test-equal (list b1)    (object-connections b2))

    ;; Break some stuff
    (page-remove! P n1)
    (component-remove! C bp)

    ;; Test modified connections
    (test-equal '()            (object-connections np))
    (test-assert-thrown 'object-state (object-connections n1))
    (test-equal '()            (object-connections n2))

    (test-assert-thrown 'object-state (object-connections bp))
    (test-equal (list b2)      (object-connections b1))
    (test-equal (list b1)      (object-connections b2))

    ;; Change stuff back
    (page-append! P n1)
    (component-append! C bp)

    ;; Test modified connections
    (test-equal (list n1 b1) (object-connections C))

    (test-equal (list n1)    (object-connections np))
    (test-equal (list np n2) (object-connections n1))
    (test-equal (list n1)    (object-connections n2))

    (test-equal (list b1)    (object-connections bp))
    (test-equal (list b2 bp) (object-connections b1))
    (test-equal (list b1)    (object-connections b2))

    ;; Clean up.
    (close-page! P)))


;;; Test what happens when you connect to a net (incorrectly)
;;; placed in a component.
;;;
;;; The "right thing" is probably to be as permissive as possible.
(let ((Q (make-page "/test/page/B"))
      (C1 (make-component "test component" '(0 . 0) 0 #t #f))
      (C2 (make-component "test component" '(0 . 0) 0 #t #f))
      (p1 (make-net-pin '(100 . 0) '(0 . 0)))
      (p2 (make-net-pin '(100 . 0) '(200 . 0)))
      (n1 (make-net '(100 . 100) '(100 . 0)))
      (n2 (make-net '(100 . 100) '(0 . 100)))
      (n3 (make-net '(100 . 100) '(200 . 100)))
      (n4 (make-net '(100 . 100) '(100 . 200))))

  (test-group-with-cleanup "net-in-component-connections"

    (page-append! Q C1 C2)

    ;; Connections within the same component are fine
    (component-append! C1 n1 n2 p1)
    (test-equal (list n1) (object-connections p1))

    ;; Connections between objects in different components are only
    ;; permitted if both objects are pins.
    (component-append! C2 n3 p2)
    (test-equal (list p1) (object-connections p2))
    (test-equal '() (object-connections n3))

    ;; Connections between nets in the page and nets in components are
    ;; forbidden.
    (page-append! Q n4)
    (test-equal '() (object-connections n4))

    ;; Clean up.
    (close-page! Q)))

(test-end "object-connection-functions")


(test-begin "object-connections-wrong-argument")

(test-assert-thrown 'wrong-type-arg (object-connections 'x))

(test-end "object-connections-wrong-argument")
