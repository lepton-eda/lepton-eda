;;; Test Scheme procedures for getting connections.

(use-modules ((geda object) #:renamer (symbol-prefix-proc 'geda:))
             (lepton page))

(test-begin "geda:object-connection-functions")

(let ((P (make-page "/test/page/A"))
      (C (geda:make-component "test component" '(1 . 2) 0 #t #f))
      (np (geda:make-net-pin '(100 . 0) '(0 . 0)))
      (bp (geda:make-bus-pin '(100 . 200) '(0 . 200)))
      (n1 (geda:make-net '(100 . 0) '(100 . 100)))
      (n2 (geda:make-net '(100 . 100) '(200 . 100)))
      (b1 (geda:make-bus '(100 . 200) '(200 . 200)))
      (b2 (geda:make-bus '(200 . 100) '(200 . 200))))

  (test-group-with-cleanup "geda:object-connections"

    (test-assert-thrown 'object-state (geda:object-connections np))

    ;; Build component
    (geda:component-append! C np bp)
    (test-assert-thrown 'object-state (geda:object-connections np))

    ;; Build page
    (page-append! P C n1 n2 b1 b2)

    ;; Test initial connections
    (test-equal (list n1 b1) (geda:object-connections C))

    (test-equal (list n1)    (geda:object-connections np))
    (test-equal (list np n2) (geda:object-connections n1))
    (test-equal (list n1)    (geda:object-connections n2))

    (test-equal (list b1)    (geda:object-connections bp))
    (test-equal (list bp b2) (geda:object-connections b1))
    (test-equal (list b1)    (geda:object-connections b2))

    ;; Break some stuff
    (page-remove! P n1)
    (geda:component-remove! C bp)

    ;; Test modified connections
    (test-equal '()            (geda:object-connections np))
    (test-assert-thrown 'object-state (geda:object-connections n1))
    (test-equal '()            (geda:object-connections n2))

    (test-assert-thrown 'object-state (geda:object-connections bp))
    (test-equal (list b2)      (geda:object-connections b1))
    (test-equal (list b1)      (geda:object-connections b2))

    ;; Change stuff back
    (page-append! P n1)
    (geda:component-append! C bp)

    ;; Test modified connections
    (test-equal (list n1 b1) (geda:object-connections C))

    (test-equal (list n1)    (geda:object-connections np))
    (test-equal (list np n2) (geda:object-connections n1))
    (test-equal (list n1)    (geda:object-connections n2))

    (test-equal (list b1)    (geda:object-connections bp))
    (test-equal (list b2 bp) (geda:object-connections b1))
    (test-equal (list b1)    (geda:object-connections b2))

    ;; Clean up.
    (close-page! P)))



;;; Test what happens when you connect to a net (incorrectly)
;;; placed in a component.
;;;
;;; The "right thing" is probably to be as permissive as possible.
(let ((Q (make-page "/test/page/B"))
      (C1 (geda:make-component "test component" '(0 . 0) 0 #t #f))
      (C2 (geda:make-component "test component" '(0 . 0) 0 #t #f))
      (p1 (geda:make-net-pin '(100 . 0) '(0 . 0)))
      (p2 (geda:make-net-pin '(100 . 0) '(200 . 0)))
      (n1 (geda:make-net '(100 . 100) '(100 . 0)))
      (n2 (geda:make-net '(100 . 100) '(0 . 100)))
      (n3 (geda:make-net '(100 . 100) '(200 . 100)))
      (n4 (geda:make-net '(100 . 100) '(100 . 200))))

  (test-group-with-cleanup "geda:net-in-component-connections"

    (page-append! Q C1 C2)

    ;; Connections within the same component are fine
    (geda:component-append! C1 n1 n2 p1)
    (test-equal (list n1) (geda:object-connections p1))

    ;; Connections between objects in different components are only
    ;; permitted if both objects are pins.
    (geda:component-append! C2 n3 p2)
    (test-equal (list p1) (geda:object-connections p2))
    (test-equal '() (geda:object-connections n3))

    ;; Connections between nets in the page and nets in components are
    ;; forbidden.
    (page-append! Q n4)
    (test-equal '() (geda:object-connections n4))

    ;; Clean up.
    (close-page! Q)))

(test-end "geda:object-connection-functions")
