;; Test deprecated procedures from legacy Scheme API.

(use-modules (unit-test))
(use-modules (geda deprecated))
(use-modules (geda object))
(use-modules (geda attrib))
(use-modules (geda page))

(begin-test 'get-attribute-name-value
  (let ((t (make-text '(1 . 2) 'lower-left 0 "name=value" 10 #t 'both)))
    (assert-equal '("name" . "value") (get-attribute-name-value t))))

(begin-test 'calcule-new-attrib-bounds
  ; Can't actually test this procedure in libgeda only, due to the
  ; absence of a function for calculating text bounds.
  #f)

(begin-test 'get-attribute-bounds
  ; Can't actually test this procedure in libgeda only, due to the
  ; absence of a function for calculating text bounds.
  #f)

(begin-test 'get-attribute-angle
  (let ((t0  (make-text '(1 . 2) 'lower-left 0 "name=value" 10 #t 'both))
        (t90 (make-text '(1 . 2) 'lower-left 90 "name=value" 10 #t 'both)))
    (assert-equal 0 (get-attribute-angle t0))
    (assert-equal 90 (get-attribute-angle t90)) ))

(begin-test 'get-object-attributes
  (let ((C (make-component "testcomponent" '(0 . 0) 0 #f #f))
        (p (make-net-pin '(0 . 0) '(100 . 0)))
        (x (make-text '(0 . 0) 'lower-left 0 "name=x" 10 #t 'both))
        (y (make-text '(0 . 0) 'lower-left 0 "name=y" 10 #t 'both)))

    (for-each (lambda (o) (component-append! C o)) (list p x y))
    (attach-attribs! p x y)

    (assert-equal (list y x) (get-object-attributes p))))

(begin-test 'get-attrib-value-by-attrib-name
  (let ((C (make-component "testcomponent" '(0 . 0) 0 #f #f))
        (p (make-net-pin '(0 . 0) '(100 . 0)))
        (x (make-text '(0 . 0) 'lower-left 0 "name=x" 10 #t 'both))
        (y (make-text '(0 . 0) 'lower-left 0 "name=y" 10 #t 'both))
        (z (make-text '(0 . 0) 'lower-left 0 "bork=z" 10 #t 'both)))

    (for-each (lambda (o) (component-append! C o)) (list p x y z))
    (attach-attribs! p x y z)

    (assert-equal (list "y" "x") (get-attrib-value-by-attrib-name p "name"))))

(begin-test 'get-object-type
  (let ((C (make-component "testcomponent" '(0 . 0) 0 #f #f))
        (p (make-net-pin '(0 . 0) '(100 . 0)))
        (t (make-text '(0 . 0) 'lower-left 0 "name=x" 10 #t 'both)))

    ; Obviously not exhaustive
    (assert-equal OBJ_COMPLEX (get-object-type C))
    (assert-equal OBJ_PIN (get-object-type p))
    (assert-equal OBJ_TEXT (get-object-type t))))

(begin-test 'get-line-width
  (let ((p (make-net-pin '(0 . 0) '(100 . 0))))

    ; This will break if you change PIN_WIDTH_NET in defines.h
    (assert-equal 10 (get-line-width p))))

(define P (make-page "/test/page/A"))

(begin-test 'get-page-filename
  (assert-equal "/test/page/A" (get-page-filename P)))

(begin-test 'set-page-filename
  (set-page-filename P "/test/page/B")
  (assert-equal "/test/page/B" (page-filename P)))

(close-page! P)
