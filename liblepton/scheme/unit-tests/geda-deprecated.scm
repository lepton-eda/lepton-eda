;;; Test deprecated procedures from legacy Scheme API.

(use-modules (geda deprecated)
             (lepton attrib)
             (lepton object)
             (lepton page))

(test-begin "get-attribute-name-value")

(let ((t (make-text '(1 . 2) 'lower-left 0 "name=value" 10 #t 'both)))
  (test-equal '("name" . "value") (get-attribute-name-value t)))

(test-end "get-attribute-name-value")


(test-begin "calcule-new-attrib-bounds"
  ;; Can't actually test this procedure in libgeda only, due to
  ;; the absence of a function for calculating text bounds.
  #f)

(test-begin "get-attribute-bounds"
  ;; Can't actually test this procedure in libgeda only, due to
  ;; the absence of a function for calculating text bounds.
  #f)


(test-begin "get-attribute-angle")

(let ((t0  (make-text '(1 . 2) 'lower-left 0 "name=value" 10 #t 'both))
      (t90 (make-text '(1 . 2) 'lower-left 90 "name=value" 10 #t 'both)))
  (test-equal 0 (get-attribute-angle t0))
  (test-equal 90 (get-attribute-angle t90)) )

(test-end "get-attribute-angle")


(test-begin "get-object-attributes")

(let ((C (make-component "testcomponent" '(0 . 0) 0 #f #f))
      (p (make-net-pin '(0 . 0) '(100 . 0)))
      (x (make-text '(0 . 0) 'lower-left 0 "name=x" 10 #t 'both))
      (y (make-text '(0 . 0) 'lower-left 0 "name=y" 10 #t 'both)))

  (for-each (lambda (o) (component-append! C o)) (list p x y))
  (attach-attribs! p x y)

  (test-equal (list y x) (get-object-attributes p)))

(test-end "get-object-attributes")


(test-begin "get-attrib-value-by-attrib-name")

(let ((C (make-component "testcomponent" '(0 . 0) 0 #f #f))
      (p (make-net-pin '(0 . 0) '(100 . 0)))
      (x (make-text '(0 . 0) 'lower-left 0 "name=x" 10 #t 'both))
      (y (make-text '(0 . 0) 'lower-left 0 "name=y" 10 #t 'both))
      (z (make-text '(0 . 0) 'lower-left 0 "bork=z" 10 #t 'both)))

  (for-each (lambda (o) (component-append! C o)) (list p x y z))
  (attach-attribs! p x y z)

  (test-equal (list "y" "x") (get-attrib-value-by-attrib-name p "name"))

  ;; Attributes with empty value are no longer invalid.
  (set-text! y '(0 . 0) 'lower-left 0 "name=" 10 #t 'both)
  (test-equal (list "" "x") (get-attrib-value-by-attrib-name p "name")))

(test-end "get-attrib-value-by-attrib-name")


(test-begin "get-object-type")

(let ((C (make-component "testcomponent" '(0 . 0) 0 #f #f))
      (p (make-net-pin '(0 . 0) '(100 . 0)))
      (t (make-text '(0 . 0) 'lower-left 0 "name=x" 10 #t 'both)))

  ;; Obviously not exhaustive
  (test-equal OBJ_COMPLEX (get-object-type C))
  (test-equal OBJ_PIN (get-object-type p))
  (test-equal OBJ_TEXT (get-object-type t)))

(test-end "get-object-type")


(test-begin "get-line-width")

(let ((p (make-net-pin '(0 . 0) '(100 . 0))))

  ;; This will break if you change PIN_WIDTH_NET in defines.h
  (test-equal 10 (get-line-width p)))

(test-end "get-line-width")


(define P (make-page "/test/page/A"))


(test-begin "get-page-filename")

(test-equal "/test/page/A" (get-page-filename P))

(test-end "get-page-filename")


(test-begin "set-page-filename")

(set-page-filename P "/test/page/B")
(test-equal "/test/page/B" (page-filename P))

(test-end "set-page-filename")

(close-page! P)
