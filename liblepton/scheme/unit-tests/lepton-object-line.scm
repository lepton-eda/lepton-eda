;;; Test Scheme procedures related to line objects.

(use-modules (lepton object))


(test-begin "lines" 13)

(let ((a (make-line '(1 . 2) '(3 . 4) 21))
      (b (make-line '(1 . 2) '(3 . 4))))

  (test-equal 'line (object-type a))
  (test-assert (object-type? a 'line))

  (test-assert (line? a))

  (test-equal '(1 . 2) (line-start a))
  (test-equal '(3 . 4) (line-end a))
  (test-equal (line-start a) (line-start b))
  (test-equal (line-end a) (line-end b))
  (test-equal 21 (object-color a))
  (test-equal (list (line-start a) (line-end a) (object-color a)) (line-info a))

  (set-line! a '(5 . 6) '(7 . 8))
  (test-equal '(5 . 6) (line-start a))
  (test-equal '(7 . 8) (line-end a))
  (test-equal 21 (object-color a))

  (set-line! a '(5 . 6) '(7 . 8) 22)
  (test-equal 22 (object-color a))

  (set-object-color! a 21)
  (test-equal 21 (list-ref (line-info a) 2)))

(test-end "lines")


(test-begin "nets" 13)

(let ((a (make-net '(1 . 2) '(3 . 4) 21))
      (b (make-net '(1 . 2) '(3 . 4))))

  (test-equal 'net (object-type a))
  (test-assert (object-type? a 'net))

  (test-assert (net? a))

  (test-equal '(1 . 2) (line-start a))
  (test-equal '(3 . 4) (line-end a))
  (test-equal (line-start a) (line-start b))
  (test-equal (line-end a) (line-end b))
  (test-equal 21 (object-color a))
  (test-equal (list (line-start a) (line-end a) (object-color a)) (line-info a))

  (set-line! a '(5 . 6) '(7 . 8))
  (test-equal '(5 . 6) (line-start a))
  (test-equal '(7 . 8) (line-end a))
  (test-equal 21 (object-color a))

  (set-line! a '(5 . 6) '(7 . 8) 22)
  (test-equal 22 (object-color a))

  (set-object-color! a 21)
  (test-equal 21 (list-ref (line-info a) 2)))

(test-end "nets")


(test-begin "buses" 13)

(let ((a (make-bus '(1 . 2) '(3 . 4) 21))
      (b (make-bus '(1 . 2) '(3 . 4))))

  (test-equal 'bus (object-type a))
  (test-assert (object-type? a 'bus))

  (test-assert (bus? a))

  (test-equal '(1 . 2) (line-start a))
  (test-equal '(3 . 4) (line-end a))
  (test-equal (line-start a) (line-start b))
  (test-equal (line-end a) (line-end b))
  (test-equal 21 (object-color a))
  (test-equal (list (line-start a) (line-end a) (object-color a)) (line-info a))

  (set-line! a '(5 . 6) '(7 . 8))
  (test-equal '(5 . 6) (line-start a))
  (test-equal '(7 . 8) (line-end a))
  (test-equal 21 (object-color a))

  (set-line! a '(5 . 6) '(7 . 8) 22)
  (test-equal 22 (object-color a))

  (set-object-color! a 21)
  (test-equal 21 (list-ref (line-info a) 2)))

(test-end "buses")


(test-begin "net-pins" 15)

(let ((a (make-net-pin '(1 . 2) '(3 . 4) 21))
      (b (make-net-pin '(1 . 2) '(3 . 4))))

  (test-equal 'pin (object-type a))
  (test-assert (object-type? a 'pin))

  (test-assert (pin? a))
  (test-assert (net-pin? a))
  (test-assert (not (bus-pin? a)))

  (test-equal '(1 . 2) (line-start a))
  (test-equal '(3 . 4) (line-end a))
  (test-equal (line-start a) (line-start b))
  (test-equal (line-end a) (line-end b))
  (test-equal 21 (object-color a))
  (test-equal (list (line-start a) (line-end a) (object-color a)) (line-info a))

  (set-line! a '(5 . 6) '(7 . 8))
  (test-equal '(5 . 6) (line-start a))
  (test-equal '(7 . 8) (line-end a))
  (test-equal 21 (object-color a))

  (set-line! a '(5 . 6) '(7 . 8) 22)
  (test-equal 22 (object-color a))

  (set-object-color! a 21)
  (test-equal 21 (list-ref (line-info a) 2)))

(test-begin "net-pins")


(test-begin "bus-pins" 15)

(let ((a (make-bus-pin '(1 . 2) '(3 . 4) 21))
      (b (make-bus-pin '(1 . 2) '(3 . 4))))

  (test-equal 'pin (object-type a))
  (test-assert (object-type? a 'pin))

  (test-assert (pin? a))
  (test-assert (bus-pin? a))
  (test-assert (not (net-pin? a)))

  (test-equal '(1 . 2) (line-start a))
  (test-equal '(3 . 4) (line-end a))
  (test-equal (line-start a) (line-start b))
  (test-equal (line-end a) (line-end b))
  (test-equal 21 (object-color a))
  (test-equal (list (line-start a) (line-end a) (object-color a)) (line-info a))

  (set-line! a '(5 . 6) '(7 . 8))
  (test-equal '(5 . 6) (line-start a))
  (test-equal '(7 . 8) (line-end a))
  (test-equal 21 (object-color a))

  (set-line! a '(5 . 6) '(7 . 8) 22)
  (test-equal 22 (object-color a))

  (set-object-color! a 21)
  (test-equal 21 (list-ref (line-info a) 2)))

(test-end "bus-pins")
