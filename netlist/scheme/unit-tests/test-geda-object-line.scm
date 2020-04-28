;;; Test Scheme procedures related to line objects.

(use-modules ((geda object) #:renamer (symbol-prefix-proc 'geda:)))


(test-begin "geda:lines" 13)

(let ((a (geda:make-line '(1 . 2) '(3 . 4) 21))
      (b (geda:make-line '(1 . 2) '(3 . 4))))

  (test-equal 'line (geda:object-type a))

  (test-assert (geda:line? a))

  (test-equal '(1 . 2) (geda:line-start a))
  (test-equal '(3 . 4) (geda:line-end a))
  (test-equal (geda:line-start a) (geda:line-start b))
  (test-equal (geda:line-end a) (geda:line-end b))
  (test-equal 21 (geda:object-color a))
  (test-equal (list (geda:line-start a) (geda:line-end a) (geda:object-color a)) (geda:line-info a))

  (geda:set-line! a '(5 . 6) '(7 . 8))
  (test-equal '(5 . 6) (geda:line-start a))
  (test-equal '(7 . 8) (geda:line-end a))
  (test-equal 21 (geda:object-color a))

  (geda:set-line! a '(5 . 6) '(7 . 8) 22)
  (test-equal 22 (geda:object-color a))

  (geda:set-object-color! a 21)
  (test-equal 21 (list-ref (geda:line-info a) 2)))

(test-end "geda:lines")


(test-begin "geda:nets" 13)

(let ((a (geda:make-net '(1 . 2) '(3 . 4) 21))
      (b (geda:make-net '(1 . 2) '(3 . 4))))

  (test-equal 'net (geda:object-type a))

  (test-assert (geda:net? a))

  (test-equal '(1 . 2) (geda:line-start a))
  (test-equal '(3 . 4) (geda:line-end a))
  (test-equal (geda:line-start a) (geda:line-start b))
  (test-equal (geda:line-end a) (geda:line-end b))
  (test-equal 21 (geda:object-color a))
  (test-equal (list (geda:line-start a) (geda:line-end a) (geda:object-color a)) (geda:line-info a))

  (geda:set-line! a '(5 . 6) '(7 . 8))
  (test-equal '(5 . 6) (geda:line-start a))
  (test-equal '(7 . 8) (geda:line-end a))
  (test-equal 21 (geda:object-color a))

  (geda:set-line! a '(5 . 6) '(7 . 8) 22)
  (test-equal 22 (geda:object-color a))

  (geda:set-object-color! a 21)
  (test-equal 21 (list-ref (geda:line-info a) 2)))

(test-end "geda:nets")


(test-begin "geda:buses" 13)

(let ((a (geda:make-bus '(1 . 2) '(3 . 4) 21))
      (b (geda:make-bus '(1 . 2) '(3 . 4))))

  (test-equal 'bus (geda:object-type a))

  (test-assert (geda:bus? a))

  (test-equal '(1 . 2) (geda:line-start a))
  (test-equal '(3 . 4) (geda:line-end a))
  (test-equal (geda:line-start a) (geda:line-start b))
  (test-equal (geda:line-end a) (geda:line-end b))
  (test-equal 21 (geda:object-color a))
  (test-equal (list (geda:line-start a) (geda:line-end a) (geda:object-color a)) (geda:line-info a))

  (geda:set-line! a '(5 . 6) '(7 . 8))
  (test-equal '(5 . 6) (geda:line-start a))
  (test-equal '(7 . 8) (geda:line-end a))
  (test-equal 21 (geda:object-color a))

  (geda:set-line! a '(5 . 6) '(7 . 8) 22)
  (test-equal 22 (geda:object-color a))

  (geda:set-object-color! a 21)
  (test-equal 21 (list-ref (geda:line-info a) 2)))

(test-end "geda:buses")


(test-begin "geda:net-pins" 15)

(let ((a (geda:make-net-pin '(1 . 2) '(3 . 4) 21))
      (b (geda:make-net-pin '(1 . 2) '(3 . 4))))

  (test-equal 'pin (geda:object-type a))

  (test-assert (geda:pin? a))
  (test-assert (geda:net-pin? a))
  (test-assert (not (geda:bus-pin? a)))

  (test-equal '(1 . 2) (geda:line-start a))
  (test-equal '(3 . 4) (geda:line-end a))
  (test-equal (geda:line-start a) (geda:line-start b))
  (test-equal (geda:line-end a) (geda:line-end b))
  (test-equal 21 (geda:object-color a))
  (test-equal (list (geda:line-start a) (geda:line-end a) (geda:object-color a)) (geda:line-info a))

  (geda:set-line! a '(5 . 6) '(7 . 8))
  (test-equal '(5 . 6) (geda:line-start a))
  (test-equal '(7 . 8) (geda:line-end a))
  (test-equal 21 (geda:object-color a))

  (geda:set-line! a '(5 . 6) '(7 . 8) 22)
  (test-equal 22 (geda:object-color a))

  (geda:set-object-color! a 21)
  (test-equal 21 (list-ref (geda:line-info a) 2)))

(test-end "geda:net-pins")


(test-begin "geda:bus-pins" 15)

(let ((a (geda:make-bus-pin '(1 . 2) '(3 . 4) 21))
      (b (geda:make-bus-pin '(1 . 2) '(3 . 4))))

  (test-equal 'pin (geda:object-type a))

  (test-assert (geda:pin? a))
  (test-assert (geda:bus-pin? a))
  (test-assert (not (geda:net-pin? a)))

  (test-equal '(1 . 2) (geda:line-start a))
  (test-equal '(3 . 4) (geda:line-end a))
  (test-equal (geda:line-start a) (geda:line-start b))
  (test-equal (geda:line-end a) (geda:line-end b))
  (test-equal 21 (geda:object-color a))
  (test-equal (list (geda:line-start a) (geda:line-end a) (geda:object-color a)) (geda:line-info a))

  (geda:set-line! a '(5 . 6) '(7 . 8))
  (test-equal '(5 . 6) (geda:line-start a))
  (test-equal '(7 . 8) (geda:line-end a))
  (test-equal 21 (geda:object-color a))

  (geda:set-line! a '(5 . 6) '(7 . 8) 22)
  (test-equal 22 (geda:object-color a))

  (geda:set-object-color! a 21)
  (test-equal 21 (list-ref (geda:line-info a) 2)))

(test-end "geda:bus-pins")
