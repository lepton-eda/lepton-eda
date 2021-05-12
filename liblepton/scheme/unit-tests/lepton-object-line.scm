;;; Test Scheme procedures related to line objects.

(use-modules (srfi srfi-26)
             (lepton object))


(test-begin "lines" 13)

(let ((a (make-line '(1 . 2) '(3 . 4) 21))
      (b (make-line '(1 . 2) '(3 . 4))))

  (test-equal 'line (object-type a))
  (test-assert (object-type? a 'line))
  (test-assert (not (object-type? a 'x)))

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

(define object-func-list
  `((,(make-bus '(1 . 2) '(3 . 4)) . ,make-bus)
    (,(make-bus-pin '(1 . 2) '(3 . 4)) . ,make-bus-pin)
    (,(make-net '(1 . 2) '(3 . 4)) . ,make-net)
    (,(make-net-pin '(1 . 2) '(3 . 4)) . ,make-net-pin)
    (,(make-line '(1 . 2) '(3 . 4) 3) . ,make-line)))

(define func-list
  (map cdr object-func-list))

(test-begin "line-wrong-argument")

(test-assert-thrown 'wrong-type-arg (line-info 'l))
(test-assert-thrown 'wrong-type-arg (line-start 'l))
(test-assert-thrown 'wrong-type-arg (line-end 'l))

(for-each
 (lambda (x)
   (let ((object (car x))
         (func (cdr x)))
     ;; Wrong object.
     (test-assert-thrown 'wrong-type-arg (set-line! 'l '(1 . 2) '(3 . 4) 3))
     ;; Wrong first coord.
     (test-assert-thrown 'wrong-type-arg (func 'c '(3 . 4) 3))
     (test-assert-thrown 'wrong-type-arg (set-line! object 'c '(3 . 4) 3))
     ;; Wrong first x.
     (test-assert-thrown 'wrong-type-arg (func '(x . 2) '(3 . 4) 3))
     (test-assert-thrown 'wrong-type-arg (set-line! object '(x . 2) '(3 . 4) 3))
     ;; Wrong first y.
     (test-assert-thrown 'wrong-type-arg (func '(1 . y) '(3 . 4) 3))
     (test-assert-thrown 'wrong-type-arg (set-line! object '(1 . y) '(3 . 4) 3))
     ;; Wrong second coord.
     (test-assert-thrown 'wrong-type-arg (func '(1 . 2) 'c 3))
     (test-assert-thrown 'wrong-type-arg (set-line! object '(1 . 2) 'c 3))
     ;; Wrong second x.
     (test-assert-thrown 'wrong-type-arg (func '(1 . 2) '(x . 4) 3))
     (test-assert-thrown 'wrong-type-arg (set-line! object '(1 . 2) '(x . 4) 3))
     ;; Wrong second y.
     (test-assert-thrown 'wrong-type-arg (func '(1 . 2) '(3 . y) 3))
     (test-assert-thrown 'wrong-type-arg (set-line! object '(1 . 2) '(3 . y) 3))
     ;; Wrong color.
     (test-assert-thrown 'wrong-type-arg (func '(1 . 2) '(3 . 4) 'color))
     (test-assert-thrown 'wrong-type-arg (set-line! object '(1 . 2) '(3 . 4) 'color))))
 object-func-list)

(test-end "line-wrong-argument")


;;; Common functions for transformations.

;;; Line info without unrelated colors.
(define (stripped-info line)
  (define (strip-color info)
    (reverse (cdr (reverse info))))
  (strip-color (line-info line)))


(test-begin "line-translation")

(for-each
 (lambda (func)
   (let ((make-object (cut func '(100 . 100) '(300 . 400))))
     (test-equal (stripped-info (car (translate-objects! '(500 . 500) (make-object))))
       '((600 . 600) (800 . 900)))
     (test-equal (stripped-info (car (translate-objects! '(-500 . 500) (make-object))))
       '((-400 . 600) (-200 . 900)))
     (test-equal (stripped-info (car (translate-objects! '(500 . -500) (make-object))))
       '((600 . -400) (800 . -100)))
     (test-equal (stripped-info (car (translate-objects! '(-500 . -500) (make-object))))
       '((-400 . -400) (-200 . -100)))))
 func-list)

(test-end "line-translation")


(test-begin "line-mirror")

(for-each
 (lambda (func)
   (let ((make-object (cut func '(100 . 100) '(300 . 400))))
     (test-equal (stripped-info (car (mirror-objects! 0 (make-object))))
       '((-100 . 100) (-300 . 400)))
     (test-equal (stripped-info (car (mirror-objects! 500 (make-object))))
       '((900 . 100) (700 . 400)))
     (test-equal (stripped-info (car (mirror-objects! -500 (make-object))))
       '((-1100 . 100) (-1300 . 400)))
     ;; Double mirror around the same point returns almost initial
     ;; result (coords returned for another pair of corners).
     (test-equal (stripped-info
                  (car (mirror-objects! 500
                                        (car (mirror-objects! 500 (make-object))))))
       '((100 . 100) (300 . 400)))))
 func-list)

(test-end "line-mirror")


(test-begin "line-rotation")

(define degree-ls
  '(-900 -360 -270 -180 -90 0 90 180 270 360 900))

(for-each
 (lambda (func)
   (let ((make-object (cut func '(100 . 100) '(300 . 400))))

     (define (rotate-at+500+500 angle)
       (stripped-info (car (rotate-objects! '(500 . 500) angle (make-object)))))
     (define (rotate-at-500+500 angle)
       (stripped-info (car (rotate-objects! '(-500 . 500) angle (make-object)))))
     (define (rotate-at+500-500 angle)
       (stripped-info (car (rotate-objects! '(500 . -500) angle (make-object)))))
     (define (rotate-at-500-500 angle)
       (stripped-info (car (rotate-objects! '(-500 . -500) angle (make-object)))))

     ;; The output format is
     ;; '((center-x . center-y) radius start-angle sweep-angle)
     ;; Radius and sweep angle should never change.
     (test-equal (map rotate-at+500+500 degree-ls)
       '(((900 . 900) (700 . 600))
         ((100 . 100) (300 . 400))
         ((900 . 100) (600 . 300))
         ((900 . 900) (700 . 600))
         ((100 . 900) (400 . 700))
         ((100 . 100) (300 . 400))
         ((900 . 100) (600 . 300))
         ((900 . 900) (700 . 600))
         ((100 . 900) (400 . 700))
         ((100 . 100) (300 . 400))
         ((900 . 900) (700 . 600))))

     (test-equal (map rotate-at-500+500 degree-ls)
       '(((-1100 . 900) (-1300 . 600))
         ((100 . 100) (300 . 400))
         ((-100 . 1100) (-400 . 1300))
         ((-1100 . 900) (-1300 . 600))
         ((-900 . -100) (-600 . -300))
         ((100 . 100) (300 . 400))
         ((-100 . 1100) (-400 . 1300))
         ((-1100 . 900) (-1300 . 600))
         ((-900 . -100) (-600 . -300))
         ((100 . 100) (300 . 400))
         ((-1100 . 900) (-1300 . 600))))

     (test-equal (map rotate-at+500-500 degree-ls)
       '(((900 . -1100) (700 . -1400))
         ((100 . 100) (300 . 400))
         ((-100 . -900) (-400 . -700))
         ((900 . -1100) (700 . -1400))
         ((1100 . -100) (1400 . -300))
         ((100 . 100) (300 . 400))
         ((-100 . -900) (-400 . -700))
         ((900 . -1100) (700 . -1400))
         ((1100 . -100) (1400 . -300))
         ((100 . 100) (300 . 400))
         ((900 . -1100) (700 . -1400))))

     (test-equal (map rotate-at-500-500 degree-ls)
       '(((-1100 . -1100) (-1300 . -1400))
         ((100 . 100) (300 . 400))
         ((-1100 . 100) (-1400 . 300))
         ((-1100 . -1100) (-1300 . -1400))
         ((100 . -1100) (400 . -1300))
         ((100 . 100) (300 . 400))
         ((-1100 . 100) (-1400 . 300))
         ((-1100 . -1100) (-1300 . -1400))
         ((100 . -1100) (400 . -1300))
         ((100 . 100) (300 . 400))
         ((-1100 . -1100) (-1300 . -1400))))

     ;; Invalid rotation angles, not multiple of 90 degree.
     (test-assert-thrown 'misc-error (rotate-at+500+500 100))
     (test-assert-thrown 'misc-error (rotate-at+500+500 -100))
     (test-assert-thrown 'misc-error (rotate-at+500+500 3000))
     (test-assert-thrown 'misc-error (rotate-at+500+500 -3000))
     ;; End of let.
     ))
 func-list)

(test-end "line-rotation")


(test-begin "nets" 13)

(let ((a (make-net '(1 . 2) '(3 . 4) 21))
      (b (make-net '(1 . 2) '(3 . 4))))

  (test-equal 'net (object-type a))
  (test-assert (object-type? a 'net))
  (test-assert (not (object-type? a 'x)))

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
  (test-assert (not (object-type? a 'x)))

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
  (test-assert (not (object-type? a 'x)))

  (test-assert (pin? a))
  (test-assert (net-pin? a))
  (test-assert (not (bus-pin? a)))

  (test-assert (not (net-pin? 'x)))

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
  (test-assert (not (object-type? a 'x)))

  (test-assert (pin? a))
  (test-assert (bus-pin? a))
  (test-assert (not (net-pin? a)))

  (test-assert (not (bus-pin? 'x)))

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
