;;; Test Scheme procedures related to box objects.

(use-modules (lepton object))

(test-begin "boxes" 16)

(let* ((a (make-box '(1 . 4) '(3 . 2) 21))
       (b (copy-object a)))

  (test-equal 'box (object-type a))
  (test-assert (object-type? a 'box))
  (test-assert (not (object-type? a 'x)))

  (test-assert (box? a))
  (test-assert (box? b))

  (test-equal '(1 . 4) (box-top-left a))
  (test-equal '(3 . 2) (box-bottom-right a))
  (test-equal (box-top-left a) (box-top-left b))
  (test-equal (box-bottom-right a) (box-bottom-right b))
  (test-equal 21 (object-color a))
  (test-equal (list (box-top-left a) (box-bottom-right a) (object-color a)) (box-info a))

                                        ; Check that set-box! swaps corners around correctly
  (set-box! a '(5 . 6) '(7 . 8))
  (test-equal '(5 . 8) (box-top-left a))
  (test-equal '(7 . 6) (box-bottom-right a))
  (set-box! a '(7 . 6) '(5 . 8))
  (test-equal '(5 . 8) (box-top-left a))
  (test-equal '(7 . 6) (box-bottom-right a))
  (test-equal 21 (object-color a))

  (set-box! a '(5 . 6) '(7 . 8) 22)
  (test-equal 22 (object-color a))

  (set-object-color! a 21)
  (test-equal 21 (list-ref (box-info a) 2))
  )

(test-end "boxes")


(test-begin "box-wrong-argument")

(test-assert-thrown 'wrong-type-arg (box-info 'b))
(test-assert-thrown 'wrong-type-arg (box-top-left 'b))
(test-assert-thrown 'wrong-type-arg (box-bottom-right 'b))

(let ((b (make-box '(1 . 2) '(3 . 4) 3)))
  ;; Wrong object.
  (test-assert-thrown 'wrong-type-arg (set-box! 'b '(1 . 2) '(3 . 4) 3))
  ;; Wrong first coord.
  (test-assert-thrown 'wrong-type-arg (make-box 'c '(3 . 4) 3))
  (test-assert-thrown 'wrong-type-arg (set-box! b 'c '(3 . 4) 3))
  ;; Wrong first x.
  (test-assert-thrown 'wrong-type-arg (make-box '(x . 2) '(3 . 4) 3))
  (test-assert-thrown 'wrong-type-arg (set-box! b '(x . 2) '(3 . 4) 3))
  ;; Wrong first y.
  (test-assert-thrown 'wrong-type-arg (make-box '(1 . y) '(3 . 4) 3))
  (test-assert-thrown 'wrong-type-arg (set-box! b '(1 . y) '(3 . 4) 3))
  ;; Wrong second coord.
  (test-assert-thrown 'wrong-type-arg (make-box '(1 . 2) 'c 3))
  (test-assert-thrown 'wrong-type-arg (set-box! b '(1 . 2) 'c 3))
  ;; Wrong second x.
  (test-assert-thrown 'wrong-type-arg (make-box '(1 . 2) '(x . 4) 3))
  (test-assert-thrown 'wrong-type-arg (set-box! b '(1 . 2) '(x . 4) 3))
  ;; Wrong second y.
  (test-assert-thrown 'wrong-type-arg (make-box '(1 . 2) '(3 . y) 3))
  (test-assert-thrown 'wrong-type-arg (set-box! b '(1 . 2) '(3 . y) 3))
  ;; Wrong color.
  (test-assert-thrown 'wrong-type-arg (make-box '(1 . 2) '(3 . 4) 'color))
  (test-assert-thrown 'wrong-type-arg (set-box! b '(1 . 2) '(3 . 4) 'color)))

(test-end "box-wrong-argument")


;;; Common functions for transformations.
;;; Make the same box every time.
(define (new-box)
  (make-box '(100 . 100) '(300 . 400)))
;;; Box info without unrelated colors.
(define (stripped-info box)
  (define (strip-color info)
    (reverse (cdr (reverse info))))
  (strip-color (box-info box)))


(test-begin "box-translation")

(test-equal (stripped-info (car (translate-objects! '(500 . 500) (new-box))))
  '((600 . 900) (800 . 600)))
(test-equal (stripped-info (car (translate-objects! '(-500 . 500) (new-box))))
  '((-400 . 900) (-200 . 600)))
(test-equal (stripped-info (car (translate-objects! '(500 . -500) (new-box))))
  '((600 . -100) (800 . -400)))
(test-equal (stripped-info (car (translate-objects! '(-500 . -500) (new-box))))
  '((-400 . -100) (-200 . -400)))

(test-end "box-translation")


(test-begin "box-mirror")

(test-equal (stripped-info (car (mirror-objects! 0 (new-box))))
  '((-300 . 400) (-100 . 100)))
(test-equal (stripped-info (car (mirror-objects! 500 (new-box))))
  '((700 . 400) (900 . 100)))
(test-equal (stripped-info (car (mirror-objects! -500 (new-box))))
  '((-1300 . 400) (-1100 . 100)))
;;; Double mirror around the same point returns almost initial
;;; result (coords returned for another pair of corners).
(test-equal (stripped-info
             (car (mirror-objects! 500
                                   (car (mirror-objects! 500 (new-box))))))
  '((100 . 400) (300 . 100)))

(test-end "box-mirror")


(test-begin "box-rotation")

(define degree-ls
  '(-900 -360 -270 -180 -90 0 90 180 270 360 900))

(define (rotate-at+500+500 angle)
  (stripped-info (car (rotate-objects! '(500 . 500) angle (new-box)))))
(define (rotate-at-500+500 angle)
  (stripped-info (car (rotate-objects! '(-500 . 500) angle (new-box)))))
(define (rotate-at+500-500 angle)
  (stripped-info (car (rotate-objects! '(500 . -500) angle (new-box)))))
(define (rotate-at-500-500 angle)
  (stripped-info (car (rotate-objects! '(-500 . -500) angle (new-box)))))

;;; The output format is
;;; '((center-x . center-y) radius start-angle sweep-angle)
;;; Radius and sweep angle should never change.
(test-equal (map rotate-at+500+500 degree-ls)
  '(((700 . 900) (900 . 600))
    ((100 . 400) (300 . 100))
    ((600 . 300) (900 . 100))
    ((700 . 900) (900 . 600))
    ((100 . 900) (400 . 700))
    ((100 . 400) (300 . 100))
    ((600 . 300) (900 . 100))
    ((700 . 900) (900 . 600))
    ((100 . 900) (400 . 700))
    ((100 . 400) (300 . 100))
    ((700 . 900) (900 . 600))))

(test-equal (map rotate-at-500+500 degree-ls)
  '(((-1300 . 900) (-1100 . 600))
    ((100 . 400) (300 . 100))
    ((-400 . 1300) (-100 . 1100))
    ((-1300 . 900) (-1100 . 600))
    ((-900 . -100) (-600 . -300))
    ((100 . 400) (300 . 100))
    ((-400 . 1300) (-100 . 1100))
    ((-1300 . 900) (-1100 . 600))
    ((-900 . -100) (-600 . -300))
    ((100 . 400) (300 . 100))
    ((-1300 . 900) (-1100 . 600))))

(test-equal (map rotate-at+500-500 degree-ls)
  '(((700 . -1100) (900 . -1400))
    ((100 . 400) (300 . 100))
    ((-400 . -700) (-100 . -900))
    ((700 . -1100) (900 . -1400))
    ((1100 . -100) (1400 . -300))
    ((100 . 400) (300 . 100))
    ((-400 . -700) (-100 . -900))
    ((700 . -1100) (900 . -1400))
    ((1100 . -100) (1400 . -300))
    ((100 . 400) (300 . 100))
    ((700 . -1100) (900 . -1400))))

(test-equal (map rotate-at-500-500 degree-ls)
  '(((-1300 . -1100) (-1100 . -1400))
    ((100 . 400) (300 . 100))
    ((-1400 . 300) (-1100 . 100))
    ((-1300 . -1100) (-1100 . -1400))
    ((100 . -1100) (400 . -1300))
    ((100 . 400) (300 . 100))
    ((-1400 . 300) (-1100 . 100))
    ((-1300 . -1100) (-1100 . -1400))
    ((100 . -1100) (400 . -1300))
    ((100 . 400) (300 . 100))
    ((-1300 . -1100) (-1100 . -1400))))

;;; Invalid rotation angles, not multiple of 90 degree.
(test-assert-thrown 'misc-error (rotate-at+500+500 100))
(test-assert-thrown 'misc-error (rotate-at+500+500 -100))
(test-assert-thrown 'misc-error (rotate-at+500+500 3000))
(test-assert-thrown 'misc-error (rotate-at+500+500 -3000))

(test-end "box-rotation")
