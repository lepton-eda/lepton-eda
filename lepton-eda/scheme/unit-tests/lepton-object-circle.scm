;;; Test Scheme procedures related to circle objects.

(use-modules (lepton object))

(test-begin "circles" 14)

(let* ((a (make-circle '(1 . 2) 3 21))
       (b (copy-object a)))

  (test-equal 'circle (object-type a))
  (test-assert (object-type? a 'circle))
  (test-assert (not (object-type? a 'x)))

  (test-assert (circle? a))
  (test-assert (circle? b))

  (test-equal '(1 . 2) (circle-center a))
  (test-equal 3 (circle-radius a))
  (test-equal (circle-center a) (circle-center b))
  (test-equal (circle-radius a) (circle-radius b))
  (test-equal 21 (object-color a))
  (test-equal (list (circle-center a) (circle-radius a) (object-color a))
                (circle-info a))

  (set-circle! a '(5 . 6) 7)
  (test-equal '(5 . 6) (circle-center a))
  (test-equal 7 (circle-radius a))
  (test-equal 21 (object-color a))
  (set-circle! a '(5 . 6) 7 22)
  (test-equal 22 (object-color a))

  (set-object-color! a 21)
  (test-equal 21 (list-ref (circle-info a) 2)))

(test-end "circles")


(test-begin "circle-wrong-argument")

(test-assert-thrown 'wrong-type-arg (circle-info 'c))
(test-assert-thrown 'wrong-type-arg (circle-center 'c))
(test-assert-thrown 'wrong-type-arg (circle-radius 'c))

(let ((c (make-circle '(1 . 2) 3 3)))
  ;; Wrong object.
  (test-assert-thrown 'wrong-type-arg (set-circle! 'c '(1 . 2) 3 3))
  ;; Wrong coord.
  (test-assert-thrown 'wrong-type-arg (make-circle 'c 3 3))
  (test-assert-thrown 'wrong-type-arg (set-circle! c 'c 3 3))
  ;; Wrong x.
  (test-assert-thrown 'wrong-type-arg (make-circle '(x . 2) 3 3))
  (test-assert-thrown 'wrong-type-arg (set-circle! c '(x . 2) 3 3))
  ;; Wrong y.
  (test-assert-thrown 'wrong-type-arg (make-circle '(1 . y) 3 3))
  (test-assert-thrown 'wrong-type-arg (set-circle! c '(1 . y) 3 3))
  ;; Wrong radius.
  (test-assert-thrown 'wrong-type-arg (make-circle '(1 . 2) 'r 3))
  (test-assert-thrown 'wrong-type-arg (set-circle! c '(1 . 2) 'r 3))
  ;; Wrong color.
  (test-assert-thrown 'wrong-type-arg (make-circle '(1 . 2) 3 'color))
  (test-assert-thrown 'wrong-type-arg (set-circle! c '(1 . 2) 3 'color)))

(test-end "circle-wrong-argument")


;;; Common functions for transformations.
;;; Make the same circle every time.
(define (new-circle)
  (make-circle '(100 . 100) 50))
;;; Arc info without unrelated colors.
(define (stripped-info circle)
  (define (strip-color info)
    (reverse (cdr (reverse info))))
  (strip-color (circle-info circle)))


(test-begin "circle-translation")

(test-equal (stripped-info (car (translate-objects! '(500 . 500) (new-circle))))
  '((600 . 600) 50))
(test-equal (stripped-info (car (translate-objects! '(-500 . 500) (new-circle))))
  '((-400 . 600) 50))
(test-equal (stripped-info (car (translate-objects! '(500 . -500) (new-circle))))
  '((600 . -400) 50))
(test-equal (stripped-info (car (translate-objects! '(-500 . -500) (new-circle))))
  '((-400 . -400) 50))

(test-end "circle-translation")


(test-begin "circle-mirror")

(test-equal (stripped-info (car (mirror-objects! 0 (new-circle))))
  '((-100 . 100) 50))
(test-equal (stripped-info (car (mirror-objects! 500 (new-circle))))
  '((900 . 100) 50))
(test-equal (stripped-info (car (mirror-objects! -500 (new-circle))))
  '((-1100 . 100) 50))
;;; Double mirror around the same point returns initial result.
(test-equal (stripped-info
             (car (mirror-objects! 500
                                   (car (mirror-objects! 500 (new-circle))))))
  '((100 . 100) 50))

(test-end "circle-mirror")


(test-begin "circle-rotation")

(define degree-ls
  '(-900 -360 -270 -180 -90 0 90 180 270 360 900))

(define (rotate-at+500+500 angle)
  (stripped-info (car (rotate-objects! '(500 . 500) angle (new-circle)))))
(define (rotate-at-500+500 angle)
  (stripped-info (car (rotate-objects! '(-500 . 500) angle (new-circle)))))
(define (rotate-at+500-500 angle)
  (stripped-info (car (rotate-objects! '(500 . -500) angle (new-circle)))))
(define (rotate-at-500-500 angle)
  (stripped-info (car (rotate-objects! '(-500 . -500) angle (new-circle)))))

;;; The output format is
;;; '((center-x . center-y) radius start-angle sweep-angle)
;;; Radius and sweep angle should never change.
(test-equal (map rotate-at+500+500 degree-ls)
  '(((900 . 900) 50)
    ((100 . 100) 50)
    ((900 . 100) 50)
    ((900 . 900) 50)
    ((100 . 900) 50)
    ((100 . 100) 50)
    ((900 . 100) 50)
    ((900 . 900) 50)
    ((100 . 900) 50)
    ((100 . 100) 50)
    ((900 . 900) 50)))

(test-equal (map rotate-at-500+500 degree-ls)
  '(((-1100 . 900) 50)
    ((100 . 100) 50)
    ((-100 . 1100) 50)
    ((-1100 . 900) 50)
    ((-900 . -100) 50)
    ((100 . 100) 50)
    ((-100 . 1100) 50)
    ((-1100 . 900) 50)
    ((-900 . -100) 50)
    ((100 . 100) 50)
    ((-1100 . 900) 50)))

(test-equal (map rotate-at+500-500 degree-ls)
  '(((900 . -1100) 50)
    ((100 . 100) 50)
    ((-100 . -900) 50)
    ((900 . -1100) 50)
    ((1100 . -100) 50)
    ((100 . 100) 50)
    ((-100 . -900) 50)
    ((900 . -1100) 50)
    ((1100 . -100) 50)
    ((100 . 100) 50)
    ((900 . -1100) 50)))

(test-equal (map rotate-at-500-500 degree-ls)
  '(((-1100 . -1100) 50)
    ((100 . 100) 50)
    ((-1100 . 100) 50)
    ((-1100 . -1100) 50)
    ((100 . -1100) 50)
    ((100 . 100) 50)
    ((-1100 . 100) 50)
    ((-1100 . -1100) 50)
    ((100 . -1100) 50)
    ((100 . 100) 50)
    ((-1100 . -1100) 50)))

;;; Invalid rotation angles, not multiple of 90 degree.
(test-assert-thrown 'misc-error (rotate-at+500+500 100))
(test-assert-thrown 'misc-error (rotate-at+500+500 -100))
(test-assert-thrown 'misc-error (rotate-at+500+500 3000))
(test-assert-thrown 'misc-error (rotate-at+500+500 -3000))

(test-end "circle-rotation")
