;;; Test Scheme procedures related to arc objects.

(use-modules (lepton object))

(test-begin "arcs" 20)

(let* ((a (make-arc '(1 . 2) 3 45 90 21))
       (b (copy-object a)))

  (test-equal 'arc (object-type a))
  (test-assert (object-type? a 'arc))
  (test-assert (not (object-type? a 'x)))

  (test-assert (arc? a))
  (test-assert (arc? b))

  (test-equal '(1 . 2) (arc-center a))
  (test-equal 3 (arc-radius a))
  (test-equal 45 (arc-start-angle a))
  (test-equal 90 (arc-sweep-angle a))
  (test-equal (arc-center a) (arc-center b))
  (test-equal (arc-radius a) (arc-radius b))
  (test-equal (arc-start-angle a) (arc-start-angle b))
  (test-equal (arc-sweep-angle a) (arc-sweep-angle b))
  (test-equal 21 (object-color a))
  (test-equal (list (arc-center a) (arc-radius a)
                    (arc-start-angle a) (arc-sweep-angle a)
                    (object-color a))
    (arc-info a))

  (set-arc! a '(5 . 6) 7 180 270)
  (test-equal '(5 . 6) (arc-center a))
  (test-equal 7 (arc-radius a))
  (test-equal 180 (arc-start-angle a))
  (test-equal 270 (arc-sweep-angle a))
  (test-equal 21 (object-color a))
  (set-arc! a '(5 . 6) 7 180 270 22)
  (test-equal 22 (object-color a))

  (set-object-color! a 21)
  (test-equal 21 (list-ref (arc-info a) 4))
  )

(test-end "arcs")

(test-begin "arc-wrong-argument")

(test-assert-thrown 'wrong-type-arg (arc-info 'a))
(test-assert-thrown 'wrong-type-arg (arc-center 'a))
(test-assert-thrown 'wrong-type-arg (arc-radius 'a))
(test-assert-thrown 'wrong-type-arg (arc-start-angle 'a))
(test-assert-thrown 'wrong-type-arg (arc-sweep-angle 'a))
(test-assert-thrown 'wrong-type-arg (arc-end-angle 'a))

(let ((a (make-arc '(1 . 2) 3 45 90 3)))
  ;; Wrong object.
  (test-assert-thrown 'wrong-type-arg (set-arc! 'a '(1 . 2) 3 45 90 3))
  ;; Wrong coord.
  (test-assert-thrown 'wrong-type-arg (make-arc 'c 3 45 90 3))
  (test-assert-thrown 'wrong-type-arg (set-arc! a 'c 3 45 90 3))
  ;; Wrong x.
  (test-assert-thrown 'wrong-type-arg (make-arc '(x . 2) 3 45 90 3))
  (test-assert-thrown 'wrong-type-arg (set-arc! a '(x . 2) 3 45 90 3))
  ;; Wrong y.
  (test-assert-thrown 'wrong-type-arg (make-arc '(1 . y) 3 45 90 3))
  (test-assert-thrown 'wrong-type-arg (set-arc! a '(1 . y) 3 45 90 3))
  ;; Wrong radius.
  (test-assert-thrown 'wrong-type-arg (make-arc '(1 . 2) 'r 45 90 3))
  (test-assert-thrown 'wrong-type-arg (set-arc! a '(1 . 2) 'r 45 90 3))
  ;; Wrong start angle.
  (test-assert-thrown 'wrong-type-arg (make-arc '(1 . 2) 3 'start 90 3))
  (test-assert-thrown 'wrong-type-arg (set-arc! a '(1 . 2) 3 'start 90 3))
  ;; Wrong sweep angle.
  (test-assert-thrown 'wrong-type-arg (make-arc '(1 . 2) 3 45 'sweep 3))
  (test-assert-thrown 'wrong-type-arg (set-arc! a '(1 . 2) 3 45 'sweep 3))
  ;; Wrong color.
  (test-assert-thrown 'wrong-type-arg (make-arc '(1 . 2) 3 45 90 'color))
  (test-assert-thrown 'wrong-type-arg (set-arc! a '(1 . 2) 3 45 90 'color)))

(test-end "arc-wrong-argument")


;;; Common functions for transformations.
;;; Make the same arc every time.
(define (new-arc)
  (make-arc '(100 . 100) 200 30 50))
;;; Arc info without unrelated colors.
(define (stripped-info arc)
  (define (strip-color info)
    (reverse (cdr (reverse info))))
  (strip-color (arc-info arc)))


(test-begin "arc-translation")

(test-equal (stripped-info (car (translate-objects! '(500 . 500) (new-arc))))
  '((600 . 600) 200 30 50))
(test-equal (stripped-info (car (translate-objects! '(-500 . 500) (new-arc))))
  '((-400 . 600) 200 30 50))
(test-equal (stripped-info (car (translate-objects! '(500 . -500) (new-arc))))
  '((600 . -400) 200 30 50))
(test-equal (stripped-info (car (translate-objects! '(-500 . -500) (new-arc))))
  '((-400 . -400) 200 30 50))

(test-end "arc-translation")


(test-begin "arc-mirror")

(test-equal (stripped-info (car (mirror-objects! 0 (new-arc))))
  '((-100 . 100) 200 150 -50))
(test-equal (stripped-info (car (mirror-objects! 500 (new-arc))))
  '((900 . 100) 200 150 -50))
(test-equal (stripped-info (car (mirror-objects! -500 (new-arc))))
  '((-1100 . 100) 200 150 -50))
;;; Double mirror around the same point returns initial result.
(test-equal (stripped-info
             (car (mirror-objects! 500
                                   (car (mirror-objects! 500 (new-arc))))))
  '((100 . 100) 200 30 50))

(test-end "arc-mirror")


(test-begin "arc-rotation")

(define degree-ls
  '(-900 -360 -270 -180 -90 0 90 180 270 360 900))

(define (rotate-at+500+500 angle)
  (stripped-info (car (rotate-objects! '(500 . 500) angle (new-arc)))))
(define (rotate-at-500+500 angle)
  (stripped-info (car (rotate-objects! '(-500 . 500) angle (new-arc)))))
(define (rotate-at+500-500 angle)
  (stripped-info (car (rotate-objects! '(500 . -500) angle (new-arc)))))
(define (rotate-at-500-500 angle)
  (stripped-info (car (rotate-objects! '(-500 . -500) angle (new-arc)))))

;;; The output format is
;;; '((center-x . center-y) radius start-angle sweep-angle)
;;; Radius and sweep angle should never change.
(test-equal (map rotate-at+500+500 degree-ls)
  '(((900 . 900) 200 210 50)
    ((100 . 100) 200 30 50)
    ((900 . 100) 200 120 50)
    ((900 . 900) 200 210 50)
    ((100 . 900) 200 300 50)
    ((100 . 100) 200 30 50)
    ((900 . 100) 200 120 50)
    ((900 . 900) 200 210 50)
    ((100 . 900) 200 300 50)
    ((100 . 100) 200 30 50)
    ((900 . 900) 200 210 50)))

(test-equal (map rotate-at-500+500 degree-ls)
  '(((-1100 . 900) 200 210 50)
    ((100 . 100) 200 30 50)
    ((-100 . 1100) 200 120 50)
    ((-1100 . 900) 200 210 50)
    ((-900 . -100) 200 300 50)
    ((100 . 100) 200 30 50)
    ((-100 . 1100) 200 120 50)
    ((-1100 . 900) 200 210 50)
    ((-900 . -100) 200 300 50)
    ((100 . 100) 200 30 50)
    ((-1100 . 900) 200 210 50)))

(test-equal (map rotate-at+500-500 degree-ls)
  '(((900 . -1100) 200 210 50)
    ((100 . 100) 200 30 50)
    ((-100 . -900) 200 120 50)
    ((900 . -1100) 200 210 50)
    ((1100 . -100) 200 300 50)
    ((100 . 100) 200 30 50)
    ((-100 . -900) 200 120 50)
    ((900 . -1100) 200 210 50)
    ((1100 . -100) 200 300 50)
    ((100 . 100) 200 30 50)
    ((900 . -1100) 200 210 50)))

(test-equal (map rotate-at-500-500 degree-ls)
  '(((-1100 . -1100) 200 210 50)
    ((100 . 100) 200 30 50)
    ((-1100 . 100) 200 120 50)
    ((-1100 . -1100) 200 210 50)
    ((100 . -1100) 200 300 50)
    ((100 . 100) 200 30 50)
    ((-1100 . 100) 200 120 50)
    ((-1100 . -1100) 200 210 50)
    ((100 . -1100) 200 300 50)
    ((100 . 100) 200 30 50)
    ((-1100 . -1100) 200 210 50)))

;;; Invalid rotation angles, not multiple of 90 degree.
(test-assert-thrown 'misc-error (rotate-at+500+500 100))
(test-assert-thrown 'misc-error (rotate-at+500+500 -100))
(test-assert-thrown 'misc-error (rotate-at+500+500 3000))
(test-assert-thrown 'misc-error (rotate-at+500+500 -3000))

(test-end "arc-rotation")
