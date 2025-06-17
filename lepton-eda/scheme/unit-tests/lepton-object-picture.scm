;;; Test Scheme procedures related to picture objects.

(use-modules (lepton object))

(define test-image
  (map char->integer (string->list
"/* XPM */
static char * test_image_xpm[] = {
\"2 1 1 1\",
\"      c None\",
\"  \"};
")))

(test-begin "pictures")

(let* ((a (make-picture/vector test-image "test_image.xpm"
                               '(1 . 2) '(5 . 4) 0 #f))
       (b (copy-object a)))

  (test-equal 'picture (object-type a))
  (test-assert (object-type? a 'picture))
  (test-assert (not (object-type? a 'x)))
  (test-assert (picture? a))

  (test-equal "test_image.xpm" (picture-filename a))
  (test-equal '(1 . 4) (picture-top-left a))
  (test-equal '(5 . 2) (picture-bottom-right a))
  (test-equal 0 (picture-angle a))
  (test-equal #f (picture-mirror? a))

  (test-equal (list (picture-filename a)
                    (picture-top-left a)
                    (picture-bottom-right a)
                    (picture-angle a)
                    (picture-mirror? a))
    (picture-info a))

  (test-equal (picture-info a) (picture-info b))

  ;; Check setting some parameters. We simulate a rotation by 90
  ;; degrees anti-clockwise about (3,2). This doesn't test the image
  ;; dimensions snapping yet.
  (test-equal a (set-picture! a '(2 . 0) '(4 . 4) 90 #t))
  (test-equal '(2 . 4) (picture-top-left a))
  (test-equal '(4 . 0) (picture-bottom-right a))
  (test-equal 90 (picture-angle a))
  (test-equal #t (picture-mirror? a))

  ;; Check mirroring.
  (test-equal (list a) (mirror-objects! 4 a))
  (test-equal '(4 . 4) (picture-top-left a))
  (test-equal '(6 . 0) (picture-bottom-right a))
  (test-equal 270 (picture-angle a))
  (test-equal #f (picture-mirror? a))

  ;; Check rotating.
  (test-equal (list a) (rotate-objects! '(5 . 2) 90 a))
  (test-equal '(3 . 3) (picture-top-left a))
  (test-equal '(7 . 1) (picture-bottom-right a))
  (test-equal 0 (picture-angle a))
  (test-equal #f (picture-mirror? a))

  ;; Bad angle
  (test-assert-thrown 'misc-error (set-picture! a '(1 . 2) '(5 . 4) 45 #f))
  ;; Bad data
  (test-assert-thrown 'misc-error
                      (make-picture/vector
                       (map char->integer (string->list "THIS IS NOT AN IMAGE"))
                       "not_an_image" '(1 . 2) '(5 . 4) 0 #f))
  )

(test-end "pictures")


(test-begin "picture-wrong-argument")

(test-assert-thrown 'wrong-type-arg (picture-info 'pic))
(test-assert-thrown 'wrong-type-arg (picture-filename 'pic))
(test-assert-thrown 'wrong-type-arg (picture-top-left 'pic))
(test-assert-thrown 'wrong-type-arg (picture-bottom-right 'pic))
(test-assert-thrown 'wrong-type-arg (picture-angle 'pic))
(test-assert-thrown 'wrong-type-arg (picture-mirror? 'pic))

(let ((pic (make-picture/vector test-image "test_image.xpm" '(1 . 2) '(3 . 4) 0 #f)))
  ;; Wrong object.
  (test-assert-thrown 'wrong-type-arg (set-picture! 'pic '(1 . 2) '(3 . 4) 0 #f))
  ;; Wrong image.
  (test-assert-thrown 'wrong-type-arg (make-picture/vector 'test-image "test_image.xpm" 'c '(3 . 4) 0 #f))
  ;; Wrong filename.
  (test-assert-thrown 'wrong-type-arg (make-picture/vector test-image 'test_image.xpm 'c '(3 . 4) 0 #f))
  ;; Wrong first coord.
  (test-assert-thrown 'wrong-type-arg (make-picture/vector test-image "test_image.xpm" 'c '(3 . 4) 0 #f))
  (test-assert-thrown 'wrong-type-arg (set-picture! pic 'c '(3 . 4) 0 #f))
  ;; Wrong first x.
  (test-assert-thrown 'wrong-type-arg (make-picture/vector test-image "test_image.xpm" '(x . 2) '(3 . 4) 0 #f))
  (test-assert-thrown 'wrong-type-arg (set-picture! pic '(x . 2) '(3 . 4) 0 #f))
  ;; Wrong first y.
  (test-assert-thrown 'wrong-type-arg (make-picture/vector test-image "test_image.xpm" '(1 . y) '(3 . 4) 0 #f))
  (test-assert-thrown 'wrong-type-arg (set-picture! pic '(1 . y) '(3 . 4) 0 #f))
  ;; Wrong second coord.
  (test-assert-thrown 'wrong-type-arg (make-picture/vector test-image "test_image.xpm" '(1 . 2) 'c 0 #f))
  (test-assert-thrown 'wrong-type-arg (set-picture! pic '(1 . 2) 'c 0 #f))
  ;; Wrong second x.
  (test-assert-thrown 'wrong-type-arg (make-picture/vector test-image "test_image.xpm" '(1 . 2) '(x . 4) 0 #f))
  (test-assert-thrown 'wrong-type-arg (set-picture! pic '(1 . 2) '(x . 4) 0 #f))
  ;; Wrong second y.
  (test-assert-thrown 'wrong-type-arg (make-picture/vector test-image "test_image.xpm" '(1 . 2) '(3 . y) 0 #f))
  (test-assert-thrown 'wrong-type-arg (set-picture! pic '(1 . 2) '(3 . y) 0 #f))
  ;; Wrong angle.
  (test-assert-thrown 'misc-error (make-picture/vector test-image "test_image.xpm" '(1 . 2) '(3 . 4) 360 #f))
  (test-assert-thrown 'misc-error (set-picture! pic '(1 . 2) '(3 . 4) 360 #f))
  (test-assert-thrown 'wrong-type-arg (make-picture/vector test-image "test_image.xpm" '(1 . 2) '(3 . 4) 'angle #f))
  (test-assert-thrown 'wrong-type-arg (set-picture! pic '(1 . 2) '(3 . 4) 'angle #f))
  ;; Mirror flag cannot be wrong, so there is no test for it.
)

(test-end "picture-wrong-argument")


;;; Common functions for transformations.
;;; Make the same picture every time.
(define (new-picture)
  (make-picture/vector test-image "test_image.xpm" '(100 . 400) '(300 . 200) 0 #f))
(define (stripped-info picture)
  ;; Don't take filename into account.
  (cdr (picture-info picture)))


(test-begin "picture-translation")

(test-equal (stripped-info (car (translate-objects! '(500 . 500) (new-picture))))
  '((600 . 900) (800 . 700) 0 #f))
(test-equal (stripped-info (car (translate-objects! '(-500 . 500) (new-picture))))
  '((-400 . 900) (-200 . 700) 0 #f))
(test-equal (stripped-info (car (translate-objects! '(500 . -500) (new-picture))))
  '((600 . -100) (800 . -300) 0 #f))
(test-equal (stripped-info (car (translate-objects! '(-500 . -500) (new-picture))))
  '((-400 . -100) (-200 . -300) 0 #f))

(test-end "picture-translation")


(test-begin "picture-mirror")

(test-equal (stripped-info (car (mirror-objects! 0 (new-picture))))
  '((-300 . 400) (-100 . 200) 0 #t))
(test-equal (stripped-info (car (mirror-objects! 500 (new-picture))))
  '((700 . 400) (900 . 200) 0 #t))
(test-equal (stripped-info (car (mirror-objects! -500 (new-picture))))
  '((-1300 . 400) (-1100 . 200) 0 #t))
;;; Double mirror around the same point returns almost initial
;;; result (coords returned for another pair of corners).
(test-equal (stripped-info
             (car (mirror-objects! 500
                                   (car (mirror-objects! 500 (new-picture))))))
  '((100 . 400) (300 . 200) 0 #f))

(test-end "picture-mirror")


(test-begin "picture-rotation")

(define degree-ls
  '(-900 -360 -270 -180 -90 0 90 180 270 360 900))

(define (rotate-at+500+500 angle)
  (stripped-info (car (rotate-objects! '(500 . 500) angle (new-picture)))))
(define (rotate-at-500+500 angle)
  (stripped-info (car (rotate-objects! '(-500 . 500) angle (new-picture)))))
(define (rotate-at+500-500 angle)
  (stripped-info (car (rotate-objects! '(500 . -500) angle (new-picture)))))
(define (rotate-at-500-500 angle)
  (stripped-info (car (rotate-objects! '(-500 . -500) angle (new-picture)))))

;;; The output format is
;;; '((center-x . center-y) radius start-angle sweep-angle)
;;; Radius and sweep angle should never change.
(test-equal (map rotate-at+500+500 degree-ls)
  '(((700 . 800) (900 . 600) 180 #f)
    ((100 . 400) (300 . 200) 0 #f)
    ((600 . 300) (800 . 100) 90 #f)
    ((700 . 800) (900 . 600) 180 #f)
    ((200 . 900) (400 . 700) 270 #f)
    ((100 . 400) (300 . 200) 0 #f)
    ((600 . 300) (800 . 100) 90 #f)
    ((700 . 800) (900 . 600) 180 #f)
    ((200 . 900) (400 . 700) 270 #f)
    ((100 . 400) (300 . 200) 0 #f)
    ((700 . 800) (900 . 600) 180 #f)))

(test-equal (map rotate-at-500+500 degree-ls)
  '(((-1300 . 800) (-1100 . 600) 180 #f)
    ((100 . 400) (300 . 200) 0 #f)
    ((-400 . 1300) (-200 . 1100) 90 #f)
    ((-1300 . 800) (-1100 . 600) 180 #f)
    ((-800 . -100) (-600 . -300) 270 #f)
    ((100 . 400) (300 . 200) 0 #f)
    ((-400 . 1300) (-200 . 1100) 90 #f)
    ((-1300 . 800) (-1100 . 600) 180 #f)
    ((-800 . -100) (-600 . -300) 270 #f)
    ((100 . 400) (300 . 200) 0 #f)
    ((-1300 . 800) (-1100 . 600) 180 #f)))

(test-equal (map rotate-at+500-500 degree-ls)
  '(((700 . -1200) (900 . -1400) 180 #f)
    ((100 . 400) (300 . 200) 0 #f)
    ((-400 . -700) (-200 . -900) 90 #f)
    ((700 . -1200) (900 . -1400) 180 #f)
    ((1200 . -100) (1400 . -300) 270 #f)
    ((100 . 400) (300 . 200) 0 #f)
    ((-400 . -700) (-200 . -900) 90 #f)
    ((700 . -1200) (900 . -1400) 180 #f)
    ((1200 . -100) (1400 . -300) 270 #f)
    ((100 . 400) (300 . 200) 0 #f)
    ((700 . -1200) (900 . -1400) 180 #f)))

(test-equal (map rotate-at-500-500 degree-ls)
  '(((-1300 . -1200) (-1100 . -1400) 180 #f)
    ((100 . 400) (300 . 200) 0 #f)
    ((-1400 . 300) (-1200 . 100) 90 #f)
    ((-1300 . -1200) (-1100 . -1400) 180 #f)
    ((200 . -1100) (400 . -1300) 270 #f)
    ((100 . 400) (300 . 200) 0 #f)
    ((-1400 . 300) (-1200 . 100) 90 #f)
    ((-1300 . -1200) (-1100 . -1400) 180 #f)
    ((200 . -1100) (400 . -1300) 270 #f)
    ((100 . 400) (300 . 200) 0 #f)
    ((-1300 . -1200) (-1100 . -1400) 180 #f)))

;;; Invalid rotation angles, not multiple of 90 degree.
(test-assert-thrown 'misc-error (rotate-at+500+500 100))
(test-assert-thrown 'misc-error (rotate-at+500+500 -100))
(test-assert-thrown 'misc-error (rotate-at+500+500 3000))
(test-assert-thrown 'misc-error (rotate-at+500+500 -3000))

(test-end "picture-rotation")
