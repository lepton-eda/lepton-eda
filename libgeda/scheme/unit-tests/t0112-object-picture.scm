;; Test Scheme procedures related to picture objects.

(use-modules (unit-test) (geda object))

(define test-image
  (map char->integer (string->list
"/* XPM */
static char * test_image_xpm[] = {
\"2 1 1 1\",
\" 	c None\",
\"  \"};
")))

(begin-test 'pictures
  (let* ((a (make-picture/vector test-image "test_image.xpm"
                                 '(1 . 2) '(5 . 4) 0 #f))
         (b (copy-object a)))

    (assert-equal 'picture (object-type a))
    (assert-true (picture? a))

    (assert-equal "test_image.xpm" (picture-filename a))
    (assert-equal '(1 . 4) (picture-top-left a))
    (assert-equal '(5 . 2) (picture-bottom-right a))
    (assert-equal 0 (picture-angle a))
    (assert-equal #f (picture-mirror? a))

    (assert-equal (list (picture-filename a)
                        (picture-top-left a)
                        (picture-bottom-right a)
                        (picture-angle a)
                        (picture-mirror? a))
                  (picture-info a))

    (assert-equal (picture-info a) (picture-info b))

    ;; Check setting some parameters. We simulate a rotation by 90
    ;; degrees anti-clockwise about (3,2). This doesn't test the image
    ;; dimensions snapping yet.
    (assert-equal a (set-picture! a '(2 . 0) '(4 . 4) 90 #t))
    (assert-equal '(2 . 4) (picture-top-left a))
    (assert-equal '(4 . 0) (picture-bottom-right a))
    (assert-equal 90 (picture-angle a))
    (assert-equal #t (picture-mirror? a))

    ;; Check mirroring.
    (assert-equal (list a) (mirror-objects! 4 a))
    (assert-equal '(4 . 4) (picture-top-left a))
    (assert-equal '(6 . 0) (picture-bottom-right a))
    (assert-equal 270 (picture-angle a))
    (assert-equal #f (picture-mirror? a))

    ;; Check rotating.
    (assert-equal (list a) (rotate-objects! '(5 . 2) 90 a))
    (assert-equal '(3 . 3) (picture-top-left a))
    (assert-equal '(7 . 1) (picture-bottom-right a))
    (assert-equal 0 (picture-angle a))
    (assert-equal #f (picture-mirror? a))))
