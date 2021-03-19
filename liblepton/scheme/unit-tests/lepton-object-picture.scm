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
