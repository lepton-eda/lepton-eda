;;; Test Scheme procedures related to picture objects.

(use-modules ((geda object) #:renamer (symbol-prefix-proc 'geda:)))

(define test-image
  (map char->integer (string->list
"/* XPM */
static char * test_image_xpm[] = {
\"2 1 1 1\",
\" 	c None\",
\"  \"};
")))

(test-begin "geda:pictures")

(let* ((a (geda:make-picture/vector test-image "test_image.xpm"
                                    '(1 . 2) '(5 . 4) 0 #f))
       (b (geda:copy-object a)))

  (test-equal 'picture (geda:object-type a))
  (test-assert (geda:picture? a))

  (test-equal "test_image.xpm" (geda:picture-filename a))
  (test-equal '(1 . 4) (geda:picture-top-left a))
  (test-equal '(5 . 2) (geda:picture-bottom-right a))
  (test-equal 0 (geda:picture-angle a))
  (test-equal #f (geda:picture-mirror? a))

  (test-equal (list (geda:picture-filename a)
                    (geda:picture-top-left a)
                    (geda:picture-bottom-right a)
                    (geda:picture-angle a)
                    (geda:picture-mirror? a))
    (geda:picture-info a))

  (test-equal (geda:picture-info a) (geda:picture-info b))

  ;; Check setting some parameters. We simulate a rotation by 90
  ;; degrees anti-clockwise about (3,2). This doesn't test the image
  ;; dimensions snapping yet.
  (test-equal a (geda:set-picture! a '(2 . 0) '(4 . 4) 90 #t))
  (test-equal '(2 . 4) (geda:picture-top-left a))
  (test-equal '(4 . 0) (geda:picture-bottom-right a))
  (test-equal 90 (geda:picture-angle a))
  (test-equal #t (geda:picture-mirror? a))

  ;; Check mirroring.
  (test-equal (list a) (geda:mirror-objects! 4 a))
  (test-equal '(4 . 4) (geda:picture-top-left a))
  (test-equal '(6 . 0) (geda:picture-bottom-right a))
  (test-equal 270 (geda:picture-angle a))
  (test-equal #f (geda:picture-mirror? a))

  ;; Check rotating.
  (test-equal (list a) (geda:rotate-objects! '(5 . 2) 90 a))
  (test-equal '(3 . 3) (geda:picture-top-left a))
  (test-equal '(7 . 1) (geda:picture-bottom-right a))
  (test-equal 0 (geda:picture-angle a))
  (test-equal #f (geda:picture-mirror? a))

  ;; Bad angle
  (test-assert-thrown 'misc-error (geda:set-picture! a '(1 . 2) '(5 . 4) 45 #f))
  ;; Bad data
  (test-assert-thrown 'misc-error
                      (geda:make-picture/vector
                       (map char->integer (string->list "THIS IS NOT AN IMAGE"))
                       "not_an_image" '(1 . 2) '(5 . 4) 0 #f))
  )

(test-end "geda:pictures")
