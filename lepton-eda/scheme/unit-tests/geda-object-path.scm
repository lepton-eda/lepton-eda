;;; Test Scheme procedures related to path objects.

(use-modules ((geda object) #:renamer (symbol-prefix-proc 'geda:)))

(test-begin "geda:paths")

(let* ((a (geda:make-path))
       (b (geda:make-path 21)))

  (test-equal 'path (geda:object-type a))
  (test-assert (geda:path? a))
  (test-equal 0 (geda:path-length a))
  (test-assert-thrown 'out-of-range (geda:path-ref a 0))
  (test-assert-thrown 'out-of-range (geda:path-remove! a 0))
  (test-equal #f (geda:object-bounds a))

  ;; Most trivial path possible
  (test-equal a (geda:path-insert! a -1 'lineto '(0 . 0)))
  (test-equal 1 (geda:path-length a))
  (test-equal '(lineto (0 . 0)) (geda:path-ref a 0))
  (test-equal '((-8 . 8) . (8 . -8)) (geda:object-bounds a))

  ;; Add a move to the start
  (test-equal a (geda:path-insert! a 0 'moveto '(1 . 0)))
  (test-equal 2 (geda:path-length a))
  (test-equal '(moveto (1 . 0)) (geda:path-ref a 0))
  (test-equal '(lineto (0 . 0)) (geda:path-ref a 1))
  (test-equal '((-8 . 8) . (9 . -8)) (geda:object-bounds a))

  ;; Remove the line
  (test-equal a (geda:path-remove! a 1))
  (test-equal 1 (geda:path-length a))
  (test-assert-thrown 'out-of-range (geda:path-ref a 1))
  (test-assert-thrown 'out-of-range (geda:path-remove! a 1))
  (test-equal '((-7 . 8) . (9 . -8)) (geda:object-bounds a))

  ;; Add a line, a curve and a closepath.
  (test-equal a (geda:path-insert! a -1 'lineto '(1 . 2)))
  (test-equal '(lineto (1 . 2)) (geda:path-ref a 1))
  (test-equal a (geda:path-insert! a -1 'curveto
                                   '(3 . 4) '(5 . 6) '(7 . 8)))
  (test-equal '(curveto (3 . 4) (5 . 6) (7 . 8)) (geda:path-ref a 2))
  (test-equal a (geda:path-insert! a -1 'closepath))
  (test-equal '(closepath) (geda:path-ref a 3))

  (test-equal a (geda:path-remove! a 1))

  ;; Bad path element type
  (test-assert-thrown 'misc-error (geda:path-insert! a -1 'BAD-VALUE))

  ;; Color
  (test-equal 21 (geda:object-color b))
  )

(test-end "geda:paths")
