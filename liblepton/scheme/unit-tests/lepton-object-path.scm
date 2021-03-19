;;; Test Scheme procedures related to path objects.

(use-modules (lepton object))

(test-begin "paths")

(let* ((a (make-path))
       (b (make-path 21)))

  (test-equal 'path (object-type a))
  (test-assert (object-type? a 'path))
  (test-assert (not (object-type? a 'x)))
  (test-assert (path? a))
  (test-equal 0 (path-length a))
  (test-assert-thrown 'out-of-range (path-ref a 0))
  (test-assert-thrown 'out-of-range (path-remove! a 0))
  (test-equal #f (object-bounds a))

  ;; Most trivial path possible
  (test-equal a (path-insert! a -1 'lineto '(0 . 0)))
  (test-equal 1 (path-length a))
  (test-equal '(lineto (0 . 0)) (path-ref a 0))
  (test-equal '((-8 . 8) . (8 . -8)) (object-bounds a))

  ;; Add a move to the start
  (test-equal a (path-insert! a 0 'moveto '(1 . 0)))
  (test-equal 2 (path-length a))
  (test-equal '(moveto (1 . 0)) (path-ref a 0))
  (test-equal '(lineto (0 . 0)) (path-ref a 1))
  (test-equal '((-8 . 8) . (9 . -8)) (object-bounds a))

  ;; Remove the line
  (test-equal a (path-remove! a 1))
  (test-equal 1 (path-length a))
  (test-assert-thrown 'out-of-range (path-ref a 1))
  (test-assert-thrown 'out-of-range (path-remove! a 1))
  (test-equal '((-7 . 8) . (9 . -8)) (object-bounds a))

  ;; Add a line, a curve and a closepath.
  (test-equal a (path-insert! a -1 'lineto '(1 . 2)))
  (test-equal '(lineto (1 . 2)) (path-ref a 1))
  (test-equal a (path-insert! a -1 'curveto
                              '(3 . 4) '(5 . 6) '(7 . 8)))
  (test-equal '(curveto (3 . 4) (5 . 6) (7 . 8)) (path-ref a 2))
  (test-equal a (path-insert! a -1 'closepath))
  (test-equal '(closepath) (path-ref a 3))

  (test-equal a (path-remove! a 1))

  ;; Bad path element type
  (test-assert-thrown 'misc-error (path-insert! a -1 'BAD-VALUE))

  ;; Color
  (test-equal 21 (object-color b))
  )

(test-end "paths")
