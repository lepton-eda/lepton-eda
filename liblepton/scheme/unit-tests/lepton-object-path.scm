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

(define (new-path)
  (let ((p (make-path)))
    (path-insert! p -1 'moveto '(-200 . -200))
    (path-insert! p -1 'lineto '(-200 . 400))
    (path-insert! p -1 'lineto '(200 . 400))
    (path-insert! p -1 'curveto '(200 . 400) '(500 . 400) '(500 . 100))
    (path-insert! p -1 'curveto '(500 . -200) '(200 . -200) '(200 . -200))
    (path-insert! p -1 'closepath)
    p))


(test-begin "path-wrong-argument")

;;; Wrong color in make-path.
(test-assert-thrown 'wrong-type-arg (make-path 'color))

;;; path-info
(test-assert-thrown 'wrong-type-arg (path-info 'p))

;;; path-length
(test-assert-thrown 'wrong-type-arg (path-length 'p))

;;; path-ref
(test-assert-thrown 'wrong-type-arg (path-ref 'p 0))
(test-assert-thrown 'wrong-type-arg (path-ref (new-path) 'x))

;;; path-remove!
(test-assert-thrown 'wrong-type-arg (path-remove! 'p 0))
(test-assert-thrown 'wrong-type-arg (path-remove! (new-path) 'x))

;;; path-insert!
(let ((p (new-path)))
  ;; Wrong object.
  (test-assert-thrown 'wrong-type-arg (path-insert! 'p -1 'moveto '(-200 . -200)))
  (test-assert-thrown 'wrong-type-arg (path-insert! 'p -1 'lineto '(200 . 400)))
  (test-assert-thrown 'wrong-type-arg (path-insert! 'p -1 'curveto '(200 . 400) '(500 . 400) '(500 . 100)))
  (test-assert-thrown 'wrong-type-arg (path-insert! 'p -1 'closepath))
  ;; Wrong index.
  (test-assert-thrown 'wrong-type-arg (path-insert! p 'id 'moveto '(-200 . -200)))
  (test-assert-thrown 'wrong-type-arg (path-insert! p 'id 'lineto '(200 . 400)))
  (test-assert-thrown 'wrong-type-arg (path-insert! p 'id 'curveto '(200 . 400) '(500 . 400) '(500 . 100)))
  (test-assert-thrown 'wrong-type-arg (path-insert! p 'id 'closepath))
  ;; Wrong section name.
  (test-assert-thrown 'misc-error (path-insert! p -1 'move '(-200 . -200)))
  (test-assert-thrown 'misc-error (path-insert! p -1 'line '(200 . 400)))
  (test-assert-thrown 'misc-error (path-insert! p -1 'curve '(200 . 400) '(500 . 400) '(500 . 100)))
  (test-assert-thrown 'misc-error (path-insert! p -1 'close))
  (test-assert-thrown 'wrong-type-arg (path-insert! p -1 100 '(-200 . -200)))
  (test-assert-thrown 'wrong-type-arg (path-insert! p -1 100 '(200 . 400)))
  (test-assert-thrown 'wrong-type-arg (path-insert! p -1 100 '(200 . 400) '(500 . 400) '(500 . 100)))
  (test-assert-thrown 'wrong-type-arg (path-insert! p -1 100))
  ;; Wrong coord.
  (test-assert-thrown 'wrong-type-arg (path-insert! p -1 'moveto 'c))
  (test-assert-thrown 'wrong-type-arg (path-insert! p -1 'lineto 'c))
  (test-assert-thrown 'wrong-type-arg (path-insert! p -1 'curveto 'c '(500 . 400) '(500 . 100)))
  (test-assert-thrown 'wrong-type-arg (path-insert! p -1 'curveto '(200 . 400) 'c '(500 . 100)))
  (test-assert-thrown 'wrong-type-arg (path-insert! p -1 'curveto '(200 . 400) '(500 . 400) 'c)))

(test-end "path-wrong-argument")
