;; Test Scheme procedures related to path objects.

(use-modules (unit-test))
(use-modules (geda object))

(begin-test 'paths
  (let* ((a (make-path))
         (b (make-path 21)))

    (assert-equal 'path (object-type a))
    (assert-true (path? a))
    (assert-equal 0 (path-length a))
    (assert-thrown 'out-of-range (path-ref a 0))
    (assert-thrown 'out-of-range (path-remove! a 0))
    (assert-equal #f (object-bounds a))

    ; Most trivial path possible
    (assert-equal a (path-insert! a -1 'lineto '(0 . 0)))
    (assert-equal 1 (path-length a))
    (assert-equal '(lineto (0 . 0)) (path-ref a 0))
    (assert-equal '((0 . 0) . (0 . 0)) (object-bounds a))

    ; Add a move to the start
    (assert-equal a (path-insert! a 0 'moveto '(1 . 0)))
    (assert-equal 2 (path-length a))
    (assert-equal '(moveto (1 . 0)) (path-ref a 0))
    (assert-equal '(lineto (0 . 0)) (path-ref a 1))
    (assert-equal '((0 . 0) . (1 . 0)) (object-bounds a))

    ; Remove the line
    (assert-equal a (path-remove! a 1))
    (assert-equal 1 (path-length a))
    (assert-thrown 'out-of-range (path-ref a 1))
    (assert-thrown 'out-of-range (path-remove! a 1))
    (assert-equal '((1 . 0) . (1 . 0)) (object-bounds a))

    ; Color
    (assert-equal 21 (object-color b))
))
