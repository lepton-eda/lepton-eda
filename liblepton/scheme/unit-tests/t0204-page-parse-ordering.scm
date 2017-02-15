;; Test that when we load a schematic file, everything gets connected
;; in the correct order.

(use-modules (unit-test)
             (geda page)
             (geda object)
             (geda attrib))

(define test-page
"v 20111231 2
L 0 0 1000 0 3 0 0 0 -1 -1
N 0 500 1000 500 4
{
T 0 700 5 10 1 0 0 0 1
attrib1=foo
T 0 1000 5 10 1 0 0 0 1
attrib2=bar
}")

(begin-test 'parse-ordering-objects
  (let* ((A (string->page "test/page/A" test-page))
         (lst (page-contents A)))
    (assert-true (line? (list-ref lst 0)))
    (assert-true (net? (list-ref lst 1)))
    (assert-true (text? (list-ref lst 2)))
    (assert-true (text? (list-ref lst 3)))

    (assert-equal "attrib1=foo" (text-string (list-ref lst 2)))
    (assert-equal "attrib2=bar" (text-string (list-ref lst 3)))

    (assert-equal (list-tail lst 2) (object-attribs (list-ref lst 1)))))
