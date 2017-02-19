;; Test that garbage in attribute lists causes a parse error.

(use-modules (unit-test)
             (geda page)
             (geda object)
             (geda attrib))

(define test-page
"v 20111231 2
N 0 500 1000 500 4
{
T 0 700 5 10 1 0 0 0 1
attrib1=foo
__GARBAGE__
")

(begin-test 'parse-garbage-attribute
  (assert-thrown 'string-format (string->page "/test/page/A" test-page)))
