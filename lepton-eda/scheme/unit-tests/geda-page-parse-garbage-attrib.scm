;;; Test that garbage in attribute lists causes a parse error.

(use-modules ((geda page) #:renamer (symbol-prefix-proc 'geda:)))

(define test-page
"v 20111231 2
N 0 500 1000 500 4
{
T 0 700 5 10 1 0 0 0 1
attrib1=foo
__GARBAGE__
")

(test-begin "geda:parse-garbage-attribute")

(test-assert-thrown 'string-format (geda:string->page "/test/page/A" test-page))

(test-end "geda:parse-garbage-attribute")
