;;; Test that unterminated attribute lists cause a parse error.

(use-modules (lepton page))

(define test-page
"v 20111231 2
N 0 500 1000 500 4
{
T 0 700 5 10 1 0 0 0 1
attrib1=foo
")

(test-begin "parse-unterminated-attribute-list")

(test-assert-thrown 'string-format (string->page "/test/page/A" test-page))

(test-end "parse-unterminated-attribute-list")
