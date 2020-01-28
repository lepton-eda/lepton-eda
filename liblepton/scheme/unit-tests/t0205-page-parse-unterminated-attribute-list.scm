;; Test that unterminated attribute lists cause a parse error.

(use-modules (unit-test)
             (lepton attrib)
             (lepton object)
             ((geda page) #:renamer (symbol-prefix-proc 'geda:))
             (lepton page))

(define test-page
"v 20111231 2
N 0 500 1000 500 4
{
T 0 700 5 10 1 0 0 0 1
attrib1=foo
")

(begin-test 'parse-unterminated-attribute-list
  (assert-thrown 'string-format (string->page "/test/page/A" test-page)))


;;; The same tests for the deprecated (geda page) module
;;; functions.

(begin-test 'geda:parse-unterminated-attribute-list
  (assert-thrown 'string-format (geda:string->page "/test/page/A" test-page)))
