;; Test that garbage in attribute lists causes a parse error.

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
__GARBAGE__
")

(begin-test 'parse-garbage-attribute
  (assert-thrown 'string-format (string->page "/test/page/A" test-page)))


;;; The same tests for the deprecated (geda page) module
;;; functions.

(begin-test 'geda:parse-garbage-attribute
  (assert-thrown 'string-format (geda:string->page "/test/page/A" test-page)))
