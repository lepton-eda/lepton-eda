;; Test Scheme procedures related to converting pages to & from
;; strings.

(use-modules (unit-test)
             (lepton object)
             ((geda page) #:renamer (symbol-prefix-proc 'geda:))
             (lepton page))

(begin-test 'string->page
  (assert-thrown 'string-format (string->page "/test/page/A" "__GARBAGE__")))


;;; The same tests for the deprecated (geda page) module
;;; functions.

(begin-test 'geda:string->page
  (assert-thrown 'string-format (geda:string->page "/test/page/A" "__GARBAGE__")))
