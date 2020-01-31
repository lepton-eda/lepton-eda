;; Test that loading a schematic file returns an error when an
;; embedded component data section is found without a corresponding
;; object.

(use-modules (unit-test)
             ((geda page) #:renamer (symbol-prefix-proc 'geda:))
             (lepton page))

(define test-page
"v 20111231 2
[
L 0 0 1000 0 3 0 0 0 -1 -1
]
")

(begin-test 'parse-embedded-section-without-component
  (assert-thrown 'string-format (string->page "test/page/A" test-page)))


;;; The same tests for the deprecated (geda page) module
;;; functions.

(begin-test 'geda:parse-embedded-section-without-component
  (assert-thrown 'string-format (geda:string->page "test/page/A" test-page)))
