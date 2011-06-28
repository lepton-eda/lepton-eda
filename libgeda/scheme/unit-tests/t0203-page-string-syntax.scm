;; Test Scheme procedures related to converting pages to & from
;; strings.

(use-modules (unit-test))
(use-modules (geda page))
(use-modules (geda object))

;; string->page should bork if the string contains invalid syntax.  It
;; might not throw a misc-error; this is just a placeholder key.
(begin-test 'string->page
  (assert-thrown 'invalid-string (string->page "/test/page/A" "__GARBAGE__")))
