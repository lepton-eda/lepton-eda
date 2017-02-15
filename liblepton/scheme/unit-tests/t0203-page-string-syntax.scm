;; Test Scheme procedures related to converting pages to & from
;; strings.

(use-modules (unit-test))
(use-modules (geda page))
(use-modules (geda object))

(begin-test 'string->page
  (assert-thrown 'string-format (string->page "/test/page/A" "__GARBAGE__")))
