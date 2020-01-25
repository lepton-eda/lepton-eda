;; Test that when we load a schematic file, different line endings are
;; dealt with properly.

(use-modules (unit-test)
             (geda attrib)
             (lepton object)
             ((geda page) #:renamer (symbol-prefix-proc 'geda:))
             (lepton page))

(define test-page-unix
  "v 20111231 2\nL 0 0 1000 0 3 0 0 0 -1 -1\nT 0 1000 5 10 1 0 0 0 1\nattrib2=bar")
(define test-page-dos
  "v 20111231 2\r\nL 0 0 1000 0 3 0 0 0 -1 -1\r\nT 0 1000 5 10 1 0 0 0 1\r\nattrib2=bar")
(define test-page-mac
  "v 20111231 2\rL 0 0 1000 0 3 0 0 0 -1 -1\rT 0 1000 5 10 1 0 0 0 1\rattrib2=bar")

(define (test-func data)
  (let* ((A (string->page "test/page/A" data))
         (lst (page-contents A)))
    (assert-true (line? (list-ref lst 0)))
    (assert-true (text? (list-ref lst 1)))
    (assert-equal "attrib2=bar" (text-string (list-ref lst 1)))))

(begin-test 'parse-unix-line-endings (test-func test-page-unix))
(begin-test 'parse-dos-line-endings (test-func test-page-dos))
(begin-test 'parse-mac-line-endings (test-func test-page-mac))


;;; The same tests for the deprecated (geda page) module
;;; functions.

(define (geda:test-func data)
  (let* ((A (geda:string->page "test/page/A" data))
         (lst (geda:page-contents A)))
    (assert-true (line? (list-ref lst 0)))
    (assert-true (text? (list-ref lst 1)))
    (assert-equal "attrib2=bar" (text-string (list-ref lst 1)))))

(begin-test 'geda:parse-unix-line-endings (geda:test-func test-page-unix))
(begin-test 'geda:parse-dos-line-endings (geda:test-func test-page-dos))
(begin-test 'geda:parse-mac-line-endings (geda:test-func test-page-mac))
