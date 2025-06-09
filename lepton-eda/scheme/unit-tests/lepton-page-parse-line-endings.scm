;;; Test that when we load a schematic file, different line
;;; endings are dealt with properly.

(use-modules (lepton attrib)
             (lepton object)
             (lepton page))

(define test-page-unix
  "v 20111231 2\nL 0 0 1000 0 3 0 0 0 -1 -1\nT 0 1000 5 10 1 0 0 0 1\nattrib2=bar")
(define test-page-dos
  "v 20111231 2\r\nL 0 0 1000 0 3 0 0 0 -1 -1\r\nT 0 1000 5 10 1 0 0 0 1\r\nattrib2=bar")
(define test-page-mac
  "v 20111231 2\rL 0 0 1000 0 3 0 0 0 -1 -1\rT 0 1000 5 10 1 0 0 0 1\rattrib2=bar")

(define (test-func test-name data)
  (let* ((A (string->page "test/page/A" data))
         (lst (page-contents A)))
    (test-assert test-name (line? (list-ref lst 0)))
    (test-assert test-name (text? (list-ref lst 1)))
    (test-equal test-name "attrib2=bar" (text-string (list-ref lst 1)))))

(test-begin "parse-line-endings")
(test-func "parse-unix-line-endings" test-page-unix)
(test-func "parse-dos-line-endings" test-page-dos)
(test-func "parse-mac-line-endings" test-page-mac)
(test-end "parse-line-endings")
