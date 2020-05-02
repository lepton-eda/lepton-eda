;;; Test that when we load a schematic file, everything gets connected
;;; in the correct order.

(use-modules (lepton attrib)
             (lepton object)
             (lepton page))

(define test-page
"v 20111231 2
L 0 0 1000 0 3 0 0 0 -1 -1
N 0 500 1000 500 4
{
T 0 700 5 10 1 0 0 0 1
attrib1=foo
T 0 1000 5 10 1 0 0 0 1
attrib2=bar
}")

(test-begin "parse-ordering-objects")

(let* ((A (string->page "test/page/A" test-page))
       (lst (page-contents A)))
  (test-assert (line? (list-ref lst 0)))
  (test-assert (net? (list-ref lst 1)))
  (test-assert (text? (list-ref lst 2)))
  (test-assert (text? (list-ref lst 3)))

  (test-equal "attrib1=foo" (text-string (list-ref lst 2)))
  (test-equal "attrib2=bar" (text-string (list-ref lst 3)))

  (test-equal (list-tail lst 2) (object-attribs (list-ref lst 1))))

(test-end "parse-ordering-objects")
