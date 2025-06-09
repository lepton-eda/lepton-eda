;;; Test promotable-attributes function.

(use-modules (lepton attrib)
             (lepton config)
             (lepton object)
             (lepton page))

(test-begin "promotable-attributes")

(let ((cfg (path-config-context (getcwd)))
      (P (make-page "new-page"))
      (C (make-component "test component" '(0 . 0) 0 #t #f))
      (a (make-text '(1 . 2) 'lower-left 0 "name=x" 10 #t 'both)))
  (set-config! cfg "schematic.attrib" "always-promote" "name")
  (test-equal '("name") (config-string-list cfg "schematic.attrib" "always-promote"))
  (component-append! C a)
  (page-append! P C)
  (test-eq "promotable-attribs" 1 (length (promotable-attribs C)))
  (test-eq 1 (length (page-contents P)))
  (test-eq "promote-attribs!" 1 (length (promote-attribs! C)))
  (test-eq 2 (length (page-contents P)))
  (close-page! P))

(test-assert-thrown 'wrong-type-arg (promotable-attribs 'x))

(test-end "promotable-attributes")


(test-begin "promote-attribs!/not-in-page")

(let ((p (make-net-pin '(0 . 0) '(100 . 0))))
  (test-assert-thrown 'object-state (promote-attribs! p)))

(test-end "promote-attribs!/not-in-page")


(test-begin "promote-attribs!/non-component")

(let ((P (make-page "/test/page/A"))
      (p (make-net-pin '(0 . 0) '(100 . 0))))
  (page-append! P p)
  (test-equal '() (promote-attribs! p)))

(test-end "promote-attribs!/non-component")
