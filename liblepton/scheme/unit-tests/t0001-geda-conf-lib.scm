(use-modules (unit-test)
             (lepton file-system)
             (lepton rc))

(begin-test 'build-path
 (assert-equal
  "prefix/suffix"
  (build-path "prefix" "suffix"))
 (assert-equal
  "/path/to/a/directory"
  (build-path "/path" "to" "a" "directory")))

; Makes blatant assumptions about the current directory. Oh well:
;
(begin-test 'regular-file?
 (assert-true (regular-file? "Makefile"))
 (assert-true (not (regular-file? "."))))

(begin-test 'directory?
 (assert-true (directory? "."))
 (assert-true (not (directory? "Makefile"))))
