(use-modules (unit-test)
             (lepton file-system))

; Makes blatant assumptions about the current directory. Oh well:
;
(begin-test 'regular-file?
 (assert-true (regular-file? "Makefile"))
 (assert-true (not (regular-file? "."))))

(begin-test 'directory?
 (assert-true (directory? "."))
 (assert-true (not (directory? "Makefile"))))
