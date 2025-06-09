(use-modules (lepton file-system))

;;; Makes blatant assumptions about the current directory. Oh
;;; well.
(test-begin "regular-file?" 1)
(test-assert (regular-file? "Makefile"))
(test-assert (not (regular-file? ".")))
(test-end "regular-file?")

(test-begin "directory?" 1)
(test-assert (directory? "."))
(test-assert (not (directory? "Makefile")))
(test-end "directory?")
