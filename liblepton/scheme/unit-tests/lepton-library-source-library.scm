;;; Test Scheme procedures for working with source library.

(use-modules (lepton library))

(define *testdir*      (string-append (getcwd)   file-name-separator-string "t0500-tmp"))


(test-begin "source-library-contents")
(mkdir *testdir*)
(set-source-library-contents! %default-source-library (list *testdir*))
(test-equal (source-library-contents %default-source-library) (list *testdir*))
(set-source-library-contents! %default-source-library '())
(test-equal (source-library-contents %default-source-library) '())
(rmdir *testdir*)
(test-end "source-library-contents")
