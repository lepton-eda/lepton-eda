;;; Test Scheme procedures for working with source library.

(use-modules (lepton library))

(define *testdir*      (string-append (getcwd)   file-name-separator-string "t0500-tmp"))
;; (define *testcir*      (string-append *testdir*  file-name-separator-string "file.cir"))


(test-begin "source-library-contents")
(mkdir *testdir*)
;; (with-output-to-file *testcir* newline)
(set-source-library-contents! %default-source-library (list *testdir*))
(test-equal (source-library-contents %default-source-library) (list *testdir*))
(set-source-library-contents! %default-source-library '())
(test-equal (source-library-contents %default-source-library) '())
;; (delete-file *testcir*)
(rmdir *testdir*)
(test-end "source-library-contents")
