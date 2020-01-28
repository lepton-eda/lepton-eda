;;; Test Scheme procedures for working with source library.

(use-modules (unit-test)
             (lepton library))

(define *testdir*      (string-append (getcwd)   file-name-separator-string "t0500-tmp"))
;; (define *testcir*      (string-append *testdir*  file-name-separator-string "file.cir"))


(begin-test 'source-library-contents
  (mkdir *testdir*)
  ;; (with-output-to-file *testcir* newline)
  (set-source-library-contents! %default-source-library (list *testdir*))
  (assert-equal (source-library-contents %default-source-library) (list *testdir*))
  (set-source-library-contents! %default-source-library '())
  (assert-equal (source-library-contents %default-source-library) '())
  ;; (delete-file *testcir*)
  (rmdir *testdir*))
