;;; Test Scheme procedures for working with source library.

(use-modules (lepton library))

(define *testdir*
  (string-append (getcwd) file-name-separator-string "test-source-library"))



(test-begin "source-library-contents")

(dynamic-wind
  (lambda ()
    (mkdir *testdir*))
  (lambda ()
    (set-source-library-contents! %default-source-library (list *testdir*))
    (test-equal (source-library-contents %default-source-library) (list *testdir*))
    (set-source-library-contents! %default-source-library '())
    (test-eq (source-library-contents %default-source-library) '()))
  (lambda ()
    (rmdir *testdir*)))

(test-end "source-library-contents")
