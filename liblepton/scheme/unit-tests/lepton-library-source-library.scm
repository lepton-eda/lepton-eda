;;; Test Scheme procedures for working with source library.

(use-modules (lepton library))

(define *testdir*
  (string-append (getcwd) file-name-separator-string "test-source-library"))

;;; Make filenames by joining strings and using filename separator
;;; as a delimiter between them.
(define (make-filename . args)
  (string-join args file-name-separator-string 'infix))

;;; Create library files.
(define (touch file)
  (with-output-to-file file (lambda () (display ""))))


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


(test-begin "source-library")

(dynamic-wind
  (lambda ()
    (mkdir *testdir*)
    (mkdir (make-filename *testdir* "a"))
    (mkdir (make-filename *testdir* "b"))
    (mkdir (make-filename *testdir* "b" "b"))
    (mkdir (make-filename *testdir* "b" "b" "c"))
    (mkdir (make-filename *testdir* "d"))
    ;; Make files.
    (touch (make-filename *testdir* "toplevel.cir"))
    (touch (make-filename *testdir* "a" "a.cir"))
    (touch (make-filename *testdir* "b" "b.cir"))
    (touch (make-filename *testdir* "b" "b" "bb.cir"))
    (touch (make-filename *testdir* "b" "b" "c" "bbc.cir")))

  (lambda ()
    ;; First, reset the source library to ensure it is empty.
    (let ((lib (reset-source-library)))
      ;; Test if lib is <source-library>.
      (test-assert (source-library? lib))
      ;; Test it is the default library.
      (test-eq lib %default-source-library)
      ;; Test library contents.
      (test-eq (source-library-contents lib) '())
      ;; Test for wrong argument.
      (test-assert-thrown 'wrong-type-arg (source-library 'a))

      (let ((dir-a (make-filename *testdir* "a"))
            (dir-non-existing (make-filename *testdir* "a" "non-existing-dir"))
            (dir-b (make-filename *testdir* "b"))
            (dir-d (make-filename *testdir* "d")))
        ;; Test it is still the same library after adding a
        ;; resource.
        (test-eq (source-library dir-a) lib)
        ;; Test library contents.
        (test-equal (source-library-contents lib) (list dir-a))
        ;; Test expansion of environment variables.
        (putenv (string-append "MYDIR=" dir-b))
        (test-eq (source-library dir-b) lib)
        (test-equal (source-library-contents lib) (list dir-b dir-a))
        ;; Test for non-existing directory.
        (test-eq (source-library dir-non-existing) lib)
        ;; Test the contents did not change.
        (test-equal (source-library-contents lib) (list dir-b dir-a))
        ;; Test for non-readable directory.
        (when (zero? (getuid)) (test-skip 1))
        ;; Make the directory unreadable for non-root users.
        (chmod dir-d #o000)
        ;; Try to add it to the library.
        (test-eq (source-library dir-d) lib)
        ;; Test the contents did not change.
        (test-equal (source-library-contents lib) (list dir-b dir-a))
        ;; Test for duplicate input.
        (test-eq (source-library dir-a) lib)
        ;; Test the contents did not change.
        (test-equal (source-library-contents lib) (list dir-b dir-a)))))

  ;; Get rid of the test directory.
  (lambda ()
    (system* "rm" "-rf" *testdir*)))

(test-end "source-library")


(test-begin "source-library-search")

(dynamic-wind
  (lambda ()
    (mkdir *testdir*)
    (mkdir (make-filename *testdir* "a"))
    (mkdir (make-filename *testdir* "b"))
    (mkdir (make-filename *testdir* "b" "b"))
    (mkdir (make-filename *testdir* "b" "b" "c"))
    (mkdir (make-filename *testdir* "d"))
    ;; Make files.
    (touch (make-filename *testdir* "toplevel.cir"))
    (touch (make-filename *testdir* "a" "a.cir"))
    (touch (make-filename *testdir* "b" "b.cir"))
    (touch (make-filename *testdir* "b" "b" "bb.cir"))
    (touch (make-filename *testdir* "b" "b" "c" "bbc.cir")))

  (lambda ()
    ;; First, reset the source library to ensure it is empty.
    (let ((lib (reset-source-library)))
      ;; Test if lib is <source-library>.
      (test-assert (source-library? lib))
      ;; Test it is the default library.
      (test-eq lib %default-source-library)
      ;; Test library contents.
      (test-eq (source-library-contents lib) '())
      ;; Test for wrong argument.
      (test-assert-thrown 'wrong-type-arg (source-library-search 'a))

      (let ((dir-a (make-filename *testdir* "a"))
            (dir-non-existing (make-filename *testdir* "a" "non-existing-dir"))
            (dir-b (make-filename *testdir* "b"))
            (dir-b-b (make-filename *testdir* "b" "b"))
            (dir-b-b-c (make-filename *testdir* "b" "b" "c"))
            (dir-d (make-filename *testdir* "d")))
        ;; Test it is still the same library after adding a
        ;; resource.
        (test-eq (source-library-search dir-a) lib)
        ;; Test library contents.
        (test-equal (source-library-contents lib) (list dir-a))
        ;; Test expansion of environment variables.
        (putenv (string-append "MYDIR=" dir-b))
        (test-eq (source-library-search dir-b) lib)
        (test-equal (source-library-contents lib) (list dir-b-b-c dir-b-b dir-b dir-a))
        ;; Test for non-existing directory.
        (test-eq (source-library-search dir-non-existing) lib)
        ;; Test the contents did not change.
        (test-equal (source-library-contents lib) (list dir-b-b-c dir-b-b dir-b dir-a))
        ;; Test for non-readable directory.
        (when (zero? (getuid)) (test-skip 1))
        ;; Make the directory unreadable for non-root users.
        (chmod dir-d #o000)
        ;; Try to add it to the library.
        (test-eq (source-library-search dir-d) lib)
        ;; Test the contents did not change.
        (test-equal (source-library-contents lib) (list dir-b-b-c dir-b-b dir-b dir-a))
        ;; Test for duplicate input.
        (test-eq (source-library-search dir-a) lib)
        ;; Test the contents did not change.
        (test-equal (source-library-contents lib) (list dir-b-b-c dir-b-b dir-b dir-a))
        ;; Test adding the whole toplevel directory.
        (test-eq (source-library-search *testdir*) lib)
        ;; Test the library now contains only non-duplicate
        ;; directories with files.
        (test-equal (source-library-contents lib) (list *testdir* dir-b-b-c dir-b-b dir-b dir-a))
        ;; Reset the library and populate it from scratch.  The
        ;; list of directories should change.
        (reset-source-library)
        (test-eq (source-library-search *testdir*) lib)
        (test-equal (source-library-contents lib) (list dir-a dir-b-b-c dir-b-b dir-b *testdir*)))))

  ;; Get rid of the test directory.
  (lambda ()
    (system* "rm" "-rf" *testdir*)))

(test-end "source-library-search")
