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


(define *testdir*/a (make-filename *testdir* "a"))
(define *testdir*/a/non-existing-dir (make-filename *testdir*/a "non-existing-dir"))
(define *testdir*/b (make-filename *testdir* "b"))
(define *testdir*/b/b (make-filename *testdir*/b "b"))
(define *testdir*/b/b/c (make-filename *testdir*/b/b "c"))
(define *testdir*/d (make-filename *testdir* "d"))


(test-begin "source-library")

(dynamic-wind
  (lambda ()
    (mkdir *testdir*)
    (mkdir *testdir*/a)
    (mkdir *testdir*/b)
    (mkdir *testdir*/b/b)
    (mkdir *testdir*/b/b/c)
    (mkdir *testdir*/d)
    ;; Make files.
    (touch (make-filename *testdir* "toplevel.cir"))
    (touch (make-filename *testdir*/a "a.cir"))
    (touch (make-filename *testdir*/b "b.cir"))
    (touch (make-filename *testdir*/b/b "bb.cir"))
    (touch (make-filename *testdir*/b/b/c "bbc.cir")))

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

      ;; Test it is still the same library after adding a
      ;; resource.
      (test-eq (source-library *testdir*/a) lib)
      ;; Test library contents.
      (test-equal (source-library-contents lib) (list *testdir*/a))
      ;; Test expansion of environment variables.
      (putenv (string-append "MYDIR=" *testdir*/b))
      (test-eq (source-library *testdir*/b) lib)
      (test-equal (source-library-contents lib) (list *testdir*/b *testdir*/a))
      ;; Test for non-existing directory.
      (test-eq (source-library *testdir*/a/non-existing-dir) lib)
      ;; Test the contents did not change.
      (test-equal (source-library-contents lib) (list *testdir*/b *testdir*/a))
      ;; Test for non-readable directory.
      (when (zero? (getuid)) (test-skip 1))
      ;; Make the directory unreadable for non-root users.
      (chmod *testdir*/d #o000)
      ;; Try to add it to the library.
      (test-eq (source-library *testdir*/d) lib)
      ;; Test the contents did not change.
      (test-equal (source-library-contents lib) (list *testdir*/b *testdir*/a))
      ;; Test for duplicate input.
      (test-eq (source-library *testdir*/a) lib)
      ;; Test the contents did not change.
      (test-equal (source-library-contents lib) (list *testdir*/b *testdir*/a))))

  ;; Get rid of the test directory.
  (lambda ()
    (system* "rm" "-rf" *testdir*)))

(test-end "source-library")


(test-begin "source-library-search")

(dynamic-wind
  (lambda ()
    (mkdir *testdir*)
    (mkdir *testdir*/a)
    (mkdir *testdir*/b)
    (mkdir *testdir*/b/b)
    (mkdir *testdir*/b/b/c)
    (mkdir *testdir*/d)
    ;; Make files.
    (touch (make-filename *testdir* "toplevel.cir"))
    (touch (make-filename *testdir*/a "a.cir"))
    (touch (make-filename *testdir*/b "b.cir"))
    (touch (make-filename *testdir*/b/b "bb.cir"))
    (touch (make-filename *testdir*/b/b/c "bbc.cir")))

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

      ;; Test it is still the same library after adding a
      ;; resource.
      (test-eq (source-library-search *testdir*/a) lib)
      ;; Test library contents.
      (test-equal (source-library-contents lib) (list *testdir*/a))
      ;; Test expansion of environment variables.
      (putenv (string-append "MYDIR=" *testdir*/b))
      (test-eq (source-library-search *testdir*/b) lib)
      (test-equal (source-library-contents lib)
        (list *testdir*/b/b/c *testdir*/b/b *testdir*/b *testdir*/a))
      ;; Test for non-existing directory.
      (test-eq (source-library-search *testdir*/a/non-existing-dir) lib)
      ;; Test the contents did not change.
      (test-equal (source-library-contents lib)
        (list *testdir*/b/b/c *testdir*/b/b *testdir*/b *testdir*/a))
      ;; Test for non-readable directory.
      (when (zero? (getuid)) (test-skip 1))
      ;; Make the directory unreadable for non-root users.
      (chmod *testdir*/d #o000)
      ;; Try to add it to the library.
      (test-eq (source-library-search *testdir*/d) lib)
      ;; Test the contents did not change.
      (test-equal (source-library-contents lib)
        (list *testdir*/b/b/c *testdir*/b/b *testdir*/b *testdir*/a))
      ;; Test for duplicate input.
      (test-eq (source-library-search *testdir*/a) lib)
      ;; Test the contents did not change.
      (test-equal (source-library-contents lib)
        (list *testdir*/b/b/c *testdir*/b/b *testdir*/b *testdir*/a))
      ;; Test adding the whole toplevel directory.
      (test-eq (source-library-search *testdir*) lib)
      ;; Test the library now contains only non-duplicate
      ;; directories with files.
      (test-equal (source-library-contents lib)
        (list *testdir* *testdir*/b/b/c *testdir*/b/b *testdir*/b *testdir*/a))
      ;; Reset the library and populate it from scratch.  The
      ;; list of directories should change.
      (reset-source-library)
      (test-eq (source-library-search *testdir*) lib)
      (test-equal (source-library-contents lib)
        (list *testdir*/a *testdir*/b/b/c *testdir*/b/b *testdir*/b *testdir*))))

  ;; Get rid of the test directory.
  (lambda ()
    (system* "rm" "-rf" *testdir*)))

(test-end "source-library-search")
