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
  (with-output-to-file file newline))


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

(define *testdir*/toplevel.cir (make-filename *testdir* "toplevel.cir"))
(define *testdir*/a/a.cir (make-filename *testdir*/a "a.cir"))
(define *testdir*/b/b.cir (make-filename *testdir*/b "b.cir"))
(define *testdir*/b/b/bb.cir (make-filename *testdir*/b/b "bb.cir"))
(define *testdir*/b/b/c/bbc.cir (make-filename *testdir*/b/b/c "bbc.cir"))


(define (source-library-test-setup)
  (mkdir *testdir*)
  (mkdir *testdir*/a)
  (mkdir *testdir*/b)
  (mkdir *testdir*/b/b)
  (mkdir *testdir*/b/b/c)
  (mkdir *testdir*/d)
  ;; Make files.
  (touch *testdir*/toplevel.cir)
  (touch *testdir*/a/a.cir)
  (touch *testdir*/b/b.cir)
  (touch *testdir*/b/b/bb.cir)
  (touch *testdir*/b/b/c/bbc.cir))

(define (source-library-test-teardown)
  (system* "rm" "-rf" *testdir*))


(test-begin "source-library")

(test-group-with-cleanup "source-library-group"
  (source-library-test-setup)

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
    (test-assert (getenv "MYDIR"))
    (test-eq (source-library "${MYDIR}") lib)
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
    (test-equal (source-library-contents lib) (list *testdir*/b *testdir*/a))

    ;; Test get-source-library-file().
    (test-equal (get-source-library-file "a.cir")
      *testdir*/a/a.cir)
    (test-equal (get-source-library-file "b.cir")
      *testdir*/b/b.cir)
    ;; The toplevel directory is not included.
    (test-assert (not (get-source-library-file "toplevel.cir")))
    ;; These two also fail since the directories are not added
    ;; recursively.
    (test-assert (not (get-source-library-file "bb.cir")))
    (test-assert (not (get-source-library-file "bbc.cir")))

    ;; Test for non-readable file.
    (when (zero? (getuid)) (test-skip 1))
    ;; Make the file unreadable for non-root users.
    (chmod *testdir*/a/a.cir #o000)
    (test-assert (not (get-source-library-file "a.cir"))))

  ;; Restore permissions of the directory.  This is required on
  ;; some systems to avoid breaking the test.
  (chmod *testdir*/d #o775)

  ;; Get rid of the test directory.
  (source-library-test-teardown))

(test-end "source-library")


(test-begin "source-library-search")

(test-group-with-cleanup "source-library-search-group"
  (source-library-test-setup)
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
    (test-assert (getenv "MYDIR"))
    (test-eq (source-library-search "${MYDIR}") lib)
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
    ;; This test returns different sort order on different
    ;; systems for some reason.  So just sort the lists.
    (test-equal (sort (source-library-contents lib) string<)
      (sort (list *testdir*/a *testdir*/b/b/c *testdir*/b/b *testdir*/b *testdir*) string<))

    ;; Test get-source-library-file().
    (test-equal (get-source-library-file "toplevel.cir")
      *testdir*/toplevel.cir)
    (test-equal (get-source-library-file "a.cir")
      *testdir*/a/a.cir)
    (test-equal (get-source-library-file "b.cir")
      *testdir*/b/b.cir)
    (test-equal (get-source-library-file "bb.cir")
      *testdir*/b/b/bb.cir)
    (test-equal (get-source-library-file "bbc.cir")
      *testdir*/b/b/c/bbc.cir)

    ;; Test for non-readable file.
    (when (zero? (getuid)) (test-skip 1))
    ;; Make the file unreadable for non-root users.
    (chmod *testdir*/a/a.cir #o000)
    (test-assert (not (get-source-library-file "a.cir"))))

  ;; Restore permissions of the directory.  This is required on
  ;; some systems to avoid breaking the test.
  (chmod *testdir*/d #o775)

  ;; Get rid of the test directory.
  (source-library-test-teardown))

(test-end "source-library-search")


(test-begin "get-source-library-file")

;;; The function is thoroughly tested above, so there is only one
;;; test here.
(test-assert-thrown 'wrong-type-arg (get-source-library-file 'a))

(test-end "get-source-library-file")
