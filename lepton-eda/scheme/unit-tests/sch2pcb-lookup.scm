(use-modules (system foreign)
             (sch2pcb lookup))

(define cwd (getcwd))
(define *testdir*
  (string-append (getcwd)
                 file-name-separator-string
                 "sch2pcb-tmp"))

;;; Setup/teardown directories/files needed by tests.
(define (test-setup)
  (mkdir *testdir*)
  (chdir *testdir*))

(define (test-teardown)
  (chdir cwd)
  (system* "rm" "-rf" *testdir*))


(define (touch file)
  (with-output-to-file file
    (lambda () (display ""))))


(test-begin "lookup-in-missing-directory")

;;; The procedure should probably raise an exception if directory
;;; is missing or not readable:
;;; (test-assert-thrown 'system-error
;;;                     (lookup-footprint "missing-dir" "any.fp"))
(let ((*element (lookup-footprint "missing-dir" "any.fp")))
  ;; Currently, the function returns %null-pointer if the
  ;; directory to search in is missing.
  (test-assert (null-pointer? *element)))

(test-end)


(test-begin "lookup-footprint")

(touch "x.fp")

(let ((footprint-x (lookup-footprint "." "x"))
      (footprint-x.fp (lookup-footprint "." "x.fp")))
  (test-assert (not (null-pointer? footprint-x)))
  (test-assert (not (null-pointer? footprint-x.fp)))
  (test-equal (pointer->string footprint-x) "./x.fp")
  (test-equal (pointer->string footprint-x.fp) "./x.fp"))

(delete-file "x.fp")

(test-assert (null-pointer? (lookup-footprint "." "x")))
(test-assert (null-pointer? (lookup-footprint "." "x.fp")))

(test-end)


(test-begin "lookup-footprint multiple directories")
(test-group-with-cleanup "lookup-footprint multiple directories"
  (test-setup)

  (for-each mkdir '("x" "y" "x/a" "x/b" "y/a" "y/b" "y/c" "y/c/z"))
  (for-each touch '("x/a/X.fp" "y/b/X.fp" "x/b/X.fp" "y/c/z/X.fp" "y/a/X.fp"))

  (let ((footprint-x (lookup-footprint "." "X"))
        (footprint-x.fp (lookup-footprint "." "X.fp")))
    (test-equal (pointer->string footprint-x) "./y/c/z/X.fp")
    (test-equal (pointer->string footprint-x.fp) "./y/c/z/X.fp"))

  (chmod "y/c/z" #o000)
  (test-equal (pointer->string (lookup-footprint "." "X.fp")) "./y/b/X.fp")
  (chmod "y/b" #o000)
  (test-equal (pointer->string (lookup-footprint "." "X.fp")) "./y/a/X.fp")
  (chmod "y/a" #o000)
  (test-equal (pointer->string (lookup-footprint "." "X.fp")) "./x/b/X.fp")
  (chmod "x/b" #o000)
  (test-equal (pointer->string (lookup-footprint "." "X.fp")) "./x/a/X.fp")
  (chmod "x/a" #o000)
  (test-assert (null-pointer? (lookup-footprint "." "X.fp")))

  (chmod "y/c/z" #o755)
  (chmod "y/b" #o755)
  (chmod "y/a" #o755)
  (chmod "x/b" #o755)
  (chmod "x/a" #o755)

  ;; Clean up.
  (test-teardown))
(test-end)
