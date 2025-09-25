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
(let ((element-name (lookup-footprint "missing-dir" "any.fp")))
  (test-assert (not element-name)))

(test-end)


(test-begin "lookup-footprint")

(touch "x.fp")

(let ((footprint-x (lookup-footprint "." "x"))
      (footprint-x.fp (lookup-footprint "." "x.fp")))
  (test-assert footprint-x)
  (test-assert footprint-x.fp)
  (test-equal footprint-x "./x.fp")
  (test-equal footprint-x.fp "./x.fp"))

(delete-file "x.fp")

(test-assert (not (lookup-footprint "." "x")))
(test-assert (not (lookup-footprint "." "x.fp")))

(test-end)


(test-begin "lookup-footprint multiple directories")
(test-group-with-cleanup "lookup-footprint multiple directories"
  (test-setup)

  (for-each mkdir '("x" "y" "x/a" "x/b" "y/a" "y/b" "y/c" "y/c/z"))
  (for-each touch '("x/a/X.fp" "y/b/X.fp" "x/b/X.fp" "y/c/z/X.fp" "y/a/X.fp"))

  (let ((footprint-x (lookup-footprint "." "X"))
        (footprint-x.fp (lookup-footprint "." "X.fp")))
    (test-equal "./x/a/X.fp" footprint-x)
    (test-equal "./x/a/X.fp" footprint-x.fp))

  (chmod "x/a" #o000)
  (test-equal "./x/b/X.fp" (lookup-footprint "." "X.fp"))
  (chmod "x/b" #o000)
  (test-equal "./y/a/X.fp" (lookup-footprint "." "X.fp"))
  (chmod "y/a" #o000)
  (test-equal "./y/b/X.fp" (lookup-footprint "." "X.fp"))
  (chmod "y/b" #o000)
  (test-equal "./y/c/z/X.fp" (lookup-footprint "." "X.fp"))
  (chmod "y/c/z" #o000)
  (test-assert (not (lookup-footprint "X.fp" ".")))

  (chmod "y/c/z" #o755)
  (chmod "y/b" #o755)
  (chmod "y/a" #o755)
  (chmod "x/b" #o755)
  (chmod "x/a" #o755)

  ;; Clean up.
  (test-teardown))
(test-end)
