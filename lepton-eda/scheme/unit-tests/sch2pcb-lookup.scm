(use-modules (system foreign)
             (sch2pcb lookup))

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
