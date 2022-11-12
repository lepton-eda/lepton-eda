(use-modules (system foreign)
             (sch2pcb lookup))


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
