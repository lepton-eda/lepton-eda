;;; Test Scheme procedures working with toplevel wrapped foreign
;;; pointers.

(use-modules (lepton toplevel))

(define %global-toplevel (current-toplevel))
(define %local-toplevel (make-toplevel))

(test-begin "toplevel")

;;; Test that the variables are not #f.
(test-assert %global-toplevel)
(test-assert %local-toplevel)

;;; Test if they are <toplevel> objects.
(test-assert (toplevel? %global-toplevel))
(test-assert (toplevel? %local-toplevel))

;;; Test set-current-toplevel!().
(set-current-toplevel! %local-toplevel)
(test-eq (current-toplevel) %local-toplevel)

;;; Temporary set global toplevel.
(with-toplevel %global-toplevel
 (lambda ()
   ;; Test it is used as the current one.
   (test-eq (current-toplevel) %global-toplevel)))

;;; Test once again that the current toplevel is the local one.
(test-eq (current-toplevel) %local-toplevel)

;;; Revert to the previous global value.
(set-current-toplevel! %global-toplevel)
(test-eq (current-toplevel) %global-toplevel)

(test-end "toplevel")
