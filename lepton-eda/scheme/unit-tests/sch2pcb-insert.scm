;;; Test Scheme procedures related to pcb elements.

(use-modules (system foreign)
             (lepton ffi sch2pcb)
             (sch2pcb element)
             (sch2pcb insert))

(define *testdir*
  (string-append (getcwd)
                 file-name-separator-string
                 "sch2pcb-insert-element"))

;;; Setup/teardown of directories / files needed by tests.
(define (config-test-setup)
  (mkdir *testdir*))

(define (config-test-teardown)
  (system* "rm" "-rf" *testdir*))


(test-begin "insert-file-element-null-output-file")

;;; Test the function with NULL output file.
(let* ((*output-file %null-pointer)
       (element-filename "element.fp")
       (*element (pkg-line->element "PKG_DIP14(DIP14,U100,unknown)")))
  ;; Skip the next test until the *OUTPUT-FILE argument will be
  ;; checked in the function.
  (test-skip 1)
  (test-assert-thrown 'misc-error
                      (insert-file-element *output-file element-filename *element)))
(test-end)


(test-begin "insert-file-element-missing-file")
(test-group-with-cleanup "insert-file-element-missing-file-grp"
  (config-test-setup)
  ;; Test an exit status and side effects of the function when
  ;; called with some non-existing element file.
  (let* ((*output-file
          (sch2pcb_open_file_to_write
           (string->pointer
            (string-append *testdir*
                           file-name-separator-string
                           "output.pcb"))))
         (element-filename "some-non-existing-element.fp")
         (*element (pkg-line->element "PKG_DIP14(DIP14,U100,unknown)"))
         (<stderr>
          (with-error-to-string
            (lambda ()
              (let ((<result>
                     (insert-file-element *output-file element-filename *element)))
                (test-assert (not <result>)))))))
    ;; Skip the next tests.  Currently, the C code deals with error
    ;; output.  It seems, it outputs errors directly to stderr file
    ;; using its file description info, so I cannot catch it.
    (test-skip 2)
    (test-assert (string-contains <stderr>
                                  (string-append "insert_element() can't open "
                                                 element-filename)))
    (test-assert (string-contains <stderr> "No such file or directory"))
    (sch2pcb_close_file *output-file))
  ;; Clean up.
  (config-test-teardown))
(test-end)
