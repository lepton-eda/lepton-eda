;;; Test Scheme procedures related to pcb elements.

(use-modules (ice-9 textual-ports)
             (system foreign)
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


(test-begin "insert-file-element-really-pcb-file")
(test-group-with-cleanup "insert-file-element-really-pcb-file-grp"
  (config-test-setup)
  ;; Test an exit status and side effects of the function when
  ;; called with a file element that is really a PCB file.
  (let* ((*output-file
          (sch2pcb_open_file_to_write
           (string->pointer (string-append *testdir*
                                           file-name-separator-string
                                           "output.pcb"))))
         (element-filename (string-join (list *testdir* "file.pcb")
                                        file-name-separator-string
                                        'infix))
         (element-file-contents
          "  #some comment

          \t   PCB"))
    ;; Create the element file.
    (with-output-to-file element-filename
      (lambda () (display element-file-contents)))

    (let* ((*element (pkg-line->element "PKG_DIP14(DIP14,U100,unknown)"))
           (<stderr>
            (with-error-to-string
              (lambda ()
                (let ((<result>
                       (insert-file-element *output-file element-filename *element)))
                  (test-assert (not <result>)))))))
      ;; Skip the test until the C functions producing this text
      ;; will be replaced with Scheme ones.  The main issue with
      ;; foreign C functions is that they do not use Scheme ports
      ;; and output errors directly to stderr.
      (test-skip 1)
      (test-assert (string-contains <stderr>
                                    (format #f "Warning: ~A appears to be a PCB layout file. Skipping.\n"
                                            element-filename))))
    (sch2pcb_close_file *output-file))
  ;; Clean up.
  (config-test-teardown))
(test-end)


(test-begin "insert-file-element-really-dir")
(test-group-with-cleanup "insert-file-element-really-dir-grp"
  (config-test-setup)
  ;; Test an exit status and side effects of the function when
  ;; called with a file element that is really a PCB file.
  (let* ((*output-file
          (sch2pcb_open_file_to_write
           (string->pointer (string-append *testdir*
                                           file-name-separator-string
                                           "output.pcb"))))
         (element-filename (string-join (list *testdir* "file.pcb")
                                        file-name-separator-string
                                        'infix))
         (element-file-contents
          "  #some comment

          \t   PCB"))
    ;; Create the element, it's really a directory.
    (mkdir element-filename)

    (let* ((*element (pkg-line->element "PKG_DIP14(DIP14,U100,unknown)"))
           (<stderr>
            (with-error-to-string
              (lambda ()
                (let ((<result>
                       (insert-file-element *output-file element-filename *element)))
                  (test-assert (not <result>)))))))
      ;; Skip the test until Scheme code will check whether the
      ;; path is a directory.
      (test-skip 1)
      (test-assert (string-contains <stderr>
                                    (format #f "ERROR: ~A is a directory. Skipping.\n"
                                            element-filename))))
    (sch2pcb_close_file *output-file))
  ;; Clean up.
  (config-test-teardown))
(test-end)


(test-begin "insert-file-element-non-readable")
(test-group-with-cleanup "insert-file-element-non-readable-grp"
  (config-test-setup)
  ;; Test an exit status and side effects of the function when
  ;; called with a file element that is really a PCB file.
  (let* ((*output-file
          (sch2pcb_open_file_to_write
           (string->pointer (string-append *testdir*
                                           file-name-separator-string
                                           "output.pcb"))))
         (element-filename (string-join (list *testdir* "file.fp")
                                        file-name-separator-string
                                        'infix))
         (element-file-contents
          " # comment
Element(0x00 \"DIP8 package\" \"\" \"DIP8\" 220 100 3 100 0x00)
(
        Pin(50 50 60 28 \"1\" 0x101)
        Mark(50 50)
)
"))
    ;; Create the element.
    (with-output-to-file element-filename
      (lambda () (display "")))
    ;; Make the element file non-readable.
    (chmod element-filename #o000)

    (let* ((*element (pkg-line->element "PKG_DIP14(DIP14,U100,unknown)"))
           (<stderr>
            (with-error-to-string
              (lambda ()
                (let ((<result>
                       (insert-file-element *output-file element-filename *element)))
                  (test-assert (not <result>)))))))
      ;; Skip the test until Scheme code will check whether the
      ;; path is a readable file.
      (test-skip 1)
      (test-assert (string-contains <stderr>
                                    (format #f "ERROR: ~A is not readable. Skipping.\n"
                                            element-filename))))
    (sch2pcb_close_file *output-file))
  ;; Clean up.
  (config-test-teardown))
(test-end)


(test-begin "insert-file-element")
(test-group-with-cleanup "insert-file-element-grp"
  (config-test-setup)
  ;; Test an exit status and side effects of the function when
  ;; called with a file element that is really a PCB file.
  (let* ((*output-file
          (sch2pcb_open_file_to_write
           (string->pointer (string-append *testdir*
                                           file-name-separator-string
                                           "output.pcb"))))
         (element-filename (string-join (list *testdir* "file.fp")
                                        file-name-separator-string
                                        'infix))
         (element-file-contents
          " # comment
Element(0x00 \"DIP8 package\" \"\" \"DIP8\" 220 100 3 100 0x00)
(
        Pin(50 50 60 28 \"1\" 0x101)
        Mark(50 50)
)
"))
    ;; Create the element.
    (with-output-to-file element-filename
      (lambda () (display element-file-contents)))

    (let* ((*element (pkg-line->element "PKG_DIP14(DIP14,U100,unknown)"))
           (<result>
            (insert-file-element *output-file element-filename *element)))
      (test-assert <result>))
    (sch2pcb_close_file *output-file)
    (test-equal "Element(0x00 \"DIP14\" \"U100\" \"unknown\" 0 0 3 100 0x00)
(
        Pin(50 50 60 28 \"1\" 0x101)
        Mark(50 50)
)
"
      (with-input-from-file (string-append *testdir*
                                           file-name-separator-string
                                           "output.pcb")
        (lambda () (get-string-all (current-input-port))))))
  ;; Clean up.
  (config-test-teardown))
(test-end)
