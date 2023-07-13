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

(test-begin "insert-file-element-missing-file")
(test-group-with-cleanup "insert-file-element-missing-file-grp"
  (config-test-setup)
  ;; Test an exit status and side effects of the function when
  ;; called with some non-existing element file.
  (let* ((element-filename "some-non-existing-element.fp")
         (*element (pkg-line->element "PKG_DIP14(DIP14,U100,unknown)"))
         (<stderr>
          (with-error-to-string
            (lambda ()
              (let ((<result>
                     (insert-file-element element-filename *element)))
                (test-assert (not <result>)))))))
    (test-assert (string-contains <stderr>
                                  (string-append "insert-file-element(): can't open "
                                                 element-filename)))
    (test-assert (string-contains <stderr> "No such file or directory")))
  ;; Clean up.
  (config-test-teardown))
(test-end)


(test-begin "insert-file-element-really-pcb-file")
(test-group-with-cleanup "insert-file-element-really-pcb-file-grp"
  (config-test-setup)
  ;; Test an exit status and side effects of the function when
  ;; called with a file element that is really a PCB file.
  (let ((element-filename (string-join (list *testdir* "file.pcb")
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
                       (insert-file-element element-filename *element)))
                  (test-assert (not <result>)))))))
      (test-assert (string-contains <stderr>
                                    (format #f "Warning: ~A appears to be a PCB layout file. Skipping.\n"
                                            element-filename)))))
  ;; Clean up.
  (config-test-teardown))
(test-end)


(test-begin "insert-file-element-really-dir")
(test-group-with-cleanup "insert-file-element-really-dir-grp"
  (config-test-setup)
  ;; Test an exit status and side effects of the function when
  ;; called with a file element that is really a PCB file.
  (let ((element-filename (string-join (list *testdir* "file.pcb")
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
                       (insert-file-element element-filename *element)))
                  (test-assert (not <result>)))))))
      (test-assert (string-contains <stderr>
                                    (string-append "insert-file-element(): can't open "
                                                   element-filename)))
      (test-assert (string-contains <stderr> "Is a directory"))))
  ;; Clean up.
  (config-test-teardown))
(test-end)


(test-begin "insert-file-element-non-readable")
(test-group-with-cleanup "insert-file-element-non-readable-grp"
  (config-test-setup)
  ;; Test an exit status and side effects of the function when
  ;; called with a file element that is really a PCB file.
  (let ((element-filename (string-join (list *testdir* "file.fp")
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
                       (insert-file-element element-filename *element)))
                  (test-assert (not <result>)))))))
      (test-assert (string-contains <stderr>
                                    (string-append "insert-file-element(): can't open "
                                                   element-filename)))
      (test-assert (string-contains <stderr> "Permission denied"))))
  ;; Clean up.
  (config-test-teardown))
(test-end)


(test-begin "insert-file-element")
(test-group-with-cleanup "insert-file-element-grp"
  (config-test-setup)
  ;; Test an exit status and side effects of the function when
  ;; called with a file element that is really a PCB file.
  (let ((element-filename (string-join (list *testdir* "file.fp")
                                       file-name-separator-string
                                       'infix))
        (element-file-contents
         " # Test that comment lines are omitted
  # and leading whitespaces are trimmed.
 \t Element(0x00 \"DIP8 package\" \"\" \"DIP8\" 220 100 3 100 0x00)
(
        Pin(50 50 60 28 \"1\" 0x101)
        Mark(50 50)
)
"))
    ;; Create the element.
    (with-output-to-file element-filename
      (lambda () (display element-file-contents)))

    (let* ((*element (pkg-line->element "PKG_DIP14(DIP14,U100,unknown)"))
           (<stdout>
            (with-output-to-string
              (lambda ()
                (let ((<result>
                       (insert-file-element element-filename *element)))
                  (test-assert <result>))))))
      (test-equal "Element(0x00 \"DIP14\" \"U100\" \"unknown\" 0 0 3 100 0x00)
(
        Pin(50 50 60 28 \"1\" 0x101)
        Mark(50 50)
)
"
        <stdout>)))
  ;; Clean up.
  (config-test-teardown))
(test-end)
