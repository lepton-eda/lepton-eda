(use-modules (srfi srfi-64)
             (lepton library component))

;;; Helper procedures.

;;; Make filenames by joining strings and using filename separator
;;; as a delimiter between them.
(define (make-filename . args)
  (string-join args file-name-separator-string 'infix))


;;; Create symbol files.
(define (touch file)
  (with-output-to-file file (lambda () (display ""))))


;;; Main testing directory.
(define *toplevel-dir*
  (make-filename (getcwd) "component-library-search-test"))

(test-begin "component-library-search" 1)
(dynamic-wind
  ;; Make test directory with all necessary subdirectories and files.  We
  ;; use a toplevel directory with two level folded subdirectories
  ;; and symbol files in them.
  (lambda ()
    (mkdir *toplevel-dir*)
    (mkdir (make-filename *toplevel-dir* "a"))
    (mkdir (make-filename *toplevel-dir* "b"))
    (mkdir (make-filename *toplevel-dir* "b" "b"))
    ;; Make symbol files.
    (touch (make-filename *toplevel-dir* "toplevel.sym"))
    (touch (make-filename *toplevel-dir* "a" "a.sym"))
    (touch (make-filename *toplevel-dir* "b" "b.sym"))
    (touch (make-filename *toplevel-dir* "b" "b" "bb.sym")))

  ;; Test body.
  (lambda ()
    ;; First, reset the component library to be sure it is empty.
    (reset-component-library)
    (component-library-search *toplevel-dir*)

    ;; Just check that all symbols but the toplevel one exist in
    ;; the component library created.
    (test-assert
        (and (absolute-component-name "a.sym")
             (absolute-component-name "b.sym")
             (absolute-component-name "bb.sym")))

    (test-assert (not (absolute-component-name "toplevel.sym")))

    ;; The same as above but the directory ends with "/".
    (reset-component-library)
    (component-library-search (string-append *toplevel-dir*
                                             file-name-separator-string))

    (test-assert (not (absolute-component-name "toplevel.sym"))))

  ;; Get rid of the test directory.
  (lambda ()
    (system* "rm" "-rf" *toplevel-dir*)))
(test-end "component-library-search")
