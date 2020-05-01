(use-modules (lepton library component))

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
    (mkdir (make-filename *toplevel-dir* "b" "b" "c"))
    ;; Make symbol files.
    (touch (make-filename *toplevel-dir* "toplevel.sym"))
    (touch (make-filename *toplevel-dir* "a" "a.sym"))
    (touch (make-filename *toplevel-dir* "b" "b.sym"))
    (touch (make-filename *toplevel-dir* "b" "b" "bb.sym"))
    (touch (make-filename *toplevel-dir* "b" "b" "c" "bbc.sym")))

  ;; Test body.
  (lambda ()
    ;; First, reset the component library to be sure it is empty.
    (reset-component-library)
    (component-library-search *toplevel-dir*)

    ;; Just check that all symbols but the toplevel one exist in
    ;; the component library created.
    (test-assert
        (and (absolute-component-name "toplevel.sym")
             (absolute-component-name "a.sym")
             (absolute-component-name "b.sym")
             (absolute-component-name "bb.sym")
             (absolute-component-name "bbc.sym")))

    ;; The same as above but the directory ends with "/".
    (reset-component-library)
    (component-library-search (string-append *toplevel-dir*
                                             file-name-separator-string))

    (test-assert (absolute-component-name "toplevel.sym"))


    ;; Test of libraries with same directory basenames.
    (reset-component-library)
    (component-library-search (string-append (make-filename *toplevel-dir* "b" "b")
                                             file-name-separator-string)
                              "myprefix")
    (test-assert (absolute-component-name "bbc.sym"))

    ;; Test of toplevel library with symbols without prefix.
    (reset-component-library)

    (component-library-search *toplevel-dir*)

    (let ((libs (filter
                 (lambda (lib) (string= (symbol-library-path lib) *toplevel-dir*))
                 (component-libraries))))
      (test-equal (map symbol-library-name libs)
        (list (basename *toplevel-dir*))))

    ;; Test of toplevel library with symbols with prefix.
    (reset-component-library)

    (component-library-search *toplevel-dir* "xxx")

    (let ((libs (component-libraries))
          ;; On *nix the following should result in
          ;; '("xxx" "xxxa" "xxxb" "xxxb/b" "xxxb/b/c")
          (result (map (lambda (x) (string-append "xxx" x))
                       (map (lambda (ls) (string-join ls file-name-separator-string))
                            '(("") ("a") ("b") ("b" "b") ("b" "b" "c"))))))
      (test-eq (length libs) 5)
      (test-equal (sort (map symbol-library-name libs) string<)
        result)))

  ;; Get rid of the test directory.
  (lambda ()
    (system* "rm" "-rf" *toplevel-dir*)))
(test-end "component-library-search")
