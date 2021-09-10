;;; Test Scheme procedures related to component objects.

(use-modules (ice-9 rdelim)
             (lepton attrib)
             (lepton library)
             (lepton object)
             (lepton page))

(test-begin "component" 13)

(let* ((a (make-component "test component" '(1 . 2) 0 #t #f))
       (b (copy-object a)))

  (test-equal 'complex (object-type a))
  (test-assert (object-type? a 'complex))
  (test-assert (not (object-type? a 'x)))

  (test-assert (component? a))

  (test-equal "test component" (component-basename a))
  (test-equal '(1 . 2) (component-position a))
  (test-equal 0 (component-angle a))
  (test-assert (component-mirror? a))
  (test-assert (not (component-locked? a)))

  (test-equal (list (component-basename a) (component-position a)
                    (component-angle a) (component-mirror? a)
                    (component-locked? a))
    (component-info a))

  ;; Test the copy.
  (test-assert (component? b))
  (test-equal (component-info a) (component-info b))
  (test-equal (component-angle a) (component-angle b))
  (test-equal (component-basename a) (component-basename b))
  (test-equal (component-filename a) (component-filename b))
  (test-equal (component-locked? a) (component-locked? b))
  (test-equal (component-mirror? a) (component-mirror? b))
  (test-equal (component-position a) (component-position b))
  (test-equal (component-contents a) (component-contents b))

  (set-component! a '(3 . 4) 90 #f #t)

  (test-equal '(3 . 4) (component-position a))
  (test-equal 90 (component-angle a))
  (test-assert (not (component-mirror? a)))
  (test-assert (component-locked? a))

  (test-assert-thrown 'misc-error
                      (set-component! a '(3 . 4) 45 #f #t)))

(test-end "component")


(test-begin "component-append" 6)

(let ((A (make-component "test component" '(1 . 2) 0 #t #f))
      (B (make-component "test component" '(1 . 2) 0 #t #f))
      (x (make-line '(0 . 0) '(2 . 0)))
      (y (make-line '(0 . 0) '(0 . 2))))

  (test-equal '() (component-contents A))

  (test-equal A (component-append! A x))
  (test-equal (list x) (component-contents A))

  (component-append! A x)
  (test-equal (list x) (component-contents A))

  (component-append! A y)
  (test-equal (list x y) (component-contents A))

  (test-assert-thrown 'object-state
                      (component-append! B x)))

(test-end "component-append")


(test-begin "component-remove" 5)

(let ((A (make-component "test component" '(1 . 2) 0 #t #f))
      (B (make-component "test component" '(1 . 2) 0 #t #f))
      (x (make-line '(0 . 0) '(2 . 0)))
      (y (make-line '(0 . 0) '(0 . 2)))
      (z (make-line '(1 . 0) '(2 . 2))))

  (component-append! A x)
  (test-equal A (component-remove! A x))
  (test-equal '() (component-contents A))
  (component-remove! A x)
  (component-remove! B x)

  (component-append! A x y)
  (component-remove! A x y)
  (test-equal '() (component-contents A))

  (component-append! A x y)
  (component-remove! A x)
  (test-equal (list y) (component-contents A))

  (test-assert-thrown 'object-state
                      (component-remove! B y)))

(test-end "component-remove")


(test-begin "component-append/page" 3)

(let ((P (make-page "/test/page/A"))
      (A (make-component "test component" '(1 . 2) 0 #t #f))
      (x (make-line '(0 . 0) '(2 . 0)))
      (y (make-line '(0 . 0) '(0 . 2))))
  (dynamic-wind
    (lambda () #t)
    (lambda ()
      (page-append! P x)
      (test-assert-thrown 'object-state
                          (component-append! A x))

      (page-append! P A)
      (test-assert-thrown 'object-state
                          (component-append! A x))

      (component-append! A y)
      (test-equal (list y) (component-contents A)))

    (lambda () (close-page! P))))

(test-end "component-append/page")


(test-begin "component-remove/page" 3)

(let ((P (make-page "/test/page/A"))
      (A (make-component "test component" '(1 . 2) 0 #t #f))
      (x (make-line '(0 . 0) '(2 . 0)))
      (y (make-line '(0 . 0) '(0 . 2))))
  (dynamic-wind
    (lambda () #t)
    (lambda ()
      ;; Test that if a primitive object is attached directly to
      ;; a page, attempting to remove it from a component
      ;; doesn't work.
      (page-append! P x)
      (test-assert-thrown 'object-state
                          (component-remove! A x))

      (page-append! P A)
      (test-assert-thrown 'object-state
                          (component-remove! A x))

      ;; Test that you can remove primitive objects from a
      ;; component that is attached to a page.
      (component-append! A y)
      (component-remove! A y)
      (test-equal '() (component-contents A)))

    (lambda () (close-page! P))))

(test-end "component-remove/page")


(test-begin "component-translate" 2)

(let* ((A (make-component "test component" '(0 . 0) 0 #t #f))
       (x (make-box '(0 . 2) '(2 . 0))))

  (component-append! A x)
  (set-component! A '(1 . 1) 0 #t #f)
  (test-equal '(1 . 3) (box-top-left x))
  (test-equal '(3 . 1) (box-bottom-right x)))

(test-end "component-translate")


(test-begin "component-remove-attrib" 2)

(let ((comp (make-component "test component" '(1 . 2) 0 #t #f))
      (pin (make-net-pin '(0 . 0) '(100 . 0)))
      (attrib (make-text '(0 . 0) 'lower-left 0 "name=x" 10 #t 'both)))
  (component-append! comp pin attrib)
  (attach-attribs! pin attrib)
  (test-assert-thrown 'object-state (component-remove! comp pin))
  (test-assert-thrown 'object-state (component-remove! comp attrib)))

(test-end "component-remove-attrib")


;; Set up component library, making blatant assumptions about the
;; directory layout.
(component-library (string-join (list (getenv "srcdir") "../../symbols/sym/analog") "/")
                   "Basic devices")

(define object-type-func-alist
  `((arc . ,arc-info)
    (box . ,box-info)
    (bus . ,line-info)
    (net . ,line-info)
    (line . ,line-info)
    (pin . ,line-info)
    (circle . ,circle-info)
    (complex . ,component-info)
    (path . ,path-info)
    (picture . ,picture-info)
    (text . ,text-info)))

(define (object-info object)
  (let* ((type (object-type object))
         (info-func (assq-ref object-type-func-alist type)))
    (cons type (info-func object))))

(test-begin "component/library")

(let* ((A (make-component/library "resistor-1.sym" '(1 . 2) 0 #t #f))
       (B (copy-object A)))

  (test-assert (component? A))
  (test-equal '(1 . 2) (component-position A))
  (test-equal 0 (component-angle A))
  (test-assert (component-mirror? A))
  (test-assert (not (component-locked? A)))

  (test-equal "resistor-1.sym" (component-basename A))

  (test-assert (not (null? (component-contents A))))

  ;; Test the copy.
  (test-assert (component? B))
  (test-equal (component-info A) (component-info B))
  (test-equal (component-angle A) (component-angle B))
  (test-equal (component-basename A) (component-basename B))
  (test-equal (component-filename A) (component-filename B))
  (test-equal (component-locked? A) (component-locked? B))
  (test-equal (component-mirror? A) (component-mirror? B))
  (test-equal (component-position A) (component-position B))
  (test-equal (map object-info (component-contents A))
    (map object-info (component-contents B))))

(test-end "component/library")


(test-begin "component/library-invalid")

(let ((C (make-component/library "invalid-component-name" '(1 . 2) 0 #t #f)))
  (test-assert (not C)))

(test-end "component/library-invalid")


;; Clear component library again
(reset-component-library)


(test-begin "object-component" 2)

(let* ((A (make-component "test component" '(0 . 0) 0 #t #f))
       (x (make-box '(0 . 2) '(2 . 0))))
  (test-equal #f (object-component x))
  (component-append! A x)
  (test-equal A (object-component x)))

(test-end "object-component")


;; New symbol library with one component containing only one
;; line primitive
(component-library-funcs
  (lambda ()       ; list-symbol-names function
    '("line.sym"))
  (lambda (name)   ; get-symbol-by-name function
    (let ((page (make-page "/test/page/line")))
      (page-append! page (make-line '(1 . 2) '(3 . 4)))
      (let ((s (page->string page)))
        (close-page! page)
        s)))
  "Test symbols"   ; Library name
  )

;; Test the 'set-component-with-transform!' procedure. This test
;; includes testing of mirroring and rotation of a component's
;; primitives.

(test-begin "set-component-with-transform" 2)

(let ((P (make-page "/test/page/A"))
      (C (make-component/library "line.sym" '(0 . 0) 90 #f #f)))

  (dynamic-wind
    (lambda () #t)
    (lambda ()
      (page-append! P C)

      (set-component-with-transform! C '(0 . 0) 90 #t #f)

      (test-equal '(-2 . -1) (line-start (car (component-contents C))))
      (test-equal '(-4 . -3) (line-end   (car (component-contents C))))
      )
    (lambda () (close-page! P))))

(test-end "set-component-with-transform")


;; Clear component library again
(reset-component-library)


;;; Test component file names.
(test-begin "component-filename")
(let* ((fname1  (format #f "~a.sym" (tmpnam)))
       (symdir (dirname  fname1))
       (basename1 (basename fname1))
       (basename2 "does-not-exist")
       (position '(0 . 0))
       (angle 0)
       (mirror-flag #f)
       (locked-flag #f))

  (define (make-real-component)
    ( with-output-to-file fname1
      ( lambda()
        ( format #t "v 20191003 2~%" )
        ( format #t "B 0 0 500 500 3 10 1 0 -1 -1 0 -1 -1 -1 -1 -1~%" )
        ( format #t "T 0 600 21 6 1 0 0 0 1~%" )
        ( format #t "refdes=R?" )
        )
      )

    ( component-library symdir )

    ;; return:
    (make-component/library basename1
                            position
                            angle
                            mirror-flag
                            locked-flag))

  (define (make-bogus-component)
    ;; return:
    (make-component basename2
                    position
                    angle
                    mirror-flag
                    locked-flag))

  #|
  ( format #t "cwd:     [~a]~%" (getcwd) )            ; [debug]
  ( format #t "symdir:  [~a]~%" symdir )              ; [debug]
  ( format #t "symname: [~a]~%" basename1 )           ; [debug]
  ( format #t "fname1:  [~a]~%" fname1 )              ; [debug]
  |#

  (let ((temp-component (make-real-component))
        (non-existing-component (make-bogus-component)))

    (test-group-with-cleanup "component-filename-grp"

      (test-equal (component-filename temp-component) fname1)
      (test-assert (not (component-filename non-existing-component)))
      ;; Clean up.
      (begin
        (reset-component-library)
        (delete-file fname1)))))
(test-end "component-filename")


;;; Test component-library-command().
(define script-contents
  (with-input-from-file (string-append (getenv "abs_top_srcdir")
                                       "/docs/manual/cmd-component.sh")
    read-string))

(test-begin "component-library-command")
(let* ((script-name (tmpnam))
       (get-command script-name)
       (list-command (string-append script-name " -l")))

  (with-output-to-file get-command
    (lambda () (display script-contents)))

  (chmod get-command #o755)

  (component-library-command list-command get-command "make-sym-lib")

  (let ((C1 (make-component/library "symname1.sym" '(0 . 0) 0 #f #f))
        (C2 (make-component/library "symname2.sym" '(1000 . 1000) 0 #f #f)))

    (test-group-with-cleanup "component-library-command-grp"

      (test-assert (component? C1))
      (test-assert (component? C2))
      ;; Clean up.
      (delete-file get-command))))
(test-end "component-library-command")

(test-begin "object-component-wrong-argument")

(test-assert-thrown 'wrong-type-arg (object-component 'a))

(test-end "object-component-wrong-argument")
