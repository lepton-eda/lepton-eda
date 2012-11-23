;; Test Scheme procedures for working with configuration.

(use-modules (unit-test)
             (geda os)
             (geda config))

(or (defined? 'define-syntax)
    (use-modules (ice-9 syncase)))

(define *testdir* (string-append (getcwd) separator "t0402-tmp"))
(define *testdirconf* (string-append *testdir* separator "geda.conf"))
(define *testdirA* (string-append *testdir* separator "A"))
(define *testdirAconf* (string-append *testdirA* separator "geda.conf"))
(define *testdirB* (string-append *testdir* separator "B"))
(define *testdirBconf* (string-append *testdirB* separator "geda.conf"))

;; Setup/teardown of directories / files needed by tests
(define (config-test-setup)
  (config-test-teardown)
  (mkdir *testdir*)
  (mkdir *testdirA*)
  (mkdir *testdirB*)
  (with-output-to-file *testdirconf* newline)
  (with-output-to-file *testdirAconf* newline))
(define (config-test-teardown)
  (system* "rm" "-rf" *testdir*))

(define-syntax begin-config-test
  (syntax-rules ()
    ((_ name . test-forms)
     (begin-test name
       (dynamic-wind
         config-test-setup
         (lambda () . test-forms)
         config-test-teardown)))))

(begin-test 'default-config-context
  (let ((cfg (default-config-context)))
    (assert-true (config? cfg))
    (assert-equal cfg (default-config-context))
    (assert-equal #f (config-filename cfg))
    (assert-equal #f (config-parent cfg))
    (assert-true (config-trusted? cfg))))

(begin-test 'system-config-context
  (let ((cfg (system-config-context)))
    (assert-true (config? cfg))
    (assert-equal cfg (system-config-context))
    (assert-equal (default-config-context) (config-parent cfg))
    (assert-true (config-trusted? cfg))))

(begin-test 'user-config-context
  (let ((cfg (user-config-context)))
    (assert-true (config? cfg))
    (assert-equal cfg (user-config-context))
    (assert-equal (system-config-context) (config-parent cfg))
    (assert-true (config-trusted? cfg))))

(begin-config-test 'path-config-context
  ;; Unfortunately, there's no reliable way of testing the "recurse
  ;; all the way to root and then give up" functionality, because we
  ;; can't control the contents of the superdirectories of the CWD.
  (assert-thrown 'system-error (path-config-context "/__missing/file/"))
  (let ((c (path-config-context *testdir*))
        (a (path-config-context *testdirA*))
        (b (path-config-context *testdirB*)))

    (assert-true (config? a))
    (assert-true (config? b))
    (assert-true (config? c))

    (assert-equal b c)

    (assert-equal *testdirconf* (config-filename c))
    (assert-equal *testdirAconf* (config-filename a))

    (assert-equal (user-config-context) (config-parent a))
    (assert-equal #f (config-trusted? a))))

(begin-config-test 'config-load
  (let ((a (path-config-context *testdirA*)))
    (assert-equal #f (config-loaded? a))
    (assert-equal a (config-load! a))
    (assert-true (config-loaded? a))
    (chmod *testdirAconf* #o000) ;; Make conf unreadable
    (assert-thrown 'system-error (config-load! a)))

  (assert-thrown 'system-error (config-load! (default-config-context))))

(begin-config-test 'config-save
  (let ((a (path-config-context *testdirA*)))
    (assert-equal a (config-save! a)))

  (assert-thrown 'system-error (config-save! (default-config-context)))
  ;; FIXME test writing a file without permissions to write it.
  )

(begin-config-test 'config-parent
  (let ((a (path-config-context *testdirA*))
        (b (path-config-context *testdir*)))
    (assert-equal (user-config-context) (config-parent a))
    (assert-equal (user-config-context) (config-parent b))

    (assert-equal a (set-config-parent! a #f))
    (assert-equal #f (config-parent a))

    (assert-equal a (set-config-parent! a b))
    (assert-equal b (config-parent a))

    ;; Check that configuration values are inherited from parent
    (assert-thrown 'config-error (config-boolean a "foo" "bar"))
    (set-config! b "foo" "bar" #t)
    (assert-true (config-boolean a "foo" "bar"))

    ;; Check that set-config-parent! refuses to form loops
    (assert-equal b (set-config-parent! b a))
    (assert-equal (user-config-context) (config-parent b))

    (assert-equal a (set-config-parent! a (user-config-context)))
    (assert-equal (user-config-context) (config-parent a))
    ))

(begin-config-test 'config-trust
  (let ((a (path-config-context *testdirA*)))
    (assert-equal #f (config-trusted? a))
    (assert-equal (user-config-context) (config-parent a))
    (assert-true (config-trusted? (user-config-context)))
    (assert-equal (user-config-context) (config-trusted-context a))

    (assert-equal a (set-config-trusted! a #t))
    (assert-true (config-trusted? a))
    (assert-equal a (config-trusted-context a))

    (assert-equal a (set-config-trusted! a #f))
    (assert-equal #f (config-trusted? a))))

(begin-config-test 'config-changed
  (let ((a (path-config-context *testdirA*)))
    (config-load! a)
    (assert-equal #f (config-changed? a))
    (set-config! a "foo" "bar" #t)
    (assert-true (config-changed? a))
    (config-save! a)
    (assert-equal #f (config-changed? a))
    (set-config! a "foo" "bar" #f)
    (assert-true (config-changed? a))
    (config-load! a)
    (assert-equal #f (config-changed? a))))

(begin-config-test 'config-groups
  (let ((a (path-config-context *testdir*))
        (b (path-config-context *testdirA*)))
    (dynamic-wind
       (lambda () (set-config-parent! b a))
       (lambda ()

         (config-load! a)
         (config-load! b)

         (assert-equal '() (config-groups a))
         (assert-equal '() (config-groups b))
         (assert-equal #f (config-has-group? a "foo"))
         (assert-equal #f (config-has-group? b "foo"))

         (set-config! a "foo" "bar" #t)
         (assert-equal '("foo") (config-groups a))
         (assert-equal '("foo") (config-groups b))
         (assert-true (config-has-group? a "foo"))
         (assert-true (config-has-group? b "foo"))
         (assert-equal #f (config-has-group? a "fizz"))

         (set-config! b "fizz" "bam" #t)
         (assert-equal '("foo") (config-groups a))
         (assert-equal '("fizz" "foo") (config-groups b))
         (assert-equal #f (config-has-group? a "fizz"))
         (assert-true (config-has-group? b "fizz")) )
       (lambda () (set-config-parent! b (user-config-context))))))

(begin-config-test 'config-source
  (let ((a (path-config-context *testdir*))
        (b (path-config-context *testdirA*)))
    (config-load! a)
    (config-load! b)

    (set-config! a "foo" "bar" #t)
    (assert-equal a (config-source a "foo" "bar"))
    (assert-thrown 'config-error (config-source b "foo" "bar"))

    (dynamic-wind
        (lambda () (set-config-parent! b a))
        (lambda ()
          (assert-equal a (config-source a "foo" "bar"))
          (assert-equal a (config-source b "foo" "bar")) )
        (lambda () (set-config-parent! b (user-config-context))))
    ))

(begin-config-test 'config-keys
  (let ((a (path-config-context *testdir*))
        (b (path-config-context *testdirA*)))
    (dynamic-wind
       (lambda () (set-config-parent! b a))
       (lambda ()
         (set-config-parent! b a)
         (config-load! a)
         (config-load! b)

         (assert-thrown 'config-error '() (config-keys a "foo"))

         (set-config! a "foo" "bar" #t)
         (assert-equal '("bar") (config-keys a "foo"))
         (assert-equal '("bar") (config-keys b "foo"))
         (assert-true (config-has-key? a "foo" "bar"))
         (assert-true (config-has-key? b "foo" "bar"))
         (assert-equal #f (config-has-key? a "foo" "bam"))

         (set-config! b "foo" "bam" #t)
         (assert-equal '("bar") (config-keys a "foo"))
         (assert-equal '("bam" "bar") (config-keys b "foo"))
         (assert-true (config-has-key? b "foo" "bam"))
         (assert-equal #f (config-has-key? a "foo" "bam")))

       (lambda () (set-config-parent! b (user-config-context))))))

(begin-config-test 'config-boolean
  (let ((a (path-config-context *testdir*)))
    (config-load! a)
    (assert-equal a (set-config! a "foo" "bar" #t))
    (assert-equal #t (config-boolean a "foo" "bar"))
    (assert-equal a (set-config! a "foo" "bar" #f))
    (assert-equal #f (config-boolean a "foo" "bar"))
    (assert-equal '(#f) (config-boolean-list a "foo" "bar"))

    (assert-equal a (set-config! a "foo" "bar" '(#t #f)))
    (assert-equal '(#t #f) (config-boolean-list a "foo" "bar"))))

(begin-config-test 'config-int
  (let ((a (path-config-context *testdir*)))
    (config-load! a)
    (assert-equal a (set-config! a "foo" "bar" 42))
    (assert-equal 42 (config-int a "foo" "bar"))
    (assert-equal '(42) (config-int-list a "foo" "bar"))

    (assert-equal a (set-config! a "foo" "bar" '(42 144)))
    (assert-equal '(42 144) (config-int-list a "foo" "bar"))))

(begin-config-test 'config-real
  (let ((a (path-config-context *testdir*)))
    (config-load! a)
    (assert-equal a (set-config! a "foo" "bar" 42.0))
    (assert-equal 42.0 (config-real a "foo" "bar"))
    (assert-equal '(42.0) (config-real-list a "foo" "bar"))

    (assert-equal a (set-config! a "foo" "bar" '(42.0 144.0)))
    (assert-equal '(42.0 144.0) (config-real-list a "foo" "bar"))))

(begin-config-test 'config-string
  (let ((a (path-config-context *testdir*)))
    (config-load! a)
    (assert-equal a (set-config! a "foo" "bar" "wibble"))
    (assert-equal "wibble" (config-string a "foo" "bar"))
    (assert-equal '("wibble") (config-string-list a "foo" "bar"))

    (assert-equal a (set-config! a "foo" "bar" '("wib;ble" "wobble")))
    (assert-equal '("wib;ble" "wobble") (config-string-list a "foo" "bar"))))

(begin-config-test 'config-get-set-errors
  (let ((a (path-config-context *testdir*)))
    (config-load! a)
    (assert-thrown 'wrong-type-arg (set-config! a "foo" "bar" 'BAD-VALUE))
    (assert-thrown 'wrong-type-arg (set-config! a "foo" "bar" '(BAD-VALUE)))
    (assert-thrown 'wrong-type-arg (set-config! a "foo" "bar" '(1 "foo")))

    (set-config! a "foo" "bar" "wibble")
    (assert-thrown 'config-error (config-boolean a "foo" "bar"))
    (assert-thrown 'config-error (config-int a "foo" "bar"))
    (assert-thrown 'config-error (config-real a "foo" "bar"))))

(begin-config-test 'config-events
  (let* ((a (path-config-context *testdir*))
         (call-count 0)
         (handler  (lambda (cfg group key)
                     (set! call-count (1+ call-count)))))
    (assert-equal a (add-config-event! a handler))
    (set-config! a "foo" "bar" #t)
    (assert-equal 1 call-count)

    ;; Check that a handler can't be registered multiple times with
    ;; the same context.
    (assert-equal a (add-config-event! a handler))
    (set-config! a "foo" "bar" #t)
    (assert-equal 2 call-count)

    ;; Handler removal
    (assert-equal a (remove-config-event! a handler))
    (set-config! a "foo" "bar" #t)
    (assert-equal 2 call-count)))
