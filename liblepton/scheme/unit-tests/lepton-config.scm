;;; Test Scheme procedures for working with configuration.

(use-modules (srfi srfi-1)
             (lepton config)
             (lepton os))

(define *testdir*      (string-append (getcwd)   file-name-separator-string "t0402-tmp"))
(define *testdirconf*  (string-append *testdir*  file-name-separator-string "lepton.conf"))
(define *testdirA*     (string-append *testdir*  file-name-separator-string "A"))
(define *testdirAconf* (string-append *testdirA* file-name-separator-string "lepton.conf"))
(define *testdirB*     (string-append *testdir*  file-name-separator-string "B"))
(define *testdirBconf* (string-append *testdirB* file-name-separator-string "lepton.conf"))

;;; Setup/teardown of directories / files needed by tests
(define (config-test-setup)
  (mkdir *testdir*)
  (mkdir *testdirA*)
  (mkdir *testdirB*)
  (with-output-to-file *testdirconf* newline)
  (with-output-to-file *testdirAconf* newline))

(define (config-test-teardown)
  (system* "rm" "-rf" *testdir*))


(test-begin "default-config-context")

(let ((cfg (default-config-context)))
  (test-assert (config? cfg))
  (test-equal cfg (default-config-context))
  (test-equal #f (config-filename cfg))
  (test-equal #f (config-parent cfg))
  (test-assert (config-trusted? cfg)))

(test-end "default-config-context")


(test-begin "system-config-context")

(let ((cfg (system-config-context)))
  (test-assert (config? cfg))
  (test-equal cfg (system-config-context))
  (test-equal (default-config-context) (config-parent cfg))
  (test-assert (config-trusted? cfg)))

(test-end "system-config-context")


(test-begin "user-config-context")

(let ((cfg (user-config-context)))
  (test-assert (config? cfg))
  (test-equal cfg (user-config-context))
  (test-equal (system-config-context) (config-parent cfg))
  (test-assert (config-trusted? cfg)))

(test-end "user-config-context")


(test-begin "path-config-context")
(test-group-with-cleanup "path-config-context-grp"
  (config-test-setup)
  ;; Unfortunately, there's no reliable way of testing the "recurse
  ;; all the way to root and then give up" functionality, because we
  ;; can't control the contents of the superdirectories of the CWD.
  (test-equal "/lepton.conf"
    (config-filename (path-config-context "/__missing/file/")))
  (let ((c (path-config-context *testdir*))
        (a (path-config-context *testdirA*))
        (b (path-config-context *testdirB*)))

    (test-assert (config? a))
    (test-assert (config? b))
    (test-assert (config? c))

    (test-equal b c)

    (test-equal *testdirconf* (config-filename c))
    (test-equal *testdirAconf* (config-filename a))

    (test-equal (user-config-context) (config-parent a))
    (test-equal #f (config-trusted? a)))
  ;; Clean up.
  (config-test-teardown))
(test-end "path-config-context")


(define (touch file)
  (with-output-to-file file (lambda () (display ""))))

(test-begin "anyfile-config-context")
(test-group-with-cleanup "anyfile-config-context"
  (config-test-setup)

  (let ((file-a (string-append *testdirA* file-name-separator-string "a"))
        (file-b (string-append *testdirB* file-name-separator-string "b"))
        (file-c (string-append *testdir* file-name-separator-string "c")))
    (touch file-a)
    (touch file-b)

    (let ((a (anyfile-config-context file-a)))
      (test-assert (config? a))
      (let ((b (anyfile-config-context file-b #:parent a #:trusted #t)))
        (test-assert (config? b))
        (test-assert (not (config-trusted? a)))
        (test-assert (config-trusted? b))
        (test-assert (not (config-parent a)))
        (test-equal (config-parent b) a)))

    ;; Test for wrong arguments.
    (test-assert-thrown 'wrong-type-arg
                        (anyfile-config-context 'x))
    (test-assert-thrown 'wrong-type-arg
                        (anyfile-config-context file-c #:parent 'x)))
  ;; Clean up.
  (config-test-teardown))
(test-end "anyfile-config-context")


(test-begin "config-load")
(test-group-with-cleanup "config-load-grp"
  (config-test-setup)
  (let ((a (path-config-context *testdirA*)))
    (test-equal #f (config-loaded? a))
    (test-equal a (config-load! a))
    (test-assert (config-loaded? a))
    (chmod *testdirAconf* #o000) ;; Make conf unreadable

    ; the next test will fail under root account, skip it:
    ;
    ( if ( eq? (getuid) 0 )
      ( test-skip 1 )
    )

    (test-assert-thrown 'system-error (config-load! a #:force-load #t)))

  (test-assert-thrown 'system-error (config-load! (default-config-context) #:force-load #t))
  ;; Clean up.
  (config-test-teardown))
(test-end "config-load")


(test-begin "config-save")
(test-group-with-cleanup "config-save-grp"
  (config-test-setup)
  (let ((a (path-config-context *testdirA*)))
    (test-equal a (config-save! a)))

  (test-assert-thrown 'system-error (config-save! (default-config-context)))
  ;; FIXME test writing a file without permissions to write it.
  ;; Clean up.
  (config-test-teardown))
(test-end "config-save")


(test-begin "config-parent")
(test-group-with-cleanup "config-parent-grp"
  (config-test-setup)
  (let ((a (path-config-context *testdirA*))
        (b (path-config-context *testdir*)))
    (test-equal (user-config-context) (config-parent a))
    (test-equal (user-config-context) (config-parent b))

    (test-equal a (set-config-parent! a #f))
    (test-equal #f (config-parent a))

    (test-equal a (set-config-parent! a b))
    (test-equal b (config-parent a))

    ;; Check that configuration values are inherited from parent
    (test-assert-thrown 'config-error (config-boolean a "foo" "bar"))
    (set-config! b "foo" "bar" #t)
    (test-assert (config-boolean a "foo" "bar"))

    ;; Check that set-config-parent! refuses to form loops
    (test-equal b (set-config-parent! b a))
    (test-equal (user-config-context) (config-parent b))

    (test-equal a (set-config-parent! a (user-config-context)))
    (test-equal (user-config-context) (config-parent a)))
  ;; Clean up.
  (config-test-teardown))
(test-end "config-parent")


(test-begin "config-trust")
(test-group-with-cleanup "config-trust-grp"
  (config-test-setup)
  (let ((a (path-config-context *testdirA*)))
    (test-equal #f (config-trusted? a))
    (test-equal (user-config-context) (config-parent a))
    (test-assert (config-trusted? (user-config-context)))
    (test-equal (user-config-context) (config-trusted-context a))

    (test-equal a (set-config-trusted! a #t))
    (test-assert (config-trusted? a))
    (test-equal a (config-trusted-context a))

    (test-equal a (set-config-trusted! a #f))
    (test-equal #f (config-trusted? a)))

  (test-assert-thrown 'wrong-type-arg (config-trusted? 'x))
  (test-assert-thrown 'wrong-type-arg (set-config-trusted! 'x #f))
  ;; Clean up.
  (config-test-teardown))
(test-end "config-trust")


(test-begin "config-changed")
(test-group-with-cleanup "config-changed-grp"
  (config-test-setup)
  (let ((a (path-config-context *testdirA*)))
    (config-load! a #:force-load #t)
    (test-equal #f (config-changed? a))
    (set-config! a "foo" "bar" #t)
    (test-assert (config-changed? a))
    (config-save! a)
    (test-equal #f (config-changed? a))
    (set-config! a "foo" "bar" #f)
    (test-assert (config-changed? a))
    (config-load! a #:force-load #t)
    (test-equal #f (config-changed? a)))
  (test-assert-thrown 'wrong-type-arg (config-changed? 'x))
  ;; Clean up.
  (config-test-teardown))
(test-end "config-changed")


(test-begin "config-groups")
(test-group-with-cleanup "config-groups-grp"
  (config-test-setup)
  (let ((a (path-config-context *testdir*))
        (b (path-config-context *testdirA*)))
    (dynamic-wind
      (lambda () (set-config-parent! b a))
      (lambda ()

        (config-load! a #:force-load #t)
        (config-load! b #:force-load #t)

        (test-equal '() (config-groups a))
        (test-equal '() (config-groups b))
        (test-equal #f (config-has-group? a "foo"))
        (test-equal #f (config-has-group? b "foo"))

        (set-config! a "foo" "bar" #t)
        (test-equal '("foo") (config-groups a))
        (test-equal '("foo") (config-groups b))
        (test-assert (config-has-group? a "foo"))
        (test-assert (config-has-group? b "foo"))
        (test-equal #f (config-has-group? a "fizz"))

        (set-config! b "fizz" "bam" #t)
        (test-equal '("foo") (config-groups a))
        (test-assert (lset= string= '("fizz" "foo") (config-groups b)))
        (test-equal #f (config-has-group? a "fizz"))
        (test-assert (config-has-group? b "fizz"))

        (test-assert-thrown 'wrong-type-arg (config-has-group? 'x "foo"))
        (test-assert-thrown 'wrong-type-arg (config-has-group? a 'foo)))
      (lambda () (set-config-parent! b (user-config-context)))))
  ;; Clean up.
  (config-test-teardown))
(test-end "config-groups")


(test-begin "config-source")
(test-group-with-cleanup "config-source-grp"
  (config-test-setup)
  (let ((a (path-config-context *testdir*))
        (b (path-config-context *testdirA*)))
    (config-load! a)
    (config-load! b)

    (set-config! a "foo" "bar" #t)
    (test-equal a (config-source a "foo" "bar"))
    (test-assert-thrown 'config-error (config-source b "foo" "bar"))

    (dynamic-wind
      (lambda () (set-config-parent! b a))
      (lambda ()
        (test-equal a (config-source a "foo" "bar"))
        (test-equal a (config-source b "foo" "bar")) )
      (lambda () (set-config-parent! b (user-config-context)))))
  ;; Clean up.
  (config-test-teardown))
(test-end "config-source")


(test-begin "config-keys")
(test-group-with-cleanup "config-keys-grp"
  (config-test-setup)
  (let ((a (path-config-context *testdir*))
        (b (path-config-context *testdirA*)))
    (dynamic-wind
      (lambda () (set-config-parent! b a))
      (lambda ()
        (set-config-parent! b a)
        (config-load! a #:force-load #t)
        (config-load! b #:force-load #t)

        (test-assert-thrown 'config-error '() (config-keys a "foo"))

        (set-config! a "foo" "bar" #t)
        (test-equal '("bar") (config-keys a "foo"))
        (test-equal '("bar") (config-keys b "foo"))
        (test-assert (config-has-key? a "foo" "bar"))
        (test-assert (config-has-key? b "foo" "bar"))
        (test-equal #f (config-has-key? a "foo" "bam"))

        (set-config! b "foo" "bam" #t)
        (test-equal '("bar") (config-keys a "foo"))
        (test-equal (sort '("bam" "bar") string<?)
          (sort (config-keys b "foo") string<?))
        (test-assert (config-has-key? b "foo" "bam"))
        (test-equal #f (config-has-key? a "foo" "bam")))

      (lambda () (set-config-parent! b (user-config-context)))))
  ;; Clean up.
  (config-test-teardown))
(test-end "config-keys")


(test-begin "config-boolean")
(test-group-with-cleanup "config-boolean-grp"
  (config-test-setup)
  (let ((a (path-config-context *testdir*)))
    (config-load! a)
    (test-equal a (set-config! a "foo" "bar" #t))
    (test-equal #t (config-boolean a "foo" "bar"))
    (test-equal a (set-config! a "foo" "bar" #f))
    (test-equal #f (config-boolean a "foo" "bar"))
    (test-equal '(#f) (config-boolean-list a "foo" "bar"))

    (test-equal a (set-config! a "foo" "bar" '(#t #f)))
    (test-equal '(#t #f) (config-boolean-list a "foo" "bar")))
  ;; Clean up.
  (config-test-teardown))
(test-end "config-boolean")


(test-begin "config-int")
(test-group-with-cleanup "config-int-grp"
  (config-test-setup)
  (let ((a (path-config-context *testdir*)))
    (config-load! a)
    (test-equal a (set-config! a "foo" "bar" 42))
    (test-equal 42 (config-int a "foo" "bar"))
    (test-equal '(42) (config-int-list a "foo" "bar"))

    (test-equal a (set-config! a "foo" "bar" '(42 144)))
    (test-equal '(42 144) (config-int-list a "foo" "bar")))
  ;; Clean up.
  (config-test-teardown))
(test-end "config-int")


(test-begin "config-real")
(test-group-with-cleanup "config-real-grp"
  (config-test-setup)
  (let ((a (path-config-context *testdir*)))
    (config-load! a)
    (test-equal a (set-config! a "foo" "bar" 42.0))
    (test-equal 42.0 (config-real a "foo" "bar"))
    (test-equal '(42.0) (config-real-list a "foo" "bar"))

    (test-equal a (set-config! a "foo" "bar" '(42.0 144.0)))
    (test-equal '(42.0 144.0) (config-real-list a "foo" "bar")))
  ;; Clean up.
  (config-test-teardown))
(test-end "config-real")


(test-begin "config-string")
(test-group-with-cleanup "config-string-grp"
  (config-test-setup)
  (let ((a (path-config-context *testdir*)))
    (config-load! a)
    (test-equal a (set-config! a "foo" "bar" "wibble"))
    (test-equal "wibble" (config-string a "foo" "bar"))
    (test-equal '("wibble") (config-string-list a "foo" "bar"))

    (test-equal a (set-config! a "foo" "bar" '("wib;ble" "wobble")))
    (test-equal '("wib;ble" "wobble") (config-string-list a "foo" "bar")))
  ;; Clean up.
  (config-test-teardown))
(test-end "config-string")


(test-begin "config-get-set-errors")
(test-group-with-cleanup "config-get-set-errors-grp"
  (config-test-setup)
  (let ((a (path-config-context *testdir*)))
    (config-load! a)
    (test-assert-thrown 'wrong-type-arg (set-config! a "foo" "bar" 'BAD-VALUE))
    (test-assert-thrown 'wrong-type-arg (set-config! a "foo" "bar" '(BAD-VALUE)))
    (test-assert-thrown 'wrong-type-arg (set-config! a "foo" "bar" '(1 "foo")))

    (set-config! a "foo" "bar" "wibble")
    (test-assert-thrown 'config-error (config-boolean a "foo" "bar"))
    (test-assert-thrown 'config-error (config-int a "foo" "bar"))
    (test-assert-thrown 'config-error (config-real a "foo" "bar")))
  ;; Clean up.
  (config-test-teardown))
(test-end "config-get-set-errors")


(test-begin "config-events")
(test-group-with-cleanup "config-events-grp"
  (config-test-setup)
  (let* ((a (path-config-context *testdir*))
         (call-count 0)
         (handler  (lambda (cfg group key)
                     (set! call-count (1+ call-count)))))
    (test-equal a (add-config-event! a handler))
    (set-config! a "foo" "bar" #t)
    (test-equal 1 call-count)

    ;; Check that a handler can't be registered multiple times with
    ;; the same context.
    (test-equal a (add-config-event! a handler))
    (set-config! a "foo" "bar" #t)
    (test-equal 2 call-count)

    ;; Handler removal
    (test-equal a (remove-config-event! a handler))
    (set-config! a "foo" "bar" #t)
    (test-equal 2 call-count))
  ;; Clean up.
  (config-test-teardown))
(test-end "config-events")


; Unit test for config-remove-key! function:
;
(test-begin "config-remove-key")
(test-group-with-cleanup "config-remove-key-grp"
  (config-test-setup)
( let*
  (
  ( cfg   (path-config-context *testdir*) )
  ( group #f )
  ( key   #f )
  ( handler
    ( lambda( c g k ) ; config, group, key
      ( set! group g )
      ( set! key   k )
    )
  )
  )

  ; load configuration file from *testdir* directory:
  ;
  ( config-load! cfg )

  ; add group::key, set it to "value":
  ;
  ( set-config! cfg "group" "key" "value" )
  (test-assert-thrown 'config-error (config-remove-key! cfg "missing-group" "key"))
  (test-assert-thrown 'config-error (config-remove-key! cfg "group" "missing-key"))

  ; setup config event handler for cfg:
  ;
  ( add-config-event! cfg handler )

  ; remove group::key:
  ;
  ( test-assert (config-remove-key! cfg "group" "key") )

  ; check if event handler was called:
  ;
  ( test-equal group "group" )
  ( test-equal key   "key"   )

  ; check if group::key still exists:
  ;
  (test-assert (not (config-has-key? cfg "group" "key")))

  ; exception should be thrown if group::key is not found:
  ;
  ( test-assert-thrown 'config-error (config-remove-key! cfg "group" "key") )
  (test-assert-thrown 'wrong-type-arg (config-remove-key! 'cfg "group" "key"))
  (test-assert-thrown 'wrong-type-arg (config-remove-key! cfg 'group "key"))
  (test-assert-thrown 'wrong-type-arg (config-remove-key! cfg "group" 'key))

  ;; Test for wrong config file content.  A parse error must be
  ;; raised.
  (let ((config-file (config-filename cfg)))
    (when (file-exists? config-file)
      (delete-file config-file))
    (with-output-to-file config-file (lambda () (display "wrong config")))
    (test-assert-thrown 'config-error (config-remove-key! cfg "group" "key")))
  ) ; let
  ;; Clean up.
  (config-test-teardown)
) ; 'config-remove-key()
(test-end "config-remove-key")


; Unit test for config-remove-group! function:
;
(test-begin "config-remove-group")
(test-group-with-cleanup "config-remove-group-grp"
  (config-test-setup)
( let*
  (
  ( cfg   (path-config-context *testdir*) )
  ( group #f )
  ( key   #f )
  ( handler
    ( lambda( c g k ) ; config, group, key
      ( set! group g )
      ( set! key   k )
    )
  )
  )

  ; load configuration file from *testdir* directory:
  ;
  ( config-load! cfg )

  ; add group::key, set it to "value":
  ;
  ( set-config! cfg "group" "key" "value" )
  ;; Try removing a group not existing in the config file.
  (test-assert-thrown 'config-error (config-remove-group! cfg "missing-group"))

  ; setup config event handler for cfg:
  ;
  ( add-config-event! cfg handler )

  ; remove group::key:
  ;
  ( test-assert (config-remove-group! cfg "group" ) )

  ; check if event handler was called:
  ;
  ( test-equal group "group" )
  ( test-equal key   ""      )

  ; check if group still exists:
  ;
  (test-assert (not (config-has-group? cfg "group")))

  ; exception should be thrown if group is not found:
  ;
  ( test-assert-thrown 'config-error (config-remove-group! cfg "group") )
  (test-assert-thrown 'wrong-type-arg (config-remove-group! 'cfg "group"))
  (test-assert-thrown 'wrong-type-arg (config-remove-group! cfg 'group))

  ;; Test for wrong config file content.  A parse error must be
  ;; raised.
  (let ((config-file (config-filename cfg)))
    (when (file-exists? config-file)
      (delete-file config-file))
    (with-output-to-file config-file (lambda () (display "wrong config")))
    (test-assert-thrown 'config-error (config-remove-group! cfg group)))
) ; let
  ;; Clean up.
  (config-test-teardown)
) ; 'config-remove-group()
(test-end "config-remove-group")
