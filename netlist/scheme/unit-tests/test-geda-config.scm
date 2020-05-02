;;; Test Scheme procedures for working with configuration.

(use-modules (srfi srfi-1)
             ((geda config) #:renamer (symbol-prefix-proc 'geda:))
             (lepton os))

(define *testdir-geda*      (string-append (getcwd)   file-name-separator-string "t0402-tmp-geda"))
(define *testdir-geda-conf*  (string-append *testdir-geda*  file-name-separator-string "lepton.conf"))
(define *testdir-geda-A*     (string-append *testdir-geda*  file-name-separator-string "A"))
(define *testdir-geda-Aconf* (string-append *testdir-geda-A* file-name-separator-string "lepton.conf"))
(define *testdir-geda-B*     (string-append *testdir-geda*  file-name-separator-string "B"))
(define *testdir-geda-Bconf* (string-append *testdir-geda-B* file-name-separator-string "lepton.conf"))

;;; Setup/teardown of directories / files needed by tests
(define (config-geda-test-setup)
  (mkdir *testdir-geda*)
  (mkdir *testdir-geda-A*)
  (mkdir *testdir-geda-B*)
  (with-output-to-file *testdir-geda-conf* newline)
  (with-output-to-file *testdir-geda-Aconf* newline))

(define (config-geda-test-teardown)
  (system* "rm" "-rf" *testdir-geda*))


(test-begin "geda:default-config-context")

(let ((cfg (geda:default-config-context)))
  (test-assert (geda:config? cfg))
  (test-equal cfg (geda:default-config-context))
  (test-equal #f (geda:config-filename cfg))
  (test-equal #f (geda:config-parent cfg))
  (test-assert (geda:config-trusted? cfg)))

(test-end "geda:default-config-context")


(test-begin "geda:system-config-context")

(let ((cfg (geda:system-config-context)))
  (test-assert (geda:config? cfg))
  (test-equal cfg (geda:system-config-context))
  (test-equal (geda:default-config-context) (geda:config-parent cfg))
  (test-assert (geda:config-trusted? cfg)))

(test-end "geda:system-config-context")


(test-begin "geda:user-config-context")

(let ((cfg (geda:user-config-context)))
  (test-assert (geda:config? cfg))
  (test-equal cfg (geda:user-config-context))
  (test-equal (geda:system-config-context) (geda:config-parent cfg))
  (test-assert (geda:config-trusted? cfg)))

(test-end "geda:user-config-context")


(test-group-with-cleanup "geda:path-config-context"
  (config-geda-test-setup)
  ;; Unfortunately, there's no reliable way of testing the "recurse
  ;; all the way to root and then give up" functionality, because we
  ;; can't control the contents of the superdirectories of the CWD.
  (test-equal "/lepton.conf"
                (geda:config-filename (geda:path-config-context "/__missing/file/")))
  (let ((c (geda:path-config-context *testdir-geda*))
        (a (geda:path-config-context *testdir-geda-A*))
        (b (geda:path-config-context *testdir-geda-B*)))

    (test-assert (geda:config? a))
    (test-assert (geda:config? b))
    (test-assert (geda:config? c))

    (test-equal b c)

    (test-equal *testdir-geda-conf* (geda:config-filename c))
    (test-equal *testdir-geda-Aconf* (geda:config-filename a))

    (test-equal (geda:user-config-context) (geda:config-parent a))
    (test-equal #f (geda:config-trusted? a)))
  ;; Clean up.
  (config-geda-test-teardown))


(test-group-with-cleanup "geda:config-load"
  (config-geda-test-setup)
  (let ((a (geda:path-config-context *testdir-geda-A*)))
    (test-equal #f (geda:config-loaded? a))
    (test-equal a (geda:config-load! a))
    (test-assert (geda:config-loaded? a))
    (chmod *testdir-geda-Aconf* #o000) ;; Make conf unreadable
    (test-assert-thrown 'system-error (geda:config-load! a #:force-load #t)))

  (test-assert-thrown 'system-error (geda:config-load! (geda:default-config-context) #:force-load #t))
  ;; Clean up.
  (config-geda-test-teardown))


(test-group-with-cleanup "geda:config-save"
  (config-geda-test-setup)
  (let ((a (geda:path-config-context *testdir-geda-A*)))
    (test-equal a (geda:config-save! a)))

  (test-assert-thrown 'system-error (geda:config-save! (geda:default-config-context)))
  ;; FIXME test writing a file without permissions to write it.
  ;; Clean up.
  (config-geda-test-teardown))


(test-group-with-cleanup "geda:config-parent"
  (config-geda-test-setup)
  (let ((a (geda:path-config-context *testdir-geda-A*))
        (b (geda:path-config-context *testdir-geda*)))
    (test-equal (geda:user-config-context) (geda:config-parent a))
    (test-equal (geda:user-config-context) (geda:config-parent b))

    (test-equal a (geda:set-config-parent! a #f))
    (test-equal #f (geda:config-parent a))

    (test-equal a (geda:set-config-parent! a b))
    (test-equal b (geda:config-parent a))

    ;; Check that configuration values are inherited from parent
    (test-assert-thrown 'config-error (geda:config-boolean a "foo" "bar"))
    (geda:set-config! b "foo" "bar" #t)
    (test-assert (geda:config-boolean a "foo" "bar"))

    ;; Check that geda:set-config-parent! refuses to form loops
    (test-equal b (geda:set-config-parent! b a))
    (test-equal (geda:user-config-context) (geda:config-parent b))

    (test-equal a (geda:set-config-parent! a (geda:user-config-context)))
    (test-equal (geda:user-config-context) (geda:config-parent a))
    )
  ;; Clean up.
  (config-geda-test-teardown))


(test-group-with-cleanup "geda:config-trust"
  (config-geda-test-setup)
  (let ((a (geda:path-config-context *testdir-geda-A*)))
    (test-equal #f (geda:config-trusted? a))
    (test-equal (geda:user-config-context) (geda:config-parent a))
    (test-assert (geda:config-trusted? (geda:user-config-context)))
    (test-equal (geda:user-config-context) (geda:config-trusted-context a))

    (test-equal a (geda:set-config-trusted! a #t))
    (test-assert (geda:config-trusted? a))
    (test-equal a (geda:config-trusted-context a))

    (test-equal a (geda:set-config-trusted! a #f))
    (test-equal #f (geda:config-trusted? a)))
    ;; Clean up.
  (config-geda-test-teardown))


(test-group-with-cleanup "geda:config-changed"
  (config-geda-test-setup)
  (let ((a (geda:path-config-context *testdir-geda-A*)))
    (geda:config-load! a #:force-load #t)
    (test-equal #f (geda:config-changed? a))
    (geda:set-config! a "foo" "bar" #t)
    (test-assert (geda:config-changed? a))
    (geda:config-save! a)
    (test-equal #f (geda:config-changed? a))
    (geda:set-config! a "foo" "bar" #f)
    (test-assert (geda:config-changed? a))
    (geda:config-load! a #:force-load #t)
    (test-equal #f (geda:config-changed? a)))
  ;; Clean up.
  (config-geda-test-teardown))


(test-group-with-cleanup "geda:config-groups"
  (config-geda-test-setup)
  (let ((a (geda:path-config-context *testdir-geda*))
        (b (geda:path-config-context *testdir-geda-A*)))
    (dynamic-wind
       (lambda () (geda:set-config-parent! b a))
       (lambda ()

         (geda:config-load! a #:force-load #t)
         (geda:config-load! b #:force-load #t)

         (test-equal '() (geda:config-groups a))
         (test-equal '() (geda:config-groups b))
         (test-equal #f (geda:config-has-group? a "foo"))
         (test-equal #f (geda:config-has-group? b "foo"))

         (geda:set-config! a "foo" "bar" #t)
         (test-equal '("foo") (geda:config-groups a))
         (test-equal '("foo") (geda:config-groups b))
         (test-assert (geda:config-has-group? a "foo"))
         (test-assert (geda:config-has-group? b "foo"))
         (test-equal #f (geda:config-has-group? a "fizz"))

         (geda:set-config! b "fizz" "bam" #t)
         (test-equal '("foo") (geda:config-groups a))
         (test-assert (lset= string= '("fizz" "foo") (geda:config-groups b)))
         (test-equal #f (geda:config-has-group? a "fizz"))
         (test-assert (geda:config-has-group? b "fizz")) )
       (lambda () (geda:set-config-parent! b (geda:user-config-context)))))
  ;; Clean up.
  (config-geda-test-teardown))


(test-group-with-cleanup "geda:config-source"
  (config-geda-test-setup)
  (let ((a (geda:path-config-context *testdir-geda*))
        (b (geda:path-config-context *testdir-geda-A*)))
    (geda:config-load! a)
    (geda:config-load! b)

    (geda:set-config! a "foo" "bar" #t)
    (test-equal a (geda:config-source a "foo" "bar"))
    (test-assert-thrown 'config-error (geda:config-source b "foo" "bar"))

    (dynamic-wind
        (lambda () (geda:set-config-parent! b a))
        (lambda ()
          (test-equal a (geda:config-source a "foo" "bar"))
          (test-equal a (geda:config-source b "foo" "bar")) )
        (lambda () (geda:set-config-parent! b (geda:user-config-context)))))
  ;; Clean up.
  (config-geda-test-teardown))


(test-group-with-cleanup "geda:config-keys"
  (config-geda-test-setup)
  (let ((a (geda:path-config-context *testdir-geda*))
        (b (geda:path-config-context *testdir-geda-A*)))
    (dynamic-wind
       (lambda () (geda:set-config-parent! b a))
       (lambda ()
         (geda:set-config-parent! b a)
         (geda:config-load! a #:force-load #t)
         (geda:config-load! b #:force-load #t)

         (test-assert-thrown 'config-error '() (geda:config-keys a "foo"))

         (geda:set-config! a "foo" "bar" #t)
         (test-equal '("bar") (geda:config-keys a "foo"))
         (test-equal '("bar") (geda:config-keys b "foo"))
         (test-assert (geda:config-has-key? a "foo" "bar"))
         (test-assert (geda:config-has-key? b "foo" "bar"))
         (test-equal #f (geda:config-has-key? a "foo" "bam"))

         (geda:set-config! b "foo" "bam" #t)
         (test-equal '("bar") (geda:config-keys a "foo"))
         (test-equal (sort '("bam" "bar") string<?)
                       (sort (geda:config-keys b "foo") string<?))
         (test-assert (geda:config-has-key? b "foo" "bam"))
         (test-equal #f (geda:config-has-key? a "foo" "bam")))

       (lambda () (geda:set-config-parent! b (geda:user-config-context)))))
  ;; Clean up.
  (config-geda-test-teardown))


(test-group-with-cleanup "geda:config-boolean"
  (config-geda-test-setup)
  (let ((a (geda:path-config-context *testdir-geda*)))
    (geda:config-load! a)
    (test-equal a (geda:set-config! a "foo" "bar" #t))
    (test-equal #t (geda:config-boolean a "foo" "bar"))
    (test-equal a (geda:set-config! a "foo" "bar" #f))
    (test-equal #f (geda:config-boolean a "foo" "bar"))
    (test-equal '(#f) (geda:config-boolean-list a "foo" "bar"))

    (test-equal a (geda:set-config! a "foo" "bar" '(#t #f)))
    (test-equal '(#t #f) (geda:config-boolean-list a "foo" "bar")))
  ;; Clean up.
  (config-geda-test-teardown))


(test-group-with-cleanup "geda:config-int"
  (config-geda-test-setup)
  (let ((a (geda:path-config-context *testdir-geda*)))
    (geda:config-load! a)
    (test-equal a (geda:set-config! a "foo" "bar" 42))
    (test-equal 42 (geda:config-int a "foo" "bar"))
    (test-equal '(42) (geda:config-int-list a "foo" "bar"))

    (test-equal a (geda:set-config! a "foo" "bar" '(42 144)))
    (test-equal '(42 144) (geda:config-int-list a "foo" "bar")))
  ;; Clean up.
  (config-geda-test-teardown))


(test-group-with-cleanup "geda:config-real"
  (config-geda-test-setup)
  (let ((a (geda:path-config-context *testdir-geda*)))
    (geda:config-load! a)
    (test-equal a (geda:set-config! a "foo" "bar" 42.0))
    (test-equal 42.0 (geda:config-real a "foo" "bar"))
    (test-equal '(42.0) (geda:config-real-list a "foo" "bar"))

    (test-equal a (geda:set-config! a "foo" "bar" '(42.0 144.0)))
    (test-equal '(42.0 144.0) (geda:config-real-list a "foo" "bar")))
  ;; Clean up.
  (config-geda-test-teardown))


(test-group-with-cleanup "geda:config-string"
  (config-geda-test-setup)
  (let ((a (geda:path-config-context *testdir-geda*)))
    (geda:config-load! a)
    (test-equal a (geda:set-config! a "foo" "bar" "wibble"))
    (test-equal "wibble" (geda:config-string a "foo" "bar"))
    (test-equal '("wibble") (geda:config-string-list a "foo" "bar"))

    (test-equal a (geda:set-config! a "foo" "bar" '("wib;ble" "wobble")))
    (test-equal '("wib;ble" "wobble") (geda:config-string-list a "foo" "bar")))
  ;; Clean up.
  (config-geda-test-teardown))


(test-group-with-cleanup "geda:config-get-set-errors"
  (config-geda-test-setup)
  (let ((a (geda:path-config-context *testdir-geda*)))
    (geda:config-load! a)
    (test-assert-thrown 'wrong-type-arg (geda:set-config! a "foo" "bar" 'BAD-VALUE))
    (test-assert-thrown 'wrong-type-arg (geda:set-config! a "foo" "bar" '(BAD-VALUE)))
    (test-assert-thrown 'wrong-type-arg (geda:set-config! a "foo" "bar" '(1 "foo")))

    (geda:set-config! a "foo" "bar" "wibble")
    (test-assert-thrown 'config-error (geda:config-boolean a "foo" "bar"))
    (test-assert-thrown 'config-error (geda:config-int a "foo" "bar"))
    (test-assert-thrown 'config-error (geda:config-real a "foo" "bar")))
  ;; Clean up.
  (config-geda-test-teardown))


(test-group-with-cleanup "geda:config-events"
  (config-geda-test-setup)
  (let* ((a (geda:path-config-context *testdir-geda*))
         (call-count 0)
         (handler  (lambda (cfg group key)
                     (set! call-count (1+ call-count)))))
    (test-equal a (geda:add-config-event! a handler))
    (geda:set-config! a "foo" "bar" #t)
    (test-equal 1 call-count)

    ;; Check that a handler can't be registered multiple times with
    ;; the same context.
    (test-equal a (geda:add-config-event! a handler))
    (geda:set-config! a "foo" "bar" #t)
    (test-equal 2 call-count)

    ;; Handler removal
    (test-equal a (geda:remove-config-event! a handler))
    (geda:set-config! a "foo" "bar" #t)
    (test-equal 2 call-count))
  ;; Clean up.
  (config-geda-test-teardown))



; Unit test for geda:config-remove-key! function:
;
(test-group-with-cleanup "geda:config-remove-key"
  (config-geda-test-setup)
( let*
  (
  ( cfg   (geda:path-config-context *testdir-geda*) )
  ( group #f )
  ( key   #f )
  ( handler
    ( lambda( c g k ) ; config, group, key
      ( set! group g )
      ( set! key   k )
    )
  )
  )

  ; load configuration file from *testdir-geda* directory:
  ;
  ( geda:config-load! cfg )

  ; add group::key, set it to "value":
  ;
  ( geda:set-config! cfg "group" "key" "value" )

  ; setup config event handler for cfg:
  ;
  ( geda:add-config-event! cfg handler )

  ; remove group::key:
  ;
  ( test-assert (geda:config-remove-key! cfg "group" "key") )

  ; check if event handler was called:
  ;
  ( test-equal group "group" )
  ( test-equal key   "key"   )

  ; check if group::key still exists:
  ;
  (test-assert (not (geda:config-has-key? cfg "group" "key")))

  ; exception should be thrown if group::key is not found:
  ;
  ( test-assert-thrown 'config-error (geda:config-remove-key! cfg "group" "key") )

) ; let
  ;; Clean up.
  (config-geda-test-teardown)) ; 'config-remove-key()



; Unit test for geda:config-remove-group! function:
;
(test-group-with-cleanup "geda:config-remove-group"
  (config-geda-test-setup)
( let*
  (
  ( cfg   (geda:path-config-context *testdir-geda*) )
  ( group #f )
  ( key   #f )
  ( handler
    ( lambda( c g k ) ; config, group, key
      ( set! group g )
      ( set! key   k )
    )
  )
  )

  ; load configuration file from *testdir-geda* directory:
  ;
  ( geda:config-load! cfg )

  ; add group::key, set it to "value":
  ;
  ( geda:set-config! cfg "group" "key" "value" )

  ; setup config event handler for cfg:
  ;
  ( geda:add-config-event! cfg handler )

  ; remove group::key:
  ;
  ( test-assert (geda:config-remove-group! cfg "group" ) )

  ; check if event handler was called:
  ;
  ( test-equal group "group" )
  ( test-equal key   ""      )

  ; check if group still exists:
  ;
  (test-assert (not (geda:config-has-group? cfg "group")))

  ; exception should be thrown if group is not found:
  ;
  ( test-assert-thrown 'config-error (geda:config-remove-group! cfg "group") )

) ; let
  ;; Clean up.
  (config-geda-test-teardown)) ; 'config-remove-group()
