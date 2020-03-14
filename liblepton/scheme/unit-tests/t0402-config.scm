;; Test Scheme procedures for working with configuration.

(use-modules (unit-test)
             (srfi srfi-1)
             (lepton config)
             ((geda config) #:renamer (symbol-prefix-proc 'geda:))
             (lepton os))


(define *testdir*      (string-append (getcwd)   file-name-separator-string "t0402-tmp"))
(define *testdirconf*  (string-append *testdir*  file-name-separator-string "lepton.conf"))
(define *testdirA*     (string-append *testdir*  file-name-separator-string "A"))
(define *testdirAconf* (string-append *testdirA* file-name-separator-string "lepton.conf"))
(define *testdirB*     (string-append *testdir*  file-name-separator-string "B"))
(define *testdirBconf* (string-append *testdirB* file-name-separator-string "lepton.conf"))

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
  (assert-equal "/lepton.conf"
                (config-filename (path-config-context "/__missing/file/")))
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
    (assert-thrown 'system-error (config-load! a #:force-load #t)))

  (assert-thrown 'system-error (config-load! (default-config-context) #:force-load #t)))

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
    (config-load! a #:force-load #t)
    (assert-equal #f (config-changed? a))
    (set-config! a "foo" "bar" #t)
    (assert-true (config-changed? a))
    (config-save! a)
    (assert-equal #f (config-changed? a))
    (set-config! a "foo" "bar" #f)
    (assert-true (config-changed? a))
    (config-load! a #:force-load #t)
    (assert-equal #f (config-changed? a))))

(begin-config-test 'config-groups
  (let ((a (path-config-context *testdir*))
        (b (path-config-context *testdirA*)))
    (dynamic-wind
       (lambda () (set-config-parent! b a))
       (lambda ()

         (config-load! a #:force-load #t)
         (config-load! b #:force-load #t)

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
         (assert-true (lset= string= '("fizz" "foo") (config-groups b)))
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
         (config-load! a #:force-load #t)
         (config-load! b #:force-load #t)

         (assert-thrown 'config-error '() (config-keys a "foo"))

         (set-config! a "foo" "bar" #t)
         (assert-equal '("bar") (config-keys a "foo"))
         (assert-equal '("bar") (config-keys b "foo"))
         (assert-true (config-has-key? a "foo" "bar"))
         (assert-true (config-has-key? b "foo" "bar"))
         (assert-equal #f (config-has-key? a "foo" "bam"))

         (set-config! b "foo" "bam" #t)
         (assert-equal '("bar") (config-keys a "foo"))
         (assert-equal (sort '("bam" "bar") string<?)
                       (sort (config-keys b "foo") string<?))
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



; Unit test for config-remove-key! function:
;
( begin-config-test 'config-remove-key
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

  ; setup config event handler for cfg:
  ;
  ( add-config-event! cfg handler )

  ; remove group::key:
  ;
  ( assert-true (config-remove-key! cfg "group" "key") )

  ; check if event handler was called:
  ;
  ( assert-equal group "group" )
  ( assert-equal key   "key"   )

  ; check if group::key still exists:
  ;
  ( assert-false (config-has-key? cfg "group" "key") )

  ; exception should be thrown if group::key is not found:
  ;
  ( assert-thrown 'config-error (config-remove-key! cfg "group" "key") )

) ; let
) ; 'config-remove-key()



; Unit test for config-remove-group! function:
;
( begin-config-test 'config-remove-group
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

  ; setup config event handler for cfg:
  ;
  ( add-config-event! cfg handler )

  ; remove group::key:
  ;
  ( assert-true (config-remove-group! cfg "group" ) )

  ; check if event handler was called:
  ;
  ( assert-equal group "group" )
  ( assert-equal key   ""      )

  ; check if group still exists:
  ;
  ( assert-false (config-has-group? cfg "group" ) )

  ; exception should be thrown if group is not found:
  ;
  ( assert-thrown 'config-error (config-remove-group! cfg "group") )

) ; let
) ; 'config-remove-group()

;;; The same tests for the deprecated (geda config) module
;;; functions.

(define *testdir-geda*      (string-append (getcwd)   file-name-separator-string "t0402-tmp-geda"))
(define *testdir-geda-conf*  (string-append *testdir-geda*  file-name-separator-string "lepton.conf"))
(define *testdir-geda-A*     (string-append *testdir-geda*  file-name-separator-string "A"))
(define *testdir-geda-Aconf* (string-append *testdir-geda-A* file-name-separator-string "lepton.conf"))
(define *testdir-geda-B*     (string-append *testdir-geda*  file-name-separator-string "B"))
(define *testdir-geda-Bconf* (string-append *testdir-geda-B* file-name-separator-string "lepton.conf"))

;; Setup/teardown of directories / files needed by tests
(define (config-geda-test-setup)
  (config-geda-test-teardown)
  (mkdir *testdir-geda*)
  (mkdir *testdir-geda-A*)
  (mkdir *testdir-geda-B*)
  (with-output-to-file *testdir-geda-conf* newline)
  (with-output-to-file *testdir-geda-Aconf* newline))
(define (config-geda-test-teardown)
  (system* "rm" "-rf" *testdir-geda*))

(define-syntax begin-geda-config-test
  (syntax-rules ()
    ((_ name . test-forms)
     (begin-test name
       (dynamic-wind
         config-geda-test-setup
         (lambda () . test-forms)
         config-geda-test-teardown)))))

(begin-test 'geda:default-config-context
  (let ((cfg (geda:default-config-context)))
    (assert-true (geda:config? cfg))
    (assert-equal cfg (geda:default-config-context))
    (assert-equal #f (geda:config-filename cfg))
    (assert-equal #f (geda:config-parent cfg))
    (assert-true (geda:config-trusted? cfg))))

(begin-test 'geda:system-config-context
  (let ((cfg (geda:system-config-context)))
    (assert-true (geda:config? cfg))
    (assert-equal cfg (geda:system-config-context))
    (assert-equal (geda:default-config-context) (geda:config-parent cfg))
    (assert-true (geda:config-trusted? cfg))))

(begin-test 'geda:user-config-context
  (let ((cfg (geda:user-config-context)))
    (assert-true (geda:config? cfg))
    (assert-equal cfg (geda:user-config-context))
    (assert-equal (geda:system-config-context) (geda:config-parent cfg))
    (assert-true (geda:config-trusted? cfg))))

(begin-geda-config-test 'geda:path-config-context
  ;; Unfortunately, there's no reliable way of testing the "recurse
  ;; all the way to root and then give up" functionality, because we
  ;; can't control the contents of the superdirectories of the CWD.
  (assert-equal "/lepton.conf"
                (geda:config-filename (geda:path-config-context "/__missing/file/")))
  (let ((c (geda:path-config-context *testdir-geda*))
        (a (geda:path-config-context *testdir-geda-A*))
        (b (geda:path-config-context *testdir-geda-B*)))

    (assert-true (geda:config? a))
    (assert-true (geda:config? b))
    (assert-true (geda:config? c))

    (assert-equal b c)

    (assert-equal *testdir-geda-conf* (geda:config-filename c))
    (assert-equal *testdir-geda-Aconf* (geda:config-filename a))

    (assert-equal (geda:user-config-context) (geda:config-parent a))
    (assert-equal #f (geda:config-trusted? a))))

(begin-geda-config-test 'geda:config-load
  (let ((a (geda:path-config-context *testdir-geda-A*)))
    (assert-equal #f (geda:config-loaded? a))
    (assert-equal a (geda:config-load! a))
    (assert-true (geda:config-loaded? a))
    (chmod *testdir-geda-Aconf* #o000) ;; Make conf unreadable
    (assert-thrown 'system-error (geda:config-load! a #:force-load #t)))

  (assert-thrown 'system-error (geda:config-load! (geda:default-config-context) #:force-load #t)))

(begin-geda-config-test 'geda:config-save
  (let ((a (geda:path-config-context *testdir-geda-A*)))
    (assert-equal a (geda:config-save! a)))

  (assert-thrown 'system-error (geda:config-save! (geda:default-config-context)))
  ;; FIXME test writing a file without permissions to write it.
  )

(begin-geda-config-test 'geda:config-parent
  (let ((a (geda:path-config-context *testdir-geda-A*))
        (b (geda:path-config-context *testdir-geda*)))
    (assert-equal (geda:user-config-context) (geda:config-parent a))
    (assert-equal (geda:user-config-context) (geda:config-parent b))

    (assert-equal a (geda:set-config-parent! a #f))
    (assert-equal #f (geda:config-parent a))

    (assert-equal a (geda:set-config-parent! a b))
    (assert-equal b (geda:config-parent a))

    ;; Check that configuration values are inherited from parent
    (assert-thrown 'config-error (geda:config-boolean a "foo" "bar"))
    (geda:set-config! b "foo" "bar" #t)
    (assert-true (geda:config-boolean a "foo" "bar"))

    ;; Check that geda:set-config-parent! refuses to form loops
    (assert-equal b (geda:set-config-parent! b a))
    (assert-equal (geda:user-config-context) (geda:config-parent b))

    (assert-equal a (geda:set-config-parent! a (geda:user-config-context)))
    (assert-equal (geda:user-config-context) (geda:config-parent a))
    ))

(begin-geda-config-test 'geda:config-trust
  (let ((a (geda:path-config-context *testdir-geda-A*)))
    (assert-equal #f (geda:config-trusted? a))
    (assert-equal (geda:user-config-context) (geda:config-parent a))
    (assert-true (geda:config-trusted? (geda:user-config-context)))
    (assert-equal (geda:user-config-context) (geda:config-trusted-context a))

    (assert-equal a (geda:set-config-trusted! a #t))
    (assert-true (geda:config-trusted? a))
    (assert-equal a (geda:config-trusted-context a))

    (assert-equal a (geda:set-config-trusted! a #f))
    (assert-equal #f (geda:config-trusted? a))))

(begin-geda-config-test 'geda:config-changed
  (let ((a (geda:path-config-context *testdir-geda-A*)))
    (geda:config-load! a #:force-load #t)
    (assert-equal #f (geda:config-changed? a))
    (geda:set-config! a "foo" "bar" #t)
    (assert-true (geda:config-changed? a))
    (geda:config-save! a)
    (assert-equal #f (geda:config-changed? a))
    (geda:set-config! a "foo" "bar" #f)
    (assert-true (geda:config-changed? a))
    (geda:config-load! a #:force-load #t)
    (assert-equal #f (geda:config-changed? a))))

(begin-geda-config-test 'geda:config-groups
  (let ((a (geda:path-config-context *testdir-geda*))
        (b (geda:path-config-context *testdir-geda-A*)))
    (dynamic-wind
       (lambda () (geda:set-config-parent! b a))
       (lambda ()

         (geda:config-load! a #:force-load #t)
         (geda:config-load! b #:force-load #t)

         (assert-equal '() (geda:config-groups a))
         (assert-equal '() (geda:config-groups b))
         (assert-equal #f (geda:config-has-group? a "foo"))
         (assert-equal #f (geda:config-has-group? b "foo"))

         (geda:set-config! a "foo" "bar" #t)
         (assert-equal '("foo") (geda:config-groups a))
         (assert-equal '("foo") (geda:config-groups b))
         (assert-true (geda:config-has-group? a "foo"))
         (assert-true (geda:config-has-group? b "foo"))
         (assert-equal #f (geda:config-has-group? a "fizz"))

         (geda:set-config! b "fizz" "bam" #t)
         (assert-equal '("foo") (geda:config-groups a))
         (assert-true (lset= string= '("fizz" "foo") (geda:config-groups b)))
         (assert-equal #f (geda:config-has-group? a "fizz"))
         (assert-true (geda:config-has-group? b "fizz")) )
       (lambda () (geda:set-config-parent! b (geda:user-config-context))))))

(begin-geda-config-test 'geda:config-source
  (let ((a (geda:path-config-context *testdir-geda*))
        (b (geda:path-config-context *testdir-geda-A*)))
    (geda:config-load! a)
    (geda:config-load! b)

    (geda:set-config! a "foo" "bar" #t)
    (assert-equal a (geda:config-source a "foo" "bar"))
    (assert-thrown 'config-error (geda:config-source b "foo" "bar"))

    (dynamic-wind
        (lambda () (geda:set-config-parent! b a))
        (lambda ()
          (assert-equal a (geda:config-source a "foo" "bar"))
          (assert-equal a (geda:config-source b "foo" "bar")) )
        (lambda () (geda:set-config-parent! b (geda:user-config-context))))
    ))

(begin-geda-config-test 'geda:config-keys
  (let ((a (geda:path-config-context *testdir-geda*))
        (b (geda:path-config-context *testdir-geda-A*)))
    (dynamic-wind
       (lambda () (geda:set-config-parent! b a))
       (lambda ()
         (geda:set-config-parent! b a)
         (geda:config-load! a #:force-load #t)
         (geda:config-load! b #:force-load #t)

         (assert-thrown 'config-error '() (geda:config-keys a "foo"))

         (geda:set-config! a "foo" "bar" #t)
         (assert-equal '("bar") (geda:config-keys a "foo"))
         (assert-equal '("bar") (geda:config-keys b "foo"))
         (assert-true (geda:config-has-key? a "foo" "bar"))
         (assert-true (geda:config-has-key? b "foo" "bar"))
         (assert-equal #f (geda:config-has-key? a "foo" "bam"))

         (geda:set-config! b "foo" "bam" #t)
         (assert-equal '("bar") (geda:config-keys a "foo"))
         (assert-equal (sort '("bam" "bar") string<?)
                       (sort (geda:config-keys b "foo") string<?))
         (assert-true (geda:config-has-key? b "foo" "bam"))
         (assert-equal #f (geda:config-has-key? a "foo" "bam")))

       (lambda () (geda:set-config-parent! b (geda:user-config-context))))))

(begin-geda-config-test 'geda:config-boolean
  (let ((a (geda:path-config-context *testdir-geda*)))
    (geda:config-load! a)
    (assert-equal a (geda:set-config! a "foo" "bar" #t))
    (assert-equal #t (geda:config-boolean a "foo" "bar"))
    (assert-equal a (geda:set-config! a "foo" "bar" #f))
    (assert-equal #f (geda:config-boolean a "foo" "bar"))
    (assert-equal '(#f) (geda:config-boolean-list a "foo" "bar"))

    (assert-equal a (geda:set-config! a "foo" "bar" '(#t #f)))
    (assert-equal '(#t #f) (geda:config-boolean-list a "foo" "bar"))))

(begin-geda-config-test 'geda:config-int
  (let ((a (geda:path-config-context *testdir-geda*)))
    (geda:config-load! a)
    (assert-equal a (geda:set-config! a "foo" "bar" 42))
    (assert-equal 42 (geda:config-int a "foo" "bar"))
    (assert-equal '(42) (geda:config-int-list a "foo" "bar"))

    (assert-equal a (geda:set-config! a "foo" "bar" '(42 144)))
    (assert-equal '(42 144) (geda:config-int-list a "foo" "bar"))))

(begin-geda-config-test 'geda:config-real
  (let ((a (geda:path-config-context *testdir-geda*)))
    (geda:config-load! a)
    (assert-equal a (geda:set-config! a "foo" "bar" 42.0))
    (assert-equal 42.0 (geda:config-real a "foo" "bar"))
    (assert-equal '(42.0) (geda:config-real-list a "foo" "bar"))

    (assert-equal a (geda:set-config! a "foo" "bar" '(42.0 144.0)))
    (assert-equal '(42.0 144.0) (geda:config-real-list a "foo" "bar"))))

(begin-geda-config-test 'geda:config-string
  (let ((a (geda:path-config-context *testdir-geda*)))
    (geda:config-load! a)
    (assert-equal a (geda:set-config! a "foo" "bar" "wibble"))
    (assert-equal "wibble" (geda:config-string a "foo" "bar"))
    (assert-equal '("wibble") (geda:config-string-list a "foo" "bar"))

    (assert-equal a (geda:set-config! a "foo" "bar" '("wib;ble" "wobble")))
    (assert-equal '("wib;ble" "wobble") (geda:config-string-list a "foo" "bar"))))

(begin-geda-config-test 'geda:config-get-set-errors
  (let ((a (geda:path-config-context *testdir-geda*)))
    (geda:config-load! a)
    (assert-thrown 'wrong-type-arg (geda:set-config! a "foo" "bar" 'BAD-VALUE))
    (assert-thrown 'wrong-type-arg (geda:set-config! a "foo" "bar" '(BAD-VALUE)))
    (assert-thrown 'wrong-type-arg (geda:set-config! a "foo" "bar" '(1 "foo")))

    (geda:set-config! a "foo" "bar" "wibble")
    (assert-thrown 'config-error (geda:config-boolean a "foo" "bar"))
    (assert-thrown 'config-error (geda:config-int a "foo" "bar"))
    (assert-thrown 'config-error (geda:config-real a "foo" "bar"))))

(begin-geda-config-test 'geda:config-events
  (let* ((a (geda:path-config-context *testdir-geda*))
         (call-count 0)
         (handler  (lambda (cfg group key)
                     (set! call-count (1+ call-count)))))
    (assert-equal a (geda:add-config-event! a handler))
    (geda:set-config! a "foo" "bar" #t)
    (assert-equal 1 call-count)

    ;; Check that a handler can't be registered multiple times with
    ;; the same context.
    (assert-equal a (geda:add-config-event! a handler))
    (geda:set-config! a "foo" "bar" #t)
    (assert-equal 2 call-count)

    ;; Handler removal
    (assert-equal a (geda:remove-config-event! a handler))
    (geda:set-config! a "foo" "bar" #t)
    (assert-equal 2 call-count)))



; Unit test for geda:config-remove-key! function:
;
( begin-geda-config-test 'geda:config-remove-key
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
  ( assert-true (geda:config-remove-key! cfg "group" "key") )

  ; check if event handler was called:
  ;
  ( assert-equal group "group" )
  ( assert-equal key   "key"   )

  ; check if group::key still exists:
  ;
  ( assert-false (geda:config-has-key? cfg "group" "key") )

  ; exception should be thrown if group::key is not found:
  ;
  ( assert-thrown 'config-error (geda:config-remove-key! cfg "group" "key") )

) ; let
) ; 'config-remove-key()



; Unit test for geda:config-remove-group! function:
;
( begin-geda-config-test 'geda:config-remove-group
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
  ( assert-true (geda:config-remove-group! cfg "group" ) )

  ; check if event handler was called:
  ;
  ( assert-equal group "group" )
  ( assert-equal key   ""      )

  ; check if group still exists:
  ;
  ( assert-false (geda:config-has-group? cfg "group" ) )

  ; exception should be thrown if group is not found:
  ;
  ( assert-thrown 'config-error (geda:config-remove-group! cfg "group") )

) ; let
) ; 'config-remove-group()
