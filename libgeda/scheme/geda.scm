; -*-Scheme-*-
(use-modules (geda os) (ice-9 optargs) (ice-9 ftw))

(define path-sep file-name-separator-string)

;; Clean up logfiles
;; FIXME this should be a plugin
(use-modules (geda log-rotate))

;; Legacy gEDA data & configuration directories.  These functions will
;; usually return #f if gEDA was compiled with --disable-deprecated.
;; Use the sys-data-dirs and sys-config-dirs functions from the (geda
;; os) module instead.
(define geda-data-path (or (getenv "GEDADATA")
                           ((@ (srfi srfi-1) last) (sys-data-dirs))))
(define geda-rc-path (or (getenv "GEDADATARC") (getenv "GEDADATA")
                         ((@ (srfi srfi-1) last) (sys-config-dirs))))

(define (build-path first . rest)
  (if (null? rest) first
      (apply build-path 
	     (append (list (string-append first path-sep (car rest))) 
		     (cdr rest)))))

;; Returns #t if the given path is a regular file, otherwise #f.
(define regular-file?
  (lambda (path)
    (eqv? (stat:type (stat path)) 'regular )
  ))

;; Returns #t if the given path is a directory file, otherwise #f.
(define directory?
  (lambda (path)
    (eqv? (stat:type (stat path)) 'directory )
  ))

;; Returns #t if the given string ends with the given suffix, otherwise or #f.
(define has-suffix?
  (lambda (str suf)
    (define len-str (string-length str))
    (define len-suf (string-length suf))
    (if (>= len-str len-suf)
      (string=? (substring str (- len-str len-suf) len-str) suf)
      #f
    )))

;; Execute any scheme files found in the given directory.
(define load-scheme-dir
  (lambda (scheme-dir)
  (if (and (file-exists? scheme-dir)
           (directory? scheme-dir)
           (access? scheme-dir R_OK))
    (let ((dir (opendir scheme-dir)))
      (do ((entry (readdir dir) (readdir dir)))
          ((eof-object? entry))
        (let ((path (build-path scheme-dir entry)))
          (if (and (regular-file? path)
                   (has-suffix? path ".scm")
                   (access? path R_OK))
            (eval-protected `(primitive-load ,path))
            #f
          )))
      (closedir dir))
    #f
  )))

;; Load an rc file from the system configuration path (rather than the
;; regular Scheme load path)
(define (load-rc-from-sys-config-dirs basename)
  (define any (@ (srfi srfi-1) any))
  (define sys-config-dirs (@ (geda os) sys-config-dirs))

  (define (dir-has-file? dir ext)
    (let ((path (build-path dir
                            (string-append basename ext))))
      (and (file-exists? path)
           (regular-file? path)
           path)))

  (define (dir-has-scm? dir)
    (any (lambda (x) (dir-has-file? dir x))
          '("" ".scm")))

  (define (find-first-file)
    (any dir-has-scm? (sys-config-dirs)))

  (let ((rc-file (find-first-file)))
    ;; Use primitive-load to suppress autocompilation
    (if rc-file (primitive-load rc-file))))

;; Add all symbol libraries found below DIR to be searched for
;; components, naming them with an optional PREFIX.
(define* (component-library-search rootdir  #:optional prefix)
  (let ((dht (make-hash-table 31))
        (rootdir (expand-env-variables rootdir)))
    ;; Build symbol directory list
    (ftw rootdir
         (lambda (filename statinfo flags)
           (cond
            ((eq? 'invalid-stat flags)
             (error "Invalid path ~S." filename))
            ((or (eq? 'directory-not-readable flags)
                 (eq? 'symlink flags))
             (format #t "Warning: Cannot access ~S.\n" filename))
            (else
             (and (eq? 'regular flags)
                  (string-suffix-ci? ".sym" filename)
                  (hashq-set! dht
                              (string->symbol (dirname filename))
                              #t))))
           #t))

    ; Fill component library tree
    (for-each
     (lambda (dir)
       (let ((name (substring dir (string-length rootdir))))
         (component-library dir
                            (if prefix
                                (string-append prefix name)
                                name))))
     (sort-list! (hash-map->list (lambda (key val)
                                   (symbol->string key))
                                 dht)
                 string>?))))
