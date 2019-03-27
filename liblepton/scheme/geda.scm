; -*-Scheme-*-
(use-modules (ice-9 ftw)
             (srfi srfi-1)
             (geda os)
             (lepton file-system)
             (lepton library))

(define path-sep file-name-separator-string)

;; Clean up logfiles
;; FIXME this should be a plugin
(use-modules (geda log-rotate))

;; Legacy gEDA data & configuration directories.  These functions will
;; usually return #f if gEDA was compiled with --disable-deprecated.
;; Use the sys-data-dirs and sys-config-dirs functions from the (geda
;; os) module instead.
(define geda-data-path (or (getenv "GEDADATA")
                           (last (sys-data-dirs))))
(define geda-rc-path (or (getenv "GEDADATARC") (getenv "GEDADATA")
                         (last (sys-config-dirs))))

(define (build-path first . rest)
  (string-join (cons first rest) file-name-separator-string))

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
                   (string-suffix? ".scm" path)
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
  (let ((rc-file (search-path (sys-config-dirs) basename '("" ".scm"))))
    ;; Use primitive-load to suppress autocompilation
    (if rc-file (primitive-load rc-file))))

;; Add all symbol libraries found below DIR to be searched for
;; components, naming them with an optional PREFIX.
(define* (component-library-search rootdir  #:optional (prefix ""))
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
         (component-library dir (string-append prefix name))))
     (sort-list! (hash-map->list (lambda (key val)
                                   (symbol->string key))
                                 dht)
                 string>?))))
