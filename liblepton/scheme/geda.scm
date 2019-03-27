; -*-Scheme-*-
(use-modules (ice-9 ftw)
             (geda os)
             (lepton library))

;; Clean up logfiles
;; FIXME this should be a plugin
(use-modules (geda log-rotate))


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
