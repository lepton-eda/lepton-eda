; -*-Scheme-*-
(use-modules (geda os) (ice-9 optargs) (ice-9 ftw))
(define path-sep separator)
(define geda-data-path (car (sys-data-dirs)))
(define geda-rc-path (car (sys-config-dirs)))

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
            (eval-protected `(load ,path))
            #f
          )))
      (closedir dir))
    #f
  )))

;; Add all symbol libraries found below DIR to be searched for
;; components, naming them with an optional PREFIX.
(define* (component-library-search rootdir  #:optional prefix)
  (let ((dht (make-hash-table 31)))
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
