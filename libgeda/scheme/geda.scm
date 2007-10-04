; -*-Scheme-*-
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
            (load path)
            #f
          )))
      (closedir dir))
    #f
  )))

