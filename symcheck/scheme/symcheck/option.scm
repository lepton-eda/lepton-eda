(define-module (symcheck option)
  #:use-module (ice-9 getopt-long)
  #:use-module ((srfi srfi-1) #:select (filter-map))
  #:export (%default-symcheck-options
            symcheck-option-ref
            symcheck-option-ref-length))

;;; Empty lists are default values for the keys which may repeat
;;; on command line.
(define %default-symcheck-options
  '((quiet . #f)
    (verbose . ())
    (help . #f)
    (interactive . #f)))

;;; This list contains key names which values must be lists.
(define %list-keys
  (filter-map
   (lambda (x) (and (eq? (cdr x) '()) (car x)))
   %default-symcheck-options))

;;; getopt-long compatible symcheck options.
(define %symcheck-options
  (getopt-long (program-arguments)
               ;; option spec
               '((quiet (single-char #\q))
                 (verbose (single-char #\v))
                 (help (single-char #\h))
                 (interactive (single-char #\i)))))

;;; This function extends option-ref so that for keys which may
;;; repeat on command line, it returns their value as value lists
;;; (e.g. "cmd -x a -x b" produces '("a" "b") for the key 'x).
 (define (list-option-ref options key default)
  (or (filter-map
       (lambda (x) (and (eq? (car x) key) (cdr x)))
       options)
      default))

(define (symcheck-option-ref key)
  "Returns value of symcheck option KEY. Use '() to request schematics."
  (let ((default (assq-ref %default-symcheck-options key))
        (is-list-key? (memq key %list-keys)))
    ((if is-list-key? list-option-ref option-ref) %symcheck-options key default)))

(define (symcheck-option-ref-length key)
  (length (symcheck-option-ref key)))
