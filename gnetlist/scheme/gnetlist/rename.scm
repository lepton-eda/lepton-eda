(define-module (gnetlist rename)
  #:use-module (srfi srfi-69)
  #:use-module (gnetlist core gettext)
  #:use-module (gnetlist verbose)
  #:use-module (gnetlist package)
  #:use-module (gnetlist package-pin)

  #:export (search-rename
            add-rename
            get-rename-list
            reset-rename!
            rename-all
            get-rename-list))

(define (rename-lowlevel netlist from to)

  (define (rename-in-pin pin)
    (if (string=? from (package-pin-name pin))
        (set-package-pin-name! pin to)))

  (define (rename-in-package package)
    (for-each rename-in-pin (package-pins package)))

  (for-each rename-in-package netlist)
  netlist)

(define %renames (make-hash-table))

(define (renamed? from)
  (hash-table-ref/default %renames from #f))

(define (set-rename! from to)
  (hash-table-set! %renames from to))

(define-public (reset-rename!)
  (set! %renames (make-hash-table)))

(define (update-rename from to)
  (let ((from-rename (renamed? from))
        (to-rename   (renamed? to)))
    (cond
     ;; Forward rename already exists
     ((and from-rename (equal? to from-rename)) #t)
     ;; Reverse rename already exists
     ((and to-rename (equal? from to-rename)) #t)
     ;; we found a -> b, while adding c -> a.
     ;; hence we would have c -> a -> b, so add c -> b.
     ;; avoid renaming if b is same as c!
     (to-rename (set-rename! from to-rename))
     ;; we found a -> b, while adding a -> c.
     ;; hence b <==> c, so add c -> b.
     ;; avoid renaming if b is same as c!
     (from-rename (set-rename! to from-rename))
     ;; No pre-existing renames
     (else (set-rename! from to)))))

(define-public (add-rename from to)
  (and from to (update-rename from to)))

;;; if the src is found, return true
;;; if the dest is found, also return true, but warn user
;;; If quiet flag is true than don't print anything
(define (search-rename from to quiet)
  ;; FIXME[2017-02-20] This warning message is very unhelpful
  (define (warn)
    (when (not quiet)
          (format (current-error-port)
                  (_ "WARNING: Trying to rename something twice:
\t~A and ~A
are both a src and dest name
This warning is okay if you have multiple levels of hierarchy!
")
                  to
                  to)))

  (cond
   ((renamed? from) #t)
   ((renamed? to) (begin (warn) #t))
   (else #f)))

(define-public (rename-all netlist)
  (verbose-print "- Renaming nets:\n")
  (hash-table-walk %renames
    (lambda (from to)
      (verbose-print "R")
      (rename-lowlevel netlist from to)))
  (verbose-done)
  netlist)

;; Return the alist of renames.  Sort it so that it's in a canonical
;; order no matter what the internal implementation of the hash table
;; is.
(define-public (get-rename-list)
  (sort! (hash-table->alist %renames)
         (lambda (left right)
           (or (string<? (cdr left) (cdr right))
               (and (string= (cdr left) (cdr right))
                    (string<? (car left) (car right)))))))
