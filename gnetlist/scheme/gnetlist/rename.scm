(define-module (gnetlist rename)
  #:use-module (srfi srfi-26)
  #:use-module (gnetlist core gettext)
  #:use-module (gnetlist verbose)
  #:use-module (gnetlist package)
  #:use-module (gnetlist package-pin)

  #:export (search-rename
            add-rename
            get-rename-list
            set-rename-list!
            rename-all))

;;; List of renamings
(define %rename-list '())

;;; Setter for the list of renamings
(define (set-rename-list! ls)
  (set! %rename-list ls)
  %rename-list)

;;; Getter for the list of renamings
;;
;; Returns the rename alist sorted by rename destination, then by
;; rename source.
(define (%get-rename-list)
  %rename-list)

(define (get-rename-list)
  (sort (%get-rename-list) rename<?))

(let ((rename-ls '()))
  (set! set-rename-list!
        (lambda (ls) (set! rename-ls ls) rename-ls))
  (set! %get-rename-list
        (lambda () rename-ls)))

;;; Getters for rename pairs
(define source car)
(define destination cdr)

;;; Comparison function for renames
(define (rename<? left right)
  (or
   (string<? (destination left) (destination right))
   (and (string=? (destination left) (destination right))
        (string<? (source left) (source right)))))

;;; if the src is found, return true
;;; if the dest is found, also return true, but warn user
;;; If quiet flag is true than don't print anything
(define (search-rename src dest quiet)
  (let loop ((ls (%get-rename-list)))
    (and (not (null? ls))
         (let* ((rename (car ls))
                (current-source (source rename)))
           (or (string=? src current-source)
               (and (string=? dest current-source)
                    (when (not quiet)
                      (format (current-error-port)
                              (_ "WARNING: Trying to rename something twice:
\t~A and ~A
are both a src and dest name
This warning is okay if you have multiple levels of hierarchy!
")
                              dest
                              current-source))
                    #t)
               (loop (cdr ls)))))))

(define (update-rename-list rename-list dest src)
  (define (get-new-rename src dest rename)
    (or (and (string=? dest (source rename))
             (not (string=? src (destination rename)))
             ;; we found a -> b, while adding c -> a.
             ;; hence we would have c -> a -> b, so add c -> b.
             ;; avoid renaming if b is same as c!
             (cons src (destination rename)))
        (and (string=? src (source rename))
             (not (string=? dest (destination rename)))
             ;; we found a -> b, while adding a -> c.
             ;; hence b <==> c, so add c -> b.
             ;; avoid renaming if b is same as c!
             (cons dest (destination rename)))))

  (let loop ((ls rename-list)
             (new-ls '()))
    (if (null? ls)
        (append new-ls rename-list)
        (loop (cdr ls)
              (let ((rename (get-new-rename src dest (car ls))))
                (if rename
                    (cons rename new-ls)
                    new-ls))))))

(define (add-rename src dest)
  (and src
       dest
       (set-rename-list!
        (if (search-rename src dest #f)
            (update-rename-list (%get-rename-list) src dest)
            `((,src . ,dest) . ,(%get-rename-list))))))


(define (rename-lowlevel netlist rename)
  (define (rename-it net-name rename)
    (if (and net-name
             (string=? net-name (source rename)))
        (destination rename)
        net-name))

  (define (rename-in-pin pin rename)
    (set-package-pin-name! pin (rename-it (package-pin-name pin) rename)))

  (define (rename-in-pin-list pins rename)
    (for-each (cut rename-in-pin <> rename) pins))

  (define (rename-in-package package rename)
    (rename-in-pin-list (package-pins package) rename))

  (for-each (cut rename-in-package <> rename) netlist)
  netlist)


(define (rename-all netlist)
  (verbose-print "- Renaming nets:\n")
  (let loop ((rename-list (%get-rename-list))
             (netlist netlist))
    (if (null? rename-list)
        (begin
          (verbose-done)
          netlist)
        (begin
          (verbose-print "R")
          (loop (cdr rename-list)
                (rename-lowlevel netlist
                                 (car rename-list)))))))
