(define-module (gnetlist sort)
  #:use-module (srfi srfi-1)
  #:export (sort-remove-duplicates))

(define (sort-remove-duplicates ls sort-func)
  (let ((ls (sort ls sort-func)))
    (fold-right
     (lambda (elem ret)
       (if (equal? elem (first ret))
           ret
           (cons elem ret)))
     (last-pair ls)
     ls)))
