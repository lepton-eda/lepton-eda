(define-module (gnetlist traverse)

  ; Import C procedures and variables
  #:use-module (gnetlist core traverse)

  #:use-module (ice-9 match)
  #:use-module (gnetlist package)
  #:use-module (gnetlist package-pin)
  #:use-module (gnetlist pin-net)
  #:export (traverse))

(define (list->nets ls)
  (define (list->net ls)
    (match ls
     ((id priority name connection)
      (make-pin-net id priority name connection))
     (_ #f)))
  (map list->net ls))

(define (list->pins ls)
  (define attribs '())
  (define object #f)
  (define (list->pin ls)
    (match ls
      ((id type number name label nets)
       (make-package-pin id object type number name label attribs (list->nets nets)))
      (_ #f)))
  (map list->pin ls))

(define (list->packages ls)
  (define attribs '())
  (define (list->package ls)
    (match ls
      ((id refdes tag composite object pins)
       (make-package id refdes tag composite object attribs (list->pins pins)))
      (_ #f)))
  (map list->package ls))

(define (traverse)
  (list->packages (%traverse)))
