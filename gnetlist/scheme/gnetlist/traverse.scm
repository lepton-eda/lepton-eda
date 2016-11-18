(define-module (gnetlist traverse)

  ; Import C procedures and variables
  #:use-module (gnetlist core traverse)

  #:use-module (ice-9 match)
  #:use-module (gnetlist package)
  #:use-module (gnetlist package-pin)
  #:use-module (gnetlist pin-net)
  #:export (traverse))

;;; Temporary function to deal with gnetlist's net->connected_to
;;; entries.
(define (split-string-by-char s ch)
  (let ((space-pos (string-index s ch)))
    (and space-pos
         (cons (string-take s space-pos)
               (string-drop s (1+ space-pos))))))

(define (list->nets ls)
  (define package car)
  (define pinnumber cdr)
  (define (list->net ls)
    (match ls
     ((id priority name connection)
      (let ((conn-pair (if connection
                           (split-string-by-char connection #\space)
                           '(#f . #f))))
        (make-pin-net id
                      priority
                      name
                      (package conn-pair)
                      (pinnumber conn-pair))))
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
