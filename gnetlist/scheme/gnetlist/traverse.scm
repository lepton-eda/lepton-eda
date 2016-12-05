(define-module (gnetlist traverse)

  ; Import C procedures and variables
  #:use-module (gnetlist core traverse)

  #:use-module (ice-9 match)
  #:use-module (geda attrib)
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
  (define iattribs '())
  (define attribs '())
  (define (list->package ls)
    (match ls
      ((id refdes tag composite object pins)
       (make-package id refdes tag composite object iattribs attribs (list->pins pins)))
      (_ #f)))
  (map list->package ls))


;;; Makes attribute list of OBJECT so the attached attribs have
;;; precedence over inherited once.
(define (make-attrib-list get-attribs object)
  (define (add-attrib ls attrib)
    (let* ((name (string->symbol (attrib-name attrib)))
           (prev-value (assq-ref ls name))
           (new-value (attrib-value attrib)))
      (if prev-value
          (assq-set! ls name (cons new-value prev-value))
          (acons name (list new-value) ls))))

  (let loop ((in (get-attribs object))
             (out '()))
    (if (null? in)
        out
        (loop (cdr in)
              (add-attrib out (car in))))))


;;; Sets "attribs" field of each package in NETLIST according to
;;; its object data. Returns the modified NETLIST.
(define (netlist-collect-attribs netlist)
  (define (parse-attrib* attrib)
    (let ((name-value (parse-attrib attrib)))
      (cons (string->symbol (car name-value))
            (cdr name-value))))

  (for-each
   (lambda (package)
     (let ((object (package-object package)))
       (and object
            (set-package-iattribs! package
                                   (make-attrib-list inherited-attribs object))
            (set-package-attribs! package
                                  (make-attrib-list object-attribs object))
            package)))
   netlist)

  netlist)


(define (traverse)
  (netlist-collect-attribs (list->packages (%traverse))))
