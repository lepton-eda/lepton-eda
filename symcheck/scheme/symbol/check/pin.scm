(define-module (symbol check pin)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:use-module (geda object)
  #:use-module (geda attrib)
  #:use-module (symbol gettext)
  #:use-module (symbol blame)
  #:use-module (symbol check alignment)
  #:use-module (symbol check attrib)
  #:use-module (symbol check connection)
  #:use-module (symbol check line)
  #:use-module (symbol check obsolete)

  #:export-syntax (make-symbol-pin symbol-pin?
                   symbol-pin-object set-symbol-pin-object!
                   symbol-pin-seq set-symbol-pin-seq!
                   symbol-pin-type set-symbol-pin-type!
                   symbol-pin-number set-symbol-pin-number!
                   symbol-pin-label set-symbol-pin-label!
                   symbol-pin-attribs set-symbol-pin-attribs!)

  #:export (check-pin))


(define-record-type <symbol-pin>
  (make-symbol-pin object seq type number label attribs)
  symbol-pin?
  (object symbol-pin-object set-symbol-pin-object!)
  (seq symbol-pin-seq set-symbol-pin-seq!)
  (type symbol-pin-type set-symbol-pin-type!)
  (number symbol-pin-number set-symbol-pin-number!)
  (label symbol-pin-label set-symbol-pin-label!)
  (attribs symbol-pin-attribs set-symbol-pin-attribs!))

(define (check-pin-attrib-duplicates ls)
  "Checks for duplicated attributes in object list LS."
  (define (blame-duplicate object)
    (blame-object object
                  'error
                  (format #f
                          (_ "Duplicate pin attribute on one pin: ~A")
                          (attrib-name object))))
  (unless (null? (cdr ls))
    (for-each blame-duplicate ls))
  (car ls))

(define (check-attrib entry)
  (match entry
    ((name . objects)
     `(,name . ,(check-obsolete-pin-attrib
                 (check-pin-attrib-duplicates objects))))
    (_ (error "Invalid pin attribute list."))))


(define-syntax-rule (blame-zero-value object name)
  (blame-object object
                'error
                (format #f
                        (_ "Prohibited zero value pin attribute: ~A=0")
                        name)))

(define (check-pin-attrib-value object name value)
  (match `(,name . ,value)
    (((or 'pinseq 'pinnumber) . "0") (blame-zero-value object name) #f)
    (_ value)))

(define-syntax-rule (blame-missing-or-wrong pin severity name attrib-alist)
  (let ((attrib (assq-ref attrib-alist name)))
    (unless attrib
      (blame-object pin
                    severity
                    (format #f (_ "Missing pin attribute: ~A") name)))
    (check-pin-attrib-value attrib
                            name
                            (and=> attrib attrib-value))))

(define (pin->symbol-pin object)
  (let ((attrib-alist (map check-attrib
                           (attribs->attrib-alist (object-attribs object)))))
    (make-symbol-pin object
                     (blame-missing-or-wrong object 'error 'pinseq attrib-alist)
                     (blame-missing-or-wrong object 'warning 'pintype attrib-alist)
                     (blame-missing-or-wrong object 'error 'pinnumber attrib-alist)
                     (blame-missing-or-wrong object 'warning 'pinlabel attrib-alist)
                     attrib-alist)))

(define (check-pin object)
  "Checks pin OBJECT.
  * Checks that it has non-zero size.
  * Checks if it has forbidden connections.
  * Checks that it is properly aligned by grid.
Returns record <symbol-pin>."
  (check-line-size object)
  (check-connections object)
  (check-pin-alignment object)
  (pin->symbol-pin object))
