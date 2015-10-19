(define-module (symbol check pin-attrib)
  #:use-module (geda object)
  #:use-module (geda attrib)
  #:use-module (symbol gettext)
  #:use-module (symbol blame)

  #:export (check-pin-pintype
            check-pin-pinseq
            check-pin-required-attribs))

(define %valid-pintype-values
  '(in out io oc oe pas tp tri clk pwr))

(define (check-pin-pintype object)
  "Checks attributes 'pintype' of pin OBJECT."
  (define (found-attrib name value)
    (format #f (_ "Found ~A=~A attribute\n") name value))

  (define (invalid-attrib name value)
    (format #f (_ "Invalid ~A=~A attribute\n") name value))

  (and (pin? object)
       (for-each (lambda (attrib)
                   (when (string=? "pintype" (attrib-name attrib))
                     (let ((value (attrib-value attrib)))
                       (blame-object object
                                     'info
                                     (found-attrib 'pintype value))
                       (unless (memq (string->symbol value) %valid-pintype-values)
                         (blame-object object
                                       'error
                                       (invalid-attrib 'pintype value))))))
                 (object-attribs object))))

;;; Returns OBJECT's attribute pinseq= if it is valid, otherwise returns #f.
(define (check-pin-pinseq object)
  (let ((attrib-list (filter
                      (lambda (obj) (string=? (attrib-name obj) "pinseq"))
                      (object-attribs object))))

    (if (null? attrib-list)
        (begin
          (blame-object object
                        'error
                        (format #f (_ "Missing ~A= attribute\n") 'pinseq))
          #f)
        (if (null? (cdr attrib-list))
            (let ((value (attrib-value (car attrib-list))))
              (if (string=? value "0")
                  (begin
                    (blame-object object 'error (format #f (_ "Found ~A=~A attribute\n") 'pinseq value))
                    #f)
                  (car attrib-list)))
            (begin
              (blame-object object
                           'error
                           (format #f (_ "Found multiple ~A= attributes on one pin\n") 'pinseq))
              #f)))))


(define (check-pin-required-attribs object attr-name)
  "Checks pin required attributes ATTR-NAME of pin OBJECT."
  (define (filter-attrib object)
    (and (string=? (attrib-name object) attr-name)
         object))

  (and (pin? object)
       (let ((attrib-list (filter filter-attrib (object-attribs object))))
         (if (null? attrib-list)
             (blame-object object
                              'warning
                              (format #f (_ "Missing ~A= attribute\n") attr-name))
             (unless (null? (cdr attrib-list))
               (blame-object object
                             'error (format #f
                                            (_"Found multiple ~A=~A attributes on one pin\n")
                                            attr-name
                                            (attrib-value (car
                                                           ;; reverse attributes for backward
                                                           ;; compatibility
                                                           (reverse attrib-list))))))))))
