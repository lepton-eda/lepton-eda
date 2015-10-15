(define-module (symbol check pin-attrib)
  #:use-module (geda object)
  #:use-module (geda attrib)
  #:use-module (symbol gettext)
  #:use-module (symbol blame)

  #:export (check-pin-pintype))

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
