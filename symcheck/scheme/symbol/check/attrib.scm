(define-module (symbol check attrib)
  #:use-module (geda object)
  #:use-module (geda attrib)
  #:use-module (symbol gettext)
  #:use-module (symbol blame)

  #:export (graphical-attrib?
            check-attribute
            check-device-attribs))


(define (graphical-attrib? object)
  "Checks if object is attribute 'graphical=1'."
  (and (attribute? object)
       (not (attrib-attachment object)) ; floating
       (string=? (attrib-name object) "graphical")
       (string=? (attrib-value object) "1")))

(define (check-device-attribs is-graphical? device-list)
  "Checks device= attributes in DEVICE-LIST. If the list contains
more than one attribute, adds error on that. If schematic symbol
is graphical, that is, IS-GRAPHICAL? is #t, also checks for
device= value which should be 'none' for graphical symbols."
  (if (null? (cdr device-list))
      (let* ((device (car device-list))
             (value (attrib-value device)))
        (blame-object device 'info (format #f (_ "Found ~A=~A\n") 'device value))
        (when is-graphical?
          ;; Check for "device=none" for graphical symbols.
          (if (string=? value "none")
              (blame-object device 'info
                            (format #f
                                    (_ "Found graphical symbol, ~A=~A\n")
                                    'device
                                    value))
              (blame-object device 'warning
                            (format #f
                                    (_"Found graphical symbol, device= should be set to none\n"))))))
      (for-each (lambda (object)
                  (blame-object object
                                'error
                                (format #f (_ "Conflicting attribute: ~A.\n") 'device)))
                device-list)))


(define (check-attribute object)
  "Checks attribute OBJECT."
  (and (attribute? object)
   (let ((aname  (string->symbol (attrib-name object)))
         (avalue (attrib-value object)))

     (case aname
       ((type name)
        (blame-object object
                      'error
                      (format #f
                              (_ "Found forbidden ~A= attribute: [~A=~A]\n")
                              aname
                              aname
                              avalue)))

       ((uref label email)
        (blame-object object
                      'warning
                      (format #f
                              (_ "Found obsolete ~A= attribute: [~A=~A]\n")
                              aname
                              aname
                              avalue)))

       ;; Valid pin attributes.
       ((pinlabel pintype pinseq pinnumber)
        (if (or (not (attrib-attachment object))
                (not (pin? (attrib-attachment object))))
            (blame-object object
                          'error
                          (format #f
                                  (_ "Found misplaced pin attribute: [~A=~A]\n")
                                  aname
                                  avalue))))

       ;; Valid attributes.
       ((device graphical description author
                comment numslots slotdef footprint
                documentation refdes slot net
                value symversion dist-license use-license)
        ;; Check if they are floating (not attached to anything).
        (if (attrib-attachment object)
            (blame-object object
                          'error
                          (format #f
                                  (_ "Found wrongly attached attribute: [~A=~A]\n")
                                  aname
                                  avalue))))

       ;; All other attributes are unknown.
       (else (blame-object object
                           'warning
                           (format #f
                                   (_ "Found unknown ~A= attribute: [~A=~A]\n")
                                   aname
                                   aname
                                   avalue)))))))
