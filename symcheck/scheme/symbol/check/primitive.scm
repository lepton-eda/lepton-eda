(define-module (symbol check primitive)
  #:use-module (geda object)
  #:use-module (geda attrib)
  #:use-module (symbol gettext)
  #:use-module (symbol blame)
  #:use-module (symbol check text)
  #:use-module (symbol check connection)

  #:export (check-primitive))

(define (object-info object)
  "Returns OBJECT info string."
  (case (object-type object)
    ((arc) (arc-info object))
    ((box) (box-info object))
    ((bus net line pin) (line-info object))
    ((circle) (circle-info object))
    ((complex) (component-info object))
    ((path) (path-info object))
    ((picture) (picture-info object))
    ((text) (text-info object))))


(define (check-primitive object)
  "Checks OBJECT.
Carries out appropriate checks for each type of objects. Returns
OBJECT if it needs other checks. Otherwise returns #f."
  (blame-object object
                'info
                (format #f (_ "Object: ~A") (cons (object-type object)
                                                  (object-info object))))
  ;; Specific checks for every object.
  ;; #f means no other check is needed.
  (case (object-type object)
    ((arc) #f)
    ((box) #f)
    ((net bus) (check-connections object) (check-net/bus object) #f)
    ((circle) #f)
    ((complex) #f)
    ((line) #f)
    ((path) #f)
    ((picture) #f)
    ((pin) (check-connections object) object)
    ((text)
     (check-text object)
     (and (attribute? object) object))
    (else (error "Unknown object ~A" object))))
