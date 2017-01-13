(define-module (gnetlist package)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:export (make-package package?
            package-id set-package-id!
            package-refdes set-package-refdes!
            package-tag set-package-tag!
            package-composite? set-package-composite!
            package-object set-package-object!
            package-iattribs set-package-iattribs!
            package-attribs set-package-attribs!
            package-pins set-package-pins!
            package-attributes
            package-attribute
            package-graphical?))

(define-record-type <package>
  (make-package id refdes tag composite object iattribs attribs pins)
  package?
  (id package-id set-package-id!)
  (refdes package-refdes set-package-refdes!)
  (tag package-tag set-package-tag!)
  (composite package-composite? set-package-composite!)
  (object package-object set-package-object!)
  (iattribs package-iattribs set-package-iattribs!)
  (attribs package-attribs set-package-attribs!)
  (pins package-pins set-package-pins!))

(set-record-type-printer!
 <package>
 (lambda (record port) (format port "#<geda-package ~A>" (package-id record))))


(define (package-attributes package name)
  "Returns the list of attached attributes called NAME for
PACKAGE. NAME must be a Scheme symbol (not string). If no attached
attributes found, returns the list of inherited attributes with
the same name. If neither attached nor inherited attributes have
been found, returns #f."
  (or (assq-ref (package-attribs package) name)
      (assq-ref (package-iattribs package) name)))


(define (package-attribute package name)
  "Returns first attached attribute of PACKAGE called NAME. NAME
must be a Scheme symbol (not string). If no attached attribute
found, returns first inherited attribute with NAME. If neither
attached nor inherited attribute found, returns #f."
  (and=> (package-attributes package name) car))

(define (package-graphical? package)
  "Returns #t if PACKAGE is graphical, that is, it has attribute
\"graphical=1\", otherwise returns #f."
  (and=> (package-attribute package 'graphical)
         (lambda (x) (string=? x "1"))))
