(define-module (gnetlist pin-net)
  #:use-module (srfi srfi-9)
  #:export (make-pin-net pin-net?
            pin-net-id set-pin-net-id!
            pin-net-priority set-pin-net-priority!
            pin-net-name set-pin-net-name!
            pin-net-connection set-pin-net-connection!))

(define-record-type <pin-net>
  (make-pin-net id priority name connection)
  pin-net?
  (id pin-net-id set-pin-net-id!)
  (priority pin-net-priority set-pin-net-priority!)
  (name pin-net-name set-pin-net-name!)
  (connection pin-net-connection set-pin-net-connection!))
