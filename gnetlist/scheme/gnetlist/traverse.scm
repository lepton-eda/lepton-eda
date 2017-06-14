(define-module (gnetlist traverse)

  ; Import C procedures and variables
  #:use-module (gnetlist core traverse)
  #:use-module (gnetlist core gettext)

  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (geda attrib)
  #:use-module (geda object)
  #:use-module (geda log)
  #:use-module (gnetlist config)
  #:use-module (gnetlist hierarchy)
  #:use-module (gnetlist rename)
  #:use-module (gnetlist net)
  #:use-module (gnetlist package)
  #:use-module (gnetlist package-pin)
  #:use-module (gnetlist pin-net)
  #:use-module (gnetlist verbose)
  #:use-module (symbol check net-attrib)

  #:export (traverse))


;;; Tracks which objects have been visited so far, and how many
;;; times.
(define %visits '())
;;; Increment the current visit count for a particular OBJECT.
(define (visit! object)
  (set! %visits (cons object %visits)))
;;; Retrieve the current visit count for a particular OBJECT.
(define (visited? object)
  (member object %visits))
;;; Reset all visit counts. Simply clears the %visits completely.
(define (clear-visits!)
  (set! %visits '()))

(define (traverse-net current-nets starting object hierarchy-tag)
  ;; We don't support other object types apart from net-pins and nets yet.
  (define (connection-type object)
    (or (net-pin? object)
        (net? object)
        (and (log! 'critical
                   (_ "Traverse nets: object type ~A is not supported.")
                   (object-type object))
             #f)))

  (define (make-new-net object tag)
    (verbose-print (if (pin? object)
                       (if starting "p" "P")
                       "n"))
    (if (pin? object)
        (let* ((conn-pair (net-return-connected-string object tag))
               (pinnum (and (eq? 'net-power (car conn-pair))
                            (cdr conn-pair))))
          `(,(object-id object)
            ,(not (not pinnum))
            ,(and pinnum
                  ;; use hierarchy tag here to make this net unique
                  (create-netattrib (netattrib-search-net (object-component object)
                                                          pinnum)
                                    hierarchy-tag))
            ,(and (not pinnum) conn-pair)))
        `(,(object-id object)
          #f
          ,(create-net-netname object tag)
          #f)))

  (when starting
    (clear-visits!))

  (visit! object)
  (if (connection-type object)
      (let ((nets (cons (make-new-net object hierarchy-tag) current-nets)))
        (if (or (not (pin? object))
                starting)
            (let loop ((connections (object-connections object))
                       (nets nets))
              (if (null? connections)
                  nets
                  (loop (cdr connections)
                        (let ((conn (car connections)))
                          (if (visited? conn)
                              nets
                              (traverse-net nets #f conn hierarchy-tag))))))
            nets))
      current-nets))


(define (list->nets object hierarchy-tag)
  (define package car)
  (define pinnumber cdr)
  (define (list->net ls)
    (match ls
      ((-1 . rest)
       #f)
     ((id priority name connection)
      (let ((conn-pair (or connection '(#f . #f))))
        (make-pin-net id
                      priority
                      name
                      (package conn-pair)
                      (pinnumber conn-pair))))
     (_ #f)))
  (let ((ls (if (null? (object-connections object))
                '()
                (reverse (traverse-net '() #t object hierarchy-tag)))))

    (filter-map list->net ls)))


(define %unnamed-net-counter 0)
(define (increment-unnamed-net-counter)
  (set! %unnamed-net-counter (1+ %unnamed-net-counter))
  %unnamed-net-counter)


(define (create-unnamed-netname tag netlist-mode)
  (define (hierarchical-default-name s)
    (create-netname (string-append (gnetlist-config-ref 'default-net-name) s)
                    tag))
  ((if (eq? netlist-mode 'spice) identity hierarchical-default-name)
   (number->string (increment-unnamed-net-counter))))


(define %unnamed-pin-counter 0)
(define (increment-unnamed-pin-counter)
  (set! %unnamed-pin-counter (1+ %unnamed-pin-counter))
  %unnamed-pin-counter)


(define (create-unconnected-netname)
  (string-append "unconnected_pin-"
                 (number->string (increment-unnamed-pin-counter))))

(define %netnames (make-hash-table))

(define (search-in-hash-table nets)
    (and (not (null? nets))
         (or (hash-ref %netnames (pin-net-id (car nets)))
             (search-in-hash-table (cdr nets)))))

(define (list->pins ls tag netlist-mode)
  (define (make-pin-attrib-list object)
    (define (add-attrib attrib)
      (cons (string->symbol (attrib-name attrib))
            (attrib-value attrib)))

    (map add-attrib (object-attribs object)))

  (define (make-special-netname nets)
    (if (null? nets)
        (create-unconnected-netname)
        (create-unnamed-netname tag netlist-mode)))

  (define (list->pin ls)
    (match ls
      ((#f . rest)
       #f)
      ((object number name label tag)
       (let* ((attribs (make-pin-attrib-list object))
              (nets (list->nets object tag))
              (netname (or name
                           ;; If there is no netname, probably
                           ;; some of nets has been already named.
                           (search-net-name nets)
                           ;; Didn't find a name.  Go looking for
                           ;; another net which might have already
                           ;; been named, i.e. we don't want to
                           ;; create a new unnamed net if the net
                           ;; has already been named before.
                           (search-in-hash-table nets)
                           ;; Last resort. We have not found a
                           ;; name. Make a new one.
                           (make-special-netname nets))))
         (and netname
              (for-each
               (lambda (net) (hash-set! %netnames (pin-net-id net) netname))
               nets))
         (make-package-pin (object-id object)
                           object
                           'net-pin
                           number
                           netname
                           label
                           attribs
                           nets)))
      (_ #f)))
  (filter-map list->pin ls))


;;; Searches for pinnumers in NET-MAPS and, if found, updates
;;; corresponding pins in PIN-LIST, otherwise creates new pins and
;;; adds them to the list.  ID, REFDES, and hierarchy TAG are used
;;; to create hierarchical net name.
(define (net-maps->pins net-maps id refdes tag pin-list)
  (define (pinnumber->pin pinnumber pin-list)
    (and (not (null? pin-list))
         (if (string=? (package-pin-number (car pin-list)) pinnumber)
             (car pin-list)
             (pinnumber->pin pinnumber (cdr pin-list)))))

  (define (check-shorted-nets a b priority)
    (log! 'critical
          (_ "Rename shorted nets (~A= has priority): ~A -> ~A")
          priority
          a
          b)
    (add-net-rename a b))

  (define (unnamed-net-or-unconnected-pin? name)
    (or (string-prefix? "unnamed_net" name)
        (string-prefix? "unconnected_pin" name)))

  (define (update-pin-netname pin netname id refdes)
    (let ((nets (package-pin-nets pin))
          (pinnumber (package-pin-number pin))
          (net-priority #t))
      (set-package-pin-name! pin netname)
      (if (null? nets)
          (set-package-pin-nets! pin
                                 (list (make-pin-net id
                                                     net-priority
                                                     netname
                                                     refdes
                                                     pinnumber)))
          (let ((net (car nets)))
            (set-pin-net-id! net id)
            (set-pin-net-priority! net net-priority)
            (set-pin-net-name! net netname)
            (set-pin-net-connection-package! net refdes)
            (set-pin-net-connection-pinnumber! net pinnumber)))))

  (define (update-pin pin net-map id refdes tag)
    (and refdes
         (let ((netname (create-netattrib (net-map-netname net-map) tag))
               (pin-netname (package-pin-name pin)))
           (if (and pin-netname
                    (not (unnamed-net-or-unconnected-pin? pin-netname)))
               (if (gnetlist-config-ref 'netname-attribute-priority)
                   (check-shorted-nets netname pin-netname 'netname)
                   (check-shorted-nets pin-netname netname 'net))
               (begin
                 (when (unnamed-net-or-unconnected-pin? pin-netname)
                   ;; Rename unconnected pins and unnamed nets.
                   (add-net-rename pin-netname netname))
                 (update-pin-netname pin netname id refdes))))
         #f))

  (define (make-net-map-pin net-map id refdes tag)
    (let* ((pinnumber (net-map-pinnumber net-map))
           (netname (create-netattrib (net-map-netname net-map) tag))
           (net-priority #t)
           (label #f)
           (object #f)
           (attribs '())
           (nets (list (make-pin-net id net-priority netname refdes pinnumber))))
      (make-package-pin id object 'net pinnumber netname label attribs nets)))

  (append pin-list
          (filter-map
           (lambda (net-map)
             (let ((pin (pinnumber->pin (net-map-pinnumber net-map) pin-list)))
               (if pin
                   (update-pin pin net-map id refdes tag)
                   (make-net-map-pin net-map id refdes tag))))
           net-maps)))

(define (list->packages ls netlist-mode)
  ;; Makes attribute list of OBJECT using getter GET-ATTRIBS.
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

  (define (list->package ls)
    (match ls
      ((refdes tag composite #f pins)
       #f)
      ((refdes tag composite object pins)
       (make-package (object-id object)
                     refdes
                     tag
                     composite
                     object
                     (make-attrib-list inherited-attribs object)
                     (make-attrib-list object-attribs object)
                     (net-maps->pins (check-net-maps object)
                                     (object-id object)
                                     refdes
                                     tag
                                     (list->pins pins tag netlist-mode))))
      (_ #f)))
  (filter-map list->package ls))

(define (traverse netlist-mode)
  (reset-rename!)
  (let ((cwd (getcwd))
        (netlist (list->packages (%traverse netlist-mode)
                                 netlist-mode)))
    ;; Change back to the directory where we started.  This is
    ;; done because (%traverse) can change the current working
    ;; directory.
    (chdir cwd)
    (rename-all (hierarchy-post-process netlist))))
