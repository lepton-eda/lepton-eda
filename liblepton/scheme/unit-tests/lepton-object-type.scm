;;; Test Scheme procedures working with object type.

(use-modules (srfi srfi-1)
             (srfi srfi-26)
             (system foreign)
             (lepton object foreign)
             (lepton object type)
             (lepton object))

(define test-image
  (map char->integer (string->list
"/* XPM */
static char * test_image_xpm[] = {
\"2 1 1 1\",
\"      c None\",
\"  \"};
")))

(define arc (make-arc '(1 . 2) 3 45 90))
(define box (make-box '(1 . 4) '(3 . 2)))
(define bus (make-bus '(1 . 2) '(3 . 4)))
(define circle (make-circle '(1 . 2) 3))
(define component (make-component "component" '(1 . 2) 0 #t #f))
(define line (make-line '(1 . 2) '(3 . 4)))
(define net (make-net '(1 . 2) '(3 . 4)))
(define path (make-path))
(define picture (make-picture/vector test-image
                                     "test_image.xpm"
                                     '(1 . 2)
                                     '(5 . 4)
                                     0
                                     #f))
(define bus-pin (make-bus-pin '(1 . 2) '(3 . 4)))
(define net-pin (make-net-pin '(1 . 2) '(3 . 4)))
(define text (make-text '(1 . 2) 'lower-left 0 "text" 10 #t 'both))


(test-begin "object-type")

(test-eq (object-type arc) 'arc)
(test-eq (object-type box) 'box)
(test-eq (object-type bus) 'bus)
(test-eq (object-type circle) 'circle)
(test-eq (object-type component) 'complex)
(test-eq (object-type line) 'line)
(test-eq (object-type net) 'net)
(test-eq (object-type path) 'path)
(test-eq (object-type picture) 'picture)
(test-eq (object-type bus-pin) 'pin)
(test-eq (object-type net-pin) 'pin)
(test-eq (object-type text) 'text)

(test-end "object-type")


(test-begin "object-type?")

(test-assert (object-type? arc 'arc))
(test-assert (object-type? box 'box))
(test-assert (object-type? bus 'bus))
(test-assert (object-type? circle 'circle))
(test-assert (object-type? component 'complex))
(test-assert (object-type? line 'line))
(test-assert (object-type? net 'net))
(test-assert (object-type? path 'path))
(test-assert (object-type? picture 'picture))
(test-assert (object-type? bus-pin 'pin))
(test-assert (object-type? net-pin 'pin))
(test-assert (object-type? text 'text))

(test-end "object-type?")

(define object-func-list
  `((,arc . ,arc?)
    (,box . ,box?)
    (,bus . ,bus?)
    (,circle . ,circle?)
    (,component . ,component?)
    (,line . ,line?)
    (,net . ,net?)
    (,picture . ,picture?)
    (,bus-pin . ,pin?)
    (,bus-pin . ,bus-pin?)
    (,net-pin . ,pin?)
    (,net-pin . ,net-pin?)
    (,path . ,path?)
    (,text . ,text?)))

(define object-list
  (map car object-func-list))

(define func-list
  (delete-duplicates (map cdr object-func-list)))

(define (exclude object ls)
  (define not-equal? (negate equal?))
  (filter (cut not-equal? object <>) ls))

(test-begin "check-object-type")

(for-each
 (lambda (x)
   (let* ((object (car x))
         (check-func (cdr x))
         (other-funcs (exclude check-func func-list)))
     ;; Test corresponding functions, e.g. (arc? arc).
     (test-assert (check-func object))
     ;; Test other objects, e.g. (arc? box).
     (for-each
      (lambda (func)
        (unless (or (and (equal? func pin?)
                         (equal? check-func bus-pin?))
                    (and (equal? func pin?)
                         (equal? check-func net-pin?))
                    (and (equal? func bus-pin?)
                         (equal? check-func pin?))
                    (and (equal? func net-pin?)
                         (equal? check-func pin?)))
          (test-assert (not (func object)))))
      other-funcs)
     ;; Test wrong objects.
     (for-each (lambda (x) (not (check-func x))) '(1 anything #t #f))))
 object-func-list)


(test-end "check-object-type")

(define not-null-pointer? (negate null-pointer?))

(test-begin "object-pointer")

;;; Check that all objects are correctly transformed to foreign
;;; pointers.
(for-each
 (lambda (object)
   (test-assert (not-null-pointer? (check-object object 1))))
 object-list)

(test-assert-thrown 'wrong-type-arg (check-object 'anything 1))

(for-each
 (lambda (x)
   (let* ((object (car x))
          (check-func (cdr x))
          (other-funcs (exclude check-func func-list))
          (pointer (check-object object 1 check-func 'anything)))

     ;; Test that pointer is not %null-pointer.
     (test-assert (not-null-pointer? pointer))
     ;; Test check-object() with wrong object function.
     ;; For example, (check-object arc 1 box? 'anything).
     (for-each
      (lambda (func)
        (unless (or (and (equal? func pin?)
                         (equal? check-func bus-pin?))
                    (and (equal? func pin?)
                         (equal? check-func net-pin?))
                    (and (equal? func bus-pin?)
                         (equal? check-func pin?))
                    (and (equal? func net-pin?)
                         (equal? check-func pin?)))
          (test-assert-thrown 'wrong-type-arg
                              (check-object object 1 func 'anything))))
      other-funcs)

     ;; Test pointer->object().
     (test-assert (check-func (pointer->object pointer)))))
 object-func-list)

;;; Test converting of wrong object pointers.
(test-assert-thrown 'wrong-type-arg (pointer->object 'anything))
(test-assert-thrown 'wrong-type-arg (pointer->object #f))
(test-assert-thrown 'misc-error (pointer->object %null-pointer))

(test-end "object-pointer")
