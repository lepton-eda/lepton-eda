;;; Test Scheme procedures related to pages' changed flags.

(use-modules ((geda page) #:renamer (symbol-prefix-proc 'geda:))
             (lepton attrib)
             (lepton object))

;;; Utility macro to avoid boilerplate
(define-syntax geda:assert-dirties
  (syntax-rules ()
    ((_ P . test-forms)
     (begin (begin . test-forms)
            (test-assert (geda:page-dirty? P))
            (geda:set-page-dirty! P #f)))))

(define-syntax geda:assert-not-dirties
  (syntax-rules ()
    ((_ P . test-forms)
     (begin (begin . test-forms)
            (test-assert (not (geda:page-dirty? P)))))))


(test-begin "geda:page-dirty")

(let ((P (geda:make-page "/test/page/A"))
      (C (make-component "test component" '(1 . 2) 0 #t #f)))

  ;; Make sure pages are cleaned up
  (dynamic-wind
    (lambda () #f)
    (lambda ()
      (test-assert (not (geda:page-dirty? P)))

      (geda:set-page-dirty! P)
      (test-assert (geda:page-dirty? P))

      (geda:set-page-dirty! P #f)
      (test-assert (not (geda:page-dirty? P)))

      (geda:assert-dirties P (geda:set-page-dirty! P #t))
      (geda:assert-dirties P (geda:page-append! P C))
      (geda:assert-dirties P (geda:page-remove! P C)))
    (lambda () (geda:close-page! P))))

(test-end "geda:page-dirty")


(test-begin "geda:page-dirty-objects")

(let ((P (geda:make-page "/test/page/A"))
      (l (make-line '(1 . 2) '(3 . 4)))
      (b (make-box '(1 . 4) '(3 . 2)))
      (c (make-circle '(1 . 2) 3))
      (a (make-arc '(1 . 2) 3 45 90))
      (t (make-text '(1 . 2) 'lower-left 0 "test text" 10 #t 'both))
      (C (make-component "test component" '(1 . 2) 0 #t #f)))

  ;; Make sure pages are cleaned up
  (dynamic-wind
    (lambda () #f)
    (lambda ()

      ;; Add everything to the page
      (geda:assert-dirties P (for-each (lambda (x) (geda:page-append! P x))
                                       (list l b c a t C)))

      (geda:assert-not-dirties P (apply set-line! l (line-info l)))
      (geda:assert-not-dirties P (apply set-box! b (box-info b)))
      (geda:assert-not-dirties P (apply set-circle! c (circle-info c)))
      (geda:assert-not-dirties P (apply set-arc! a (arc-info a)))
      (geda:assert-not-dirties P (apply set-text! t (text-info t)))
      (geda:assert-dirties P (apply set-component! C
                                    (list-tail (component-info C) 1)))

      (geda:assert-not-dirties P (apply set-object-stroke! l (object-stroke l)))
      (geda:assert-dirties P (apply set-object-fill! b (object-fill b)))

      ;; Remove primitives from page
      (geda:assert-dirties P (for-each (lambda (x) (geda:page-remove! P x))
                                       (list l b c a t)))

      ;; Add primitives to component
      (for-each (lambda (x) (geda:assert-dirties P (component-append! C x)))
                (list l b c a t))

      ;; Modify primitives within component
      (geda:assert-not-dirties P (apply set-line! l (line-info l)))
      (geda:assert-not-dirties P (apply set-box! b (box-info b)))
      (geda:assert-not-dirties P (apply set-circle! c (circle-info c)))
      (geda:assert-not-dirties P (apply set-arc! a (arc-info a)))
      (geda:assert-not-dirties P (apply set-text! t (text-info t)))

      (geda:assert-not-dirties P (apply set-object-stroke! l (object-stroke l)))
      (geda:assert-dirties P (apply set-object-fill! b (object-fill b)))

      ;; Remove primitives from component
      (for-each (lambda (x) (geda:assert-dirties P (component-remove! C x)))
                (list l b c a t)))

    (lambda ()
      (for-each (lambda (x) (geda:page-remove! P x)) (geda:page-contents P))
      (geda:close-page! P))))

(test-end "geda:page-dirty-objects")


(test-begin "geda:page-dirty-attribs")

(let ((P (geda:make-page "/test/page/A"))
      (p (make-net-pin '(0 . 0) '(100 . 0)))
      (t (make-text '(1 . 2) 'lower-left 0 "name=value" 10 #t 'both))
      (C (make-component "test component" '(1 . 2) 0 #t #f)))

  ;; Make sure pages are cleaned up
  (dynamic-wind
    (lambda () #f)
    (lambda ()
      ;; Populate page
      (geda:page-append! P t C) (component-append! C p)

      ;; Attach attribute to component
      (geda:assert-dirties P (attach-attribs! C t))
      ;; Detach attribute from component
      (geda:assert-dirties P (detach-attribs! C t))

      ;; Move attribute into component
      (geda:page-remove! P t)
      (component-append! C t)

      ;; Attach attribute to pin
      (geda:assert-dirties P (attach-attribs! p t))
      ;; Detach attribute from pin
      (geda:assert-dirties P (detach-attribs! p t))
      )
    (lambda () (geda:close-page! P))))

(test-end "geda:page-dirty-attribs")
