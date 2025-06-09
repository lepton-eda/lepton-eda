;;; Test Scheme procedures related to pages' changed flags.

(use-modules (lepton attrib)
             (lepton object)
             (lepton page))

;;; Utility macro to avoid boilerplate
(define-syntax assert-dirties
  (syntax-rules ()
    ((_ P . test-forms)
     (begin (begin . test-forms)
            (test-assert (page-dirty? P))
            (set-page-dirty! P #f)))))

(define-syntax assert-not-dirties
  (syntax-rules ()
    ((_ P . test-forms)
     (begin (begin . test-forms)
            (test-assert (not (page-dirty? P)))))))

(test-begin "page-dirty")

(let ((P (make-page "/test/page/A"))
      (C (make-component "test component" '(1 . 2) 0 #t #f)))

  ;; Make sure pages are cleaned up
  (dynamic-wind
    (lambda () #f)
    (lambda ()
      (test-assert (not (page-dirty? P)))

      (set-page-dirty! P)
      (test-assert (page-dirty? P))

      (set-page-dirty! P #f)
      (test-assert (not (page-dirty? P)))

      (assert-dirties P (set-page-dirty! P #t))
      (assert-dirties P (page-append! P C))
      (assert-dirties P (page-remove! P C)))
    (lambda () (close-page! P))))

(test-end "page-dirty")


(define image
  (map char->integer (string->list
"/* XPM */
static char * image_xpm[] = {
\"2 1 1 1\",
\"      c None\",
\"  \"};
")))

(test-begin "page-dirty-objects")

(let ((P (make-page "/test/page/A"))
      (l (make-line '(1 . 2) '(3 . 4)))
      (b (make-box '(1 . 4) '(3 . 2)))
      (c (make-circle '(1 . 2) 3))
      (a (make-arc '(1 . 2) 3 45 90))
      (t (make-text '(1 . 2) 'lower-left 0 "test text" 10 #t 'both))
      (pic (make-picture/vector image "image.xpm" '(1 . 2) '(5 . 4) 0 #f))
      (C (make-component "test component" '(1 . 2) 0 #t #f)))

  ;; Make sure pages are cleaned up
  (dynamic-wind
    (lambda () #f)
    (lambda ()

      ;; Add everything to the page
      (assert-dirties P (for-each (lambda (x) (page-append! P x))
                                  (list l b c a t pic C)))
      ;; Arc.
      ;; The same parameters do not modify the page.
      (assert-not-dirties P (apply set-arc! a (arc-info a)))
      ;; Set color explicitly to facilitate next tests.
      (set-arc! a '(1 . 2) 3 45 90 3)
      ;; Reset dirty flag if it was set.
      (set-page-dirty! P #f)
      ;; Change center-x.
      (assert-dirties P (apply set-arc! a '((2 . 2) 3 45 90 3)))
      ;; Change center-y.
      (assert-dirties P (apply set-arc! a '((2 . 3) 3 45 90 3)))
      ;; Change radius.
      (assert-dirties P (apply set-arc! a '((2 . 3) 4 45 90 3)))
      ;; Change start-angle.
      (assert-dirties P (apply set-arc! a '((2 . 3) 4 90 90 3)))
      ;; Change sweep-angle.
      (assert-dirties P (apply set-arc! a '((2 . 3) 4 90 45 3)))
      ;; Change color.
      (assert-dirties P (apply set-arc! a '((2 . 3) 4 90 45 4)))

      ;; Box.
      ;; The same parameters do not modify the page.
      (assert-not-dirties P (apply set-box! b (box-info b)))
      ;; Set color explicitly to facilitate next tests.
      (set-box! b '(1 . 2) '(3 . 4) 3)
      ;; Reset dirty flag if it was set.
      (set-page-dirty! P #f)
      ;; Change x of the first corner.
      (assert-dirties P (apply set-box! b '((2 . 2) (3 . 4) 3)))
      ;; Change y of the first corner.
      (assert-dirties P (apply set-box! b '((2 . 3) (3 . 4) 3)))
      ;; Change x of the second corner.
      (assert-dirties P (apply set-box! b '((2 . 3) (4 . 4) 3)))
      ;; Change y of the second corner.
      (assert-dirties P (apply set-box! b '((2 . 3) (4 . 5) 3)))
      ;; Change color.
      (assert-dirties P (apply set-box! b '((2 . 3) (4 . 5) 4)))

      ;; Circle.
      ;; The same parameters do not modify the page.
      (assert-not-dirties P (apply set-circle! c (circle-info c)))
      ;; Set color explicitly to facilitate next tests.
      (set-circle! c '(1 . 2) 10 3)
      ;; Change center x.
      (assert-dirties P (apply set-circle! c '((2 . 2) 10 3)))
      ;; Change center y.
      (assert-dirties P (apply set-circle! c '((2 . 3) 10 3)))
      ;; Change radius.
      (assert-dirties P (apply set-circle! c '((2 . 3) 11 3)))
      ;; Change color.
      (assert-dirties P (apply set-circle! c '((2 . 3) 11 4)))

      ;; Line.
      ;; The same parameters do not modify the page.
      (assert-not-dirties P (apply set-line! l (line-info l)))
      ;; Set color explicitly to facilitate next tests.
      (set-line! l '(1 . 2) '(3 . 4) 3)
      ;; Reset dirty flag if it was set.
      (set-page-dirty! P #f)
      ;; Change x of the first corner.
      (assert-dirties P (apply set-line! l '((2 . 2) (3 . 4) 3)))
      ;; Change y of the first corner.
      (assert-dirties P (apply set-line! l '((2 . 3) (3 . 4) 3)))
      ;; Change x of the second corner.
      (assert-dirties P (apply set-line! l '((2 . 3) (4 . 4) 3)))
      ;; Change y of the second corner.
      (assert-dirties P (apply set-line! l '((2 . 3) (4 . 5) 3)))
      ;; Change color.
      (assert-dirties P (apply set-line! l '((2 . 3) (4 . 5) 4)))

      ;; Text.
      ;; The same parameters do not modify the page.
      (assert-not-dirties P (apply set-text! t (text-info t)))
      ;; Set color explicitly to facilitate next tests.
      (set-text! t '(1 . 2) 'lower-left 0 "name=value" 10 #t 'both 21)
      ;; Change anchor x.
      (assert-dirties P (apply set-text! t '((2 . 2) lower-left 0 "name=value" 10 #t both 21)))
      ;; Change anchor y.
      (assert-dirties P (apply set-text! t '((2 . 3) lower-left 0 "name=value" 10 #t both 21)))
      ;; Change alignment.
      (assert-dirties P (apply set-text! t '((2 . 3) upper-right 0 "name=value" 10 #t both 21)))
      ;; Change angle.
      (assert-dirties P (apply set-text! t '((2 . 3) upper-right 90 "name=value" 10 #t both 21)))
      ;; Change string.
      (assert-dirties P (apply set-text! t '((2 . 3) upper-right 90 "value=name" 10 #t both 21)))
      ;; Change size.
      (assert-dirties P (apply set-text! t '((2 . 3) upper-right 90 "value=name" 20 #t both 21)))
      ;; Change visibility.
      (assert-dirties P (apply set-text! t '((2 . 3) upper-right 90 "value=name" 20 #f both 21)))
      ;; Change show attribute mode.
      (assert-dirties P (apply set-text! t '((2 . 3) upper-right 90 "value=name" 20 #f name 21)))
      ;; Change color.
      (assert-dirties P (apply set-text! t '((2 . 3) upper-right 90 "value=name" 20 #f name 3)))
      ;; Text setters.
      (assert-not-dirties P (set-text-string! t (text-string t)))
      (assert-dirties P (set-text-string! t "other-string"))
      (assert-not-dirties P (set-text-visibility! t (text-visible? t)))
      (assert-dirties P (set-text-visibility! t (not (text-visible? t))))


      ;; Picture.
      ;; The same parameters do not modify the page.
      (assert-not-dirties P (apply set-picture! pic (cdr (picture-info pic))))
      ;; Set initial values to amend later.
      (set-picture! pic '(0 . 10) '(10 . 0) 0 #f)
      ;; Reset dirty flag if it was set.
      (set-page-dirty! P #f)
      ;; Change x of the first corner.
      (assert-dirties P (apply set-picture! pic '((1 . 10) (10 . 0) 0 #f)))
      ;; Change y of the first corner.
      (assert-dirties P (apply set-picture! pic '((1 . 11) (10 . 0) 0 #f)))
      ;; Change x of the second corner.
      (assert-dirties P (apply set-picture! pic '((1 . 11) (11 . 0) 0 #f)))
      ;; Change y of the second corner.
      (assert-dirties P (apply set-picture! pic '((1 . 11) (11 . 1) 0 #f)))
      ;; Change angle.
      (assert-dirties P (apply set-picture! pic '((1 . 11) (11 . 1) 90 #f)))
      ;; Change mirror flag.
      (assert-dirties P (apply set-picture! pic '((1 . 11) (11 . 1) 90 #t)))

      (assert-dirties P (apply set-component! C
                               (list-tail (component-info C) 1)))

      ;; The same stroke parameters do not modify the page.
      (assert-not-dirties P (apply set-object-stroke! l (object-stroke l)))

      (set-object-stroke! l 1 'none 'solid)
      ;; Reset dirty flag if it was set.
      (set-page-dirty! P #f)
      ;; Change stroke width.
      (assert-dirties P (set-object-stroke! l 2 'none 'solid))
      ;; Change cap style.
      (assert-dirties P (set-object-stroke! l 2 'round 'solid))
      ;; Change dash style.
      (assert-dirties P (set-object-stroke! l 2 'round 'dotted 1))
      ;; Change space between dots/dashes.
      (assert-dirties P (set-object-stroke! l 2 'round 'dotted 2))
      ;; Change dash style once again to check other parameters.
      (assert-dirties P (set-object-stroke! l 2 'round 'dashed 1 1))
      ;; Change dash length.
      (assert-dirties P (set-object-stroke! l 2 'round 'dashed 1 2))

      (assert-dirties P (apply set-object-fill! b (object-fill b)))

      ;; Remove primitives from page
      (assert-dirties P (for-each (lambda (x) (page-remove! P x))
                                  (list l b c a t pic)))

      ;; Add primitives to component
      (for-each (lambda (x) (assert-dirties P (component-append! C x)))
                (list l b c a t pic))

      ;; Modify primitives within component

      ;; Arc.
      ;; The same parameters do not modify the page.
      (assert-not-dirties P (apply set-arc! a (arc-info a)))
      ;; Set color explicitly to facilitate next tests.
      (set-arc! a '(1 . 2) 3 45 90 3)
      ;; Reset dirty flag if it was set.
      (set-page-dirty! P #f)
      ;; Change center-x.
      (assert-dirties P (apply set-arc! a '((2 . 2) 3 45 90 3)))
      ;; Change center-y.
      (assert-dirties P (apply set-arc! a '((2 . 3) 3 45 90 3)))
      ;; Change radius.
      (assert-dirties P (apply set-arc! a '((2 . 3) 4 45 90 3)))
      ;; Change start-angle.
      (assert-dirties P (apply set-arc! a '((2 . 3) 4 90 90 3)))
      ;; Change sweep-angle.
      (assert-dirties P (apply set-arc! a '((2 . 3) 4 90 45 3)))
      ;; Change color.
      (assert-dirties P (apply set-arc! a '((2 . 3) 4 90 45 4)))

      ;; Box.
      ;; The same parameters do not modify the page.
      (assert-not-dirties P (apply set-box! b (box-info b)))
      ;; Set color explicitly to facilitate next tests.
      (set-box! b '(1 . 2) '(3 . 4) 3)
      ;; Reset dirty flag if it was set.
      (set-page-dirty! P #f)
      ;; Change x of the first corner.
      (assert-dirties P (apply set-box! b '((2 . 2) (3 . 4) 3)))
      ;; Change y of the first corner.
      (assert-dirties P (apply set-box! b '((2 . 3) (3 . 4) 3)))
      ;; Change x of the second corner.
      (assert-dirties P (apply set-box! b '((2 . 3) (4 . 4) 3)))
      ;; Change y of the second corner.
      (assert-dirties P (apply set-box! b '((2 . 3) (4 . 5) 3)))
      ;; Change color.
      (assert-dirties P (apply set-box! b '((2 . 3) (4 . 5) 4)))

      ;; Circle.
      ;; The same parameters do not modify the page.
      (assert-not-dirties P (apply set-circle! c (circle-info c)))
      ;; Set color explicitly to facilitate next tests.
      (set-circle! c '(1 . 2) 10 3)
      ;; Reset dirty flag if it was set.
      (set-page-dirty! P #f)
      ;; Change center x.
      (assert-dirties P (apply set-circle! c '((2 . 2) 10 3)))
      ;; Change center y.
      (assert-dirties P (apply set-circle! c '((2 . 3) 10 3)))
      ;; Change radius.
      (assert-dirties P (apply set-circle! c '((2 . 3) 11 3)))
      ;; Change color.
      (assert-dirties P (apply set-circle! c '((2 . 3) 11 4)))

      ;; Line.
      ;; The same parameters do not modify the page.
      (assert-not-dirties P (apply set-line! l (line-info l)))
      ;; Set color explicitly to facilitate next tests.
      (set-line! l '(1 . 2) '(3 . 4) 3)
      ;; Reset dirty flag if it was set.
      (set-page-dirty! P #f)
      ;; Change x of the first corner.
      (assert-dirties P (apply set-line! l '((2 . 2) (3 . 4) 3)))
      ;; Change y of the first corner.
      (assert-dirties P (apply set-line! l '((2 . 3) (3 . 4) 3)))
      ;; Change x of the second corner.
      (assert-dirties P (apply set-line! l '((2 . 3) (4 . 4) 3)))
      ;; Change y of the second corner.
      (assert-dirties P (apply set-line! l '((2 . 3) (4 . 5) 3)))
      ;; Change color.
      (assert-dirties P (apply set-line! l '((2 . 3) (4 . 5) 4)))

      ;; Text.
      ;; The same parameters do not modify the page.
      (assert-not-dirties P (apply set-text! t (text-info t)))
      ;; Set color explicitly to facilitate next tests.
      (set-text! t '(1 . 2) 'lower-left 0 "name=value" 10 #t 'both 21)
      ;; Change anchor x.
      (assert-dirties P (apply set-text! t '((2 . 2) lower-left 0 "name=value" 10 #t both 21)))
      ;; Change anchor y.
      (assert-dirties P (apply set-text! t '((2 . 3) lower-left 0 "name=value" 10 #t both 21)))
      ;; Change alignment.
      (assert-dirties P (apply set-text! t '((2 . 3) upper-right 0 "name=value" 10 #t both 21)))
      ;; Change angle.
      (assert-dirties P (apply set-text! t '((2 . 3) upper-right 90 "name=value" 10 #t both 21)))
      ;; Change string.
      (assert-dirties P (apply set-text! t '((2 . 3) upper-right 90 "value=name" 10 #t both 21)))
      ;; Change size.
      (assert-dirties P (apply set-text! t '((2 . 3) upper-right 90 "value=name" 20 #t both 21)))
      ;; Change visibility.
      (assert-dirties P (apply set-text! t '((2 . 3) upper-right 90 "value=name" 20 #f both 21)))
      ;; Change show attribute mode.
      (assert-dirties P (apply set-text! t '((2 . 3) upper-right 90 "value=name" 20 #f name 21)))
      ;; Change color.
      (assert-dirties P (apply set-text! t '((2 . 3) upper-right 90 "value=name" 20 #f name 3)))
      ;; Text setters.
      (assert-not-dirties P (set-text-string! t (text-string t)))
      (assert-dirties P (set-text-string! t "other-string"))
      (assert-not-dirties P (set-text-visibility! t (text-visible? t)))
      (assert-dirties P (set-text-visibility! t (not (text-visible? t))))


      ;; Picture.
      ;; The same parameters do not modify the page.
      (assert-not-dirties P (apply set-picture! pic (cdr (picture-info pic))))
      ;; Set initial values to amend later.
      (set-picture! pic '(0 . 10) '(10 . 0) 0 #f)
      ;; Reset dirty flag if it was set.
      (set-page-dirty! P #f)
      ;; Change x of the first corner.
      (assert-dirties P (apply set-picture! pic '((1 . 10) (10 . 0) 0 #f)))
      ;; Change y of the first corner.
      (assert-dirties P (apply set-picture! pic '((1 . 11) (10 . 0) 0 #f)))
      ;; Change x of the second corner.
      (assert-dirties P (apply set-picture! pic '((1 . 11) (11 . 0) 0 #f)))
      ;; Change y of the second corner.
      (assert-dirties P (apply set-picture! pic '((1 . 11) (11 . 1) 0 #f)))
      ;; Change angle.
      (assert-dirties P (apply set-picture! pic '((1 . 11) (11 . 1) 90 #f)))
      ;; Change mirror flag.
      (assert-dirties P (apply set-picture! pic '((1 . 11) (11 . 1) 90 #t)))

      ;; The same stroke parameters do not modify the page.
      (assert-not-dirties P (apply set-object-stroke! l (object-stroke l)))

      (set-object-stroke! l 1 'none 'solid)
      ;; Reset dirty flag if it was set.
      (set-page-dirty! P #f)
      ;; Change stroke width.
      (assert-dirties P (set-object-stroke! l 2 'none 'solid))
      ;; Change cap style.
      (assert-dirties P (set-object-stroke! l 2 'round 'solid))
      ;; Change dash style.
      (assert-dirties P (set-object-stroke! l 2 'round 'dotted 1))
      ;; Change space between dots/dashes.
      (assert-dirties P (set-object-stroke! l 2 'round 'dotted 2))
      ;; Change dash style once again to check other parameters.
      (assert-dirties P (set-object-stroke! l 2 'round 'dashed 1 1))
      ;; Change dash length.
      (assert-dirties P (set-object-stroke! l 2 'round 'dashed 1 2))

      (assert-dirties P (apply set-object-fill! b (object-fill b)))

      ;; Remove primitives from component
      (for-each (lambda (x) (assert-dirties P (component-remove! C x)))
                (list l b c a t pic)))

    (lambda ()
      (for-each (lambda (x) (page-remove! P x)) (page-contents P))
      (close-page! P))))

(test-end "page-dirty-objects")


(test-begin "page-dirty-attribs")

(let ((P (make-page "/test/page/A"))
      (p (make-net-pin '(0 . 0) '(100 . 0)))
      (t (make-text '(1 . 2) 'lower-left 0 "name=value" 10 #t 'both))
      (C (make-component "test component" '(1 . 2) 0 #t #f)))

  ;; Make sure pages are cleaned up
  (dynamic-wind
    (lambda () #f)
    (lambda ()
      ;; Populate page
      (page-append! P t C) (component-append! C p)

      ;; Attach attribute to component
      (assert-dirties P (attach-attribs! C t))
      ;; Detach attribute from component
      (assert-dirties P (detach-attribs! C t))

      ;; Move attribute into component
      (page-remove! P t)
      (component-append! C t)

      ;; Attach attribute to pin
      (assert-dirties P (attach-attribs! p t))
      ;; Detach attribute from pin
      (assert-dirties P (detach-attribs! p t))
      )
    (lambda () (close-page! P))))

(test-end "page-dirty-attribs")
