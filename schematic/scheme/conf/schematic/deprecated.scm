(use-modules (ice-9 regex)
             (lepton page)
             (gschem hook)
             (gschem window))

;
; Start of color section
;

;; Make the gschem color maps more user-friendly
(color-map-make-friendly display-color-map)
(color-map-make-friendly display-outline-color-map)

; Load up a color scheme which has a dark (black) background.
; Comment out the first line and comment in the second line for a
; light (almost white) background.  The dark background is the
; original look.
;
(primitive-load (build-path geda-rc-path "gschem-colormap-darkbg")) ; dark background
;(load (build-path geda-rc-path "gschem-colormap-lightbg")) ; light background
;(load (build-path geda-rc-path "gschem-colormap-bw")) ; light background, bw

;
; End of color section
;




;
; Start of hooks
;

;;
;; Comment in this scheme code if you want automatic numbering when
;; placing new component and copying components.
;
;(load-from-path "auto-uref.scm")
;(add-hook! add-component-hook auto-uref)
;(add-hook! copy-component-hook auto-uref)
;
;; Define value of page-offset for auto number on insert.
;; Refdeses will be numbered from integer multiples of page-offset,
;; depending on the lowest refdes value found on the page.
;; If lowest value is 323 and page offset is 100, then next refdes
;; will be 301.
;; Setting to 0 disables the feature.
;
;(auto-uref-set-page-offset 100)


; Define default pin attributes
; Attributes:
;   - Attribute name.
;   - Value of the attribute.
;   - Visibility: #t (visible) or #f (hidden).
;   - Show_list:  a list containing what to show, using
;                 elements like "name" or "value", or an empty list.
(define default-pin-attributes
       '(("pintype"   "unknown" #f ())
	 ("pinlabel"  "unknown" #t ("value"))
	 ("pinnumber" "0"       #t ("value"))
	 ("pinseq"    "0"       #f ())))

; Convert a character into a string
(define char2str
  (lambda (char)
    (list->string (list char))))

; Attribute autoplacement grid
(define autoplace-attributes-grid 50)

; Load the default position of attributes, for attribute autoplacing
; functions.
(load-from-path "default-attrib-positions.scm")

; Adds the default pin attributes to each newly placed pin.
(define (add-default-pin-attributes object)
  (for-each
    (lambda (a)
      (apply add-attribute-to-object object a)) default-pin-attributes))

; Comment in this hook to automatically add the default attributes to
; each newly placed pin
(add-hook! add-pin-hook add-default-pin-attributes)


; Comment in this to load the functions to place the attributes automatically.
(load-from-path "auto-place-attribs.scm")

; Autoplace pin text attributes hook.
; Comment in these if you want the pin attributes to be automatically placed.
; There are different hooks for situations like adding a new pin and rotating
; or mirroring an existing one.
; The #t at the end means that function is appended to the end of the hook.
(add-hook! add-pin-hook (lambda (pin)
	(autoplace-pin-attributes pin )) #t)
;(add-hook! rotate-pin-hook (lambda (pin)
;	(autoplace-pin-attributes pin )) #t)
;(add-hook! mirror-pin-hook (lambda (pin)
;	(autoplace-pin-attributes pin )) #t)

; Autoplace component/net/buses text attributes hook.
; Comment in these if you want the component attributes to be
; automatically placed.
; There are different hooks for situations like adding a new pin, rotating
; or mirroring an existing one, adding a new attribute or a new component.
; The #t at the end means that function is appended to the end of the hook.
;(add-hook! add-component-object-hook (lambda (object)
;	(autoplace-object-attributes object)) #t)
;(add-hook! rotate-component-object-hook (lambda (object)
;	(autoplace-object-attributes object)) #t)
;(add-hook! mirror-component-object-hook (lambda (object)
;	(autoplace-object-attributes object)) #t)
;(add-hook! add-attribute-hook (lambda (object)
;	(autoplace-object-attributes object)) #t)
;(add-hook! complex-place-list-changed-hook (lambda (object)
;         (autoplace-object-attributes object)) #t)

; Autoplace netname= attribute hook.  This autoplaces netname
; attribute at the time that it's added.
(load-from-path "auto-place-netname.scm")
(add-hook! add-objects-hook place-netname-attribute-handler)

; Automatically place a titleblock (or other components) when creating
; a new page.
; Comment in these lines if you want gschem to automatically place a titleblock
; when you create a new _empty_ page.
; Users can customize the default titleblock by adding the following line
; (without the semi-colons at the beginning) to the gschemrc file:
;; (define default-titleblock "title-A4.sym")
;; Change "title-A4.sym" by the name of your preferred titleblock!
;
; If you don't want a titleblock to be added automatically, then add one of
; the following lines to your gschemrc file (without the semicolon).
; There are several ways, so just choose one:
;   (define default-titleblock "")
;   (define default-titleblock '())
;   (define default-titleblock #f)
;
(define default-titleblock "title-B.sym")

(add-hook! new-page-hook (lambda (page)
   ; Only place the titleblock if there are no objects in the page
   ; and the page filename ends in ".sym".
   (if (and (null? (get-objects-in-page page))
	    (not (string-match ".*\\.[sS][yY][mM]"
                               (get-page-filename page))))
;      Syntax             Symbol name        X   Y    angle selectable mirrored
       (add-component-at-xy page default-titleblock 40000 40000   0       #f       #f))

   ;; After adding titleblock, reset page to mark as unchanged.
   (set-page-dirty! page #f))
	   #t)

;
; End of hooks
;


;;
;; Major modes
;;

;; Comment in this scheme code if you want to link with pcb
;;
;; Please note that the code in pcb.scm is still highly experimental
;; and there are known (and easy) ways to crash pcb and/or gschem with this code.
;; The short answer is neither program likes a pipe to break.
;;
; (load-from-path "pcb.scm")

