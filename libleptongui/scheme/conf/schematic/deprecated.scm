(use-modules (ice-9 regex)
             (lepton attrib)
             (lepton autoplace)
             (lepton config)
             (lepton legacy-config)
             (lepton object)
             (lepton page)
             (lepton rc)
             (gschem deprecated)
             (schematic hook)
             (schematic window))

;
; Start of color section
;

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
;(use-modules (schematic refdes))
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

; Adds the default pin attributes to each newly placed pin.
(define (add-default-pin-attributes object)
  (for-each
    (lambda (a)
      (apply add-attribute-to-object object a)) default-pin-attributes))

; Comment in this hook to automatically add the default attributes to
; each newly placed pin
(add-hook! add-pin-hook add-default-pin-attributes)


; Autoplace pin text attributes hook.
; Comment in these if you want the pin attributes to be automatically placed.
; There are different hooks for situations like adding a new pin and rotating
; or mirroring an existing one.
; The #t at the end means that function is appended to the end of the hook.
(add-hook! add-pin-hook (lambda (pin)
        (autoplace-pin-attributes pin )) #t)
;(add-hook! rotate-pin-hook (lambda (pin)
;       (autoplace-pin-attributes pin )) #t)
;(add-hook! mirror-pin-hook (lambda (pin)
;       (autoplace-pin-attributes pin )) #t)

; Autoplace component/net/buses text attributes hook.
; Comment in these if you want the component attributes to be
; automatically placed.
; There are different hooks for situations like adding a new pin, rotating
; or mirroring an existing one, adding a new attribute or a new component.
; The #t at the end means that function is appended to the end of the hook.
;(add-hook! add-component-object-hook (lambda (object)
;       (autoplace-object-attributes object)) #t)
;(add-hook! rotate-component-object-hook (lambda (object)
;       (autoplace-object-attributes object)) #t)
;(add-hook! mirror-component-object-hook (lambda (object)
;       (autoplace-object-attributes object)) #t)
;(add-hook! add-attribute-hook (lambda (object)
;       (autoplace-object-attributes object)) #t)

;;; Change positions of attributes of a component selected for
;;; insertion into schematic according to some automatic rules
;;; defined in autoplace-object-attributes().  Please note that
;;; the hook calls the function for each object in the placement
;;; list which usually contains a component and its toplevel
;;; attributes.
;; (add-hook!
;;  complex-place-list-changed-hook
;;  (lambda (ls)
;;    (for-each autoplace-object-attributes ls))
;;  #t)

; Autoplace netname= attribute hook.  This autoplaces netname
; attribute at the time that it's added.
(add-hook! add-objects-hook place-netname-attribute-handler)



; Automatically place a titleblock when creating a new page.
;
( define ( add-titleblock page )

  ( define ( warn-deprecated )
    ( format (current-error-port)
      ( warning-option-deprecated
        "default-titleblock"
        "schematic.gui"
        "default-titleblock"
      )
    )
  )

  ( define ( read-cfg cfg )
    ( config-string cfg "schematic.gui" "default-titleblock" )
  )

  ( define ( add? symname )
    ( and
      ( not (string-null? symname) )
      ( null? (page-contents page) )
      ( not (string-match ".*\\.[sS][yY][mM]" (page-filename page)) )
    )
  )

  ( define ( add-to-page symname )
  ( let*
    (
    ( attrs '() )
    ( pos    ( cons 40000 40000 ) )
    ( comp   ( make-component/library symname pos 0 #f #t ) )
    )

    ( when comp
      ( page-append! page comp )
      ( set! attrs ( promote-attribs! comp ) )
      ( run-hook add-objects-hook (cons comp attrs) )
      ( set-page-dirty! page #f )
    )

  ) ; let
  ) ; add-to-page()


  ( let*
    (
    ( default "title-B.sym" )
    ( cfg ( path-config-context (getcwd) ) )
    ( symname ( false-if-exception (read-cfg cfg) ) )
    )

    ( when ( defined? 'default-titleblock )
      ( set! symname default-titleblock )
      ( warn-deprecated )
    )

    ( unless symname
      ( set! symname default )
    )

    ( if ( add? symname )
      ( add-to-page symname )
    )

  ) ; let

) ; add-titleblock()



( add-hook! new-page-hook add-titleblock )

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
; (use-modules (schematic pcb))
