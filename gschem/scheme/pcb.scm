;; -*- Scheme -*-
;;
;; $Id$
;;
;;

;; ** WARNING **
;; This file contains highly experimental code.  Use at your own risk.
;;
;; TODO
;;  Note:  This list is incomplete.
;;
;; - Instead of being lame and writing our output to a fixed file name,
;;   we should actualy open pcb with popen and talk to it directly.
;;
;; - Find a way to configure path to pcb and/or a log file to tee the
;;   commands to (cat | tee /path/to/logfile | pcb --listen)
;;
;; - consolidate the code which checks to see if the pipe is open.  Maybe
;;   we should only open the pipe if the user asks for it via a menu
;;   choice and then in all the hooks, just return without doing anything
;;   if the pcb pipe is not open.
;;
;; - figure out how to add real menu choices (including the callback functions)
;;   in scheme only so we can isolate everything related to pcb into this
;;   one file and not have to touch the core of gschem in c code.  Menu items
;;   needed include:
;;
;;    =) launch pcb (do this with a two-way pipe so we can talk to it)
;;    =) run gsch2pcb (and maybe tell the listening pcb to load the new
;;       netlist and also load the new elements to the paste buffer).
;;    =) back annotate (once we implement it)
;;
;;    =) figure out how to have pcb talk back to gschem for selecting/deselecting
;;       elements.
;;


(use-modules (ice-9 popen))

;; In general, we should probably load gnetlist.scm and the appropriate
;; gnetlist backend to have the refdes aliasing code available.

(define pcb:pipe #f)

;; (close-pipe pcb:pipe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; pcb-select-component-hook
;;

(define (pcb-select-component-hook attribs)

  (if (not pcb:pipe)
      (set! pcb:pipe (open-output-pipe "/bin/cat > /tmp/pcb-hook.log"))
      )

  (for-each 
     (lambda (attrib) 
       (let* ((name-value (get-attribute-name-value attrib))
              (name (car name-value))
              (value (cdr name-value))
             )
	     (if (string=? name "refdes")
		 (let ()
		  (display "Select(ElementByName, ^" pcb:pipe) 
		  (display value pcb:pipe) 
		  (display "$)\n" pcb:pipe)
		  )
		 )
	     )
       )
      attribs
      )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; pcb-deselect-component-hook
;;

(define (pcb-deselect-component-hook attribs)

  (if (not pcb:pipe)
      (set! pcb:pipe (open-output-pipe "/bin/cat > /tmp/pcb-hook.log"))
      )

  (for-each 
     (lambda (attrib) 
       (let* ((name-value (get-attribute-name-value attrib))
              (name (car name-value))
              (value (cdr name-value))
             )
	     (if (string=? name "refdes")
		 (let ()
		  (display "Unselect(ElementByName, ^" pcb:pipe) 
		  (display value pcb:pipe) 
		  (display "$)\n" pcb:pipe)
		  )
		 )
	     )
       )
      attribs
      )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; pcb-select-net-hook
;;

(define (pcb-select-net-hook attribs)
  ;; Select net hook
  #t
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; pcb-deselect-net-hook
;;

(define (pcb-deselect-net-hook attribs)
  ;; Select net hook
  #t
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; pcb-deselect-all-hook
;;

(define (pcb-deselect-all-hook attribs)
  (if (not pcb:pipe)
      (set! pcb:pipe (open-output-pipe "/bin/cat > /tmp/pcb-hook.log"))
      )
  ;; Select net hook
  (display "Unselect(All)\n" pcb:pipe)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Menus
;;
;;
;;
(define pcb:menu-items 
;;
;;          menu item name      menu action     	menu hotkey action
;;
	'( ("About..."		help-about      	help-about)
	   ("Manual..."         help-manual             help-manual)
	   ("Hotkeys..."        help-hotkeys            help-hotkeys)
	   ("Component..."      hierarchy-documentation
						    hierarchy-documentation)))

;;(add-menu "PCB" pcb:menu-items)

