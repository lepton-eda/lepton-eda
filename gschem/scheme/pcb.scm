;; -*- Scheme -*-
;;
;; $Id$
;;
;; Copyright (C) 2006 Dan McMahill
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;;


;; ** WARNING **
;; This file contains highly experimental code.  Use at your own risk.
;;
;; TODO
;;  Note:  This list is incomplete.
;;
;; - Find a way to configure path to pcb and/or a log file to tee the
;;   commands to (cat | tee /path/to/logfile | pcb --listen)
;;
;; - consolidate the code which checks to see if the pipe is open.  Maybe
;;   we should only open the pipe if the user asks for it via a menu
;;   choice and then in all the hooks, just return without doing anything
;;   if the pcb pipe is not open.
;;
;;   Menu items
;;
;;    =) tell the listening pcb to load the new netlist and also load the
;;       new elements to the paste buffer after running gsch2pcb
;;
;;    =) back annotate (once we implement it)
;;
;;    =) figure out how to have pcb talk back to gschem for selecting/deselecting
;;       elements.
;;
;;
;; - Add a gschem exit-hook which will gracefully shutdown pcb instead of just
;;   having pcb abort due to a terminated input pipe.  Maybe need to catch this
;;   in pcb too?
;;
;; - Have this module detect if the user exited from pcb which breaks the pipe

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
;; Add the hooks
;;
(add-hook! deselect-component-hook pcb-deselect-component-hook)
(add-hook! deselect-net-hook pcb-deselect-net-hook)
(add-hook! deselect-all-hook pcb-deselect-all-hook)
(add-hook! select-component-hook pcb-select-component-hook)
(add-hook! select-net-hook pcb-select-net-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Menus
;;
;;
;;
(define (pcb:about)
  (display "This is the pcb major mode for gschem\n")
  (display "pcb.scm version $Id$\n")
  (display "***** WARNING *****\n")
  (display "This is highly experimental\n")
  (display "You should save your work often\n")
  (display "and keep backup copies.  You have\n")
  (display "been warned.\n")
)

(define (pcb:launch-pcb)
  (set! pcb:pipe (open-output-pipe "pcb --listen"))
)

(define (pcb:run-gsch2pcb)
  (system "gsch2pcb")
)

; All keys in this keymap *must* be unique
(define pcb:pcb-keymap
  '(
    ("?" . pcb:about)
    ("l" . pcb:launch-pcb)
    ("n" . pcb:run-gsch2pcb)
    )
  )

(define pcb:menu-items 
;;
;;          menu item name      menu action     	menu hotkey action
;;
	'( ("About..."		pcb:about       	pcb:about)
	   ("Launch PCB"        pcb:launch-pcb          pcb:launch-pcb)
	   ("Run gsch2pcb"      pcb:run-gsch2pcb        pcb:run-gsch2pcb)
	   )
)

(add-menu "PCB" pcb:menu-items)


