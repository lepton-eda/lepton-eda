;; -*- Scheme -*-
;;
;; Copyright (C) 2006-2010 Dan McMahill
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
;; - complete a project file which is essentially the same as a gsch2pcb
;;   project file.
;;
;; - figure out how we should specify gsch2pcb arguments.  The project file?
;;
;; - figure out how to launch a gsch2pcb editor to modify the gsch2pcb project
;;   file.
;;
;; - figure out how to capture standard output from gsch2pcb and put it to 
;;   the gschem log.  Perhaps a gschem-system-and-log function?
;;
;; - tell the listening pcb to load the new netlist and also load the
;;   new elements to the paste buffer after running gsch2pcb
;;
;; - when launching pcb, tell it to load the correct file or maybe after after
;;   running gsch2pcb, tell pcb what to load.
;;
;; - back annotate (once we implement it)
;;
;; - figure out how to have pcb talk back to gschem for selecting/deselecting
;;   elements.
;;
;; - figure out how to install the hotkeys
;;

(use-modules (ice-9 popen))

(gschem-log "Loading the PCB major mode\n")
(gschem-log "The PCB major mode is incomplete and considered experimental at this time\n")

;; These may be changed by the user in their gafrc files (FIXME -- make this
;; preceeding comment be true)

;; Various executibles
(define pcb:pcb-cmd "pcb")
(define pcb:gsch2pcb-cmd "gsch2pcb")

;; FIXME
;; should probably look for a value defined in a gafrc, the EDITOR env variable,
;; and then some default here.
(define pcb:editor-cmd "emacs &")

;; In general, we should probably load gnetlist.scm and the appropriate
;; gnetlist backend to have the refdes aliasing code available.

(define pcb:pipe #f)

(define pcb:project-file-name "")

;; (close-pipe pcb:pipe)

;; Use this instead of
;; (display val pcb:pipe)
;;
(define (pcb:pipe-write val)
  (if pcb:pipe

      ;; pipe is open so try and write out our value
      (begin
	(catch #t 

	       ;; try to write our value
	       (lambda ()
		 (display val pcb:pipe) 
		 )

	       ;; and if we fail spit out a message
	       ;; and set pcb:pipe to false so we don't
	       ;; try and write again
	       (lambda (key . args)
		 (gschem-log "It appears that PCB has terminated.\n")
		 (gschem-log "If this is not the case, you should save and exit and\n")
		 (gschem-log "report this as a bug.\n\n")
		 (gschem-log "If you exited PCB on purpose, you can ignore \n")
		 (gschem-log "this message\n\n")
		 (set! pcb:pipe #f)
		 )

	       )
	)

      ;; pipe is not open so don't try and write to it
      ;;(display "pcb:pipe is not open\n")
      
      )
  
)

;;
;;
;; pcb-select-component-hook
;;

(define (pcb-select-component-hook attribs)
  
  (for-each 
     (lambda (attrib) 
       (let* ((name-value (get-attribute-name-value attrib))
              (name (car name-value))
              (value (cdr name-value))
             )
	     (if (string=? name "refdes")
		 (let ()
		  (pcb:pipe-write "Select(ElementByName, ^")
		  (pcb:pipe-write value) 
		  (pcb:pipe-write "$)\n")
		  )
		 )
	     )
       )
      attribs
      )
)

;;
;;
;; pcb-deselect-component-hook
;;

(define (pcb-deselect-component-hook attribs)
  (for-each 
     (lambda (attrib) 
       (let* ((name-value (get-attribute-name-value attrib))
              (name (car name-value))
              (value (cdr name-value))
             )
	     (if (string=? name "refdes")
		 (let ()
		  (pcb:pipe-write "Unselect(ElementByName, ^")
		  (pcb:pipe-write value)
		  (pcb:pipe-write "$)\n")
		  )
		 )
	     )
       )
      attribs
      )
)

;;
;;
;; pcb-select-net-hook
;;

(define (pcb-select-net-hook attribs)
  ;; Select net hook
  #t
)

;;
;;
;; pcb-deselect-net-hook
;;

(define (pcb-deselect-net-hook attribs)
  ;; Select net hook
  #t
)

;;
;;
;; pcb-deselect-all-hook
;;

(define (pcb-deselect-all-hook attribs)
  (pcb:pipe-write "Unselect(All)\n")
)

;;
;;
;; Add the hooks
;;
(add-hook! deselect-component-hook pcb-deselect-component-hook)
(add-hook! deselect-net-hook pcb-deselect-net-hook)
(add-hook! deselect-all-hook pcb-deselect-all-hook)
(add-hook! select-component-hook pcb-select-component-hook)
(add-hook! select-net-hook pcb-select-net-hook)


;;
;;
;; Menus
;;
;;
;;
(define (pcb:about)
  (gschem-msg (string-append
	       "This is the pcb major mode for gschem\n"
	       "***** WARNING *****\n"
	       "This is highly experimental\n"
	       "You should save your work often\n"
	       "and keep backup copies.  You have\n"
	       "been warned.\n"
	       )
	      )
  )

(define (pcb:launch-pcb)
  ;; We don't want to crash on a SIGPIPE if the user
  ;; exits from PCB
  (if pcb:pipe 
      (begin
	(gschem-log "PCB is already running\n")
	(gschem-msg "PCB is already running\n")
	)

      (begin
	(if (gschem-confirm "Start pcb?")
	    (begin
	      (sigaction SIGPIPE SIG_IGN)
	      (gschem-log "Launching PCB\n")
	      (set! pcb:pipe (open-output-pipe 
			      (string-append pcb:pcb-cmd " --listen")
			      )
		    )
	      (if (not pcb:pipe)
		  (gschem-log "Failed to launch PCB\n")
		  (gschem-log "Launched PCB\n")
		  )
	      )
	    (gschem-msg "Not launching PCB\n")
	    )
	)
      )
  )

(define (pcb:run-gsch2pcb)
  (gschem-log "Running gsch2pcb")
  (system pcb:gsch2pcb-cmd)
)

(define (pcb:run-editor)
  (system pcb:editor-cmd)
)

(define (pcb:load-project)
  (let ((f nil))
    (gschem-msg "This menu choice does not really do anything yet other than select a file\n")

    (set! f (gschem-filesel "Select Project File" pcb:project-file-name  'open 'must_exist))
    (if f (set! pcb:project-file-name f) )
  )
)

(define (pcb:save-project)
  (let ((f def nil))
    (gschem-msg "This menu choice does not really do anything yet other than select a file\n")

    (set! f (gschem-filesel "Save Project File As" pcb:project-file-name 'save 'may_exist))
    (if f (set! pcb:project-file-name f) )
  )
)

; All keys in this keymap *must* be unique
(define pcb:pcb-keymap
  '(
    ("?" . pcb:about)
    ("l" . pcb:launch-pcb)
    ("n" . pcb:run-gsch2pcb)
    ("e" . pcb:run-editor)
    ("o" . pcb:load-project)
    ("s" . pcb:save-project)
    )
  )

(define pcb:menu-items 
;;
;;          menu item name      menu action     	menu hotkey action
;;
	'( ("About..."		         pcb:about               pcb:about)
	   ("Launch PCB"                 pcb:launch-pcb          pcb:launch-pcb)
	   ("Run gsch2pcb"               pcb:run-gsch2pcb        pcb:run-gsch2pcb)
	   ("Edit gsch2pcb project"      pcb:run-editor          pcb:run-editor)
	   ("Load Project..."            pcb:load-project        pcb:load-project)
	   ("Save Project..."            pcb:save-project        pcb:save-project)
	   )
)

(add-menu "PCB" pcb:menu-items)


