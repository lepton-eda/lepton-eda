;; gEDA - GPL Electronic Design Automation
;; gschem - gEDA Schematic Capture - Scheme API
;; Copyright (C) 2010-2011 Peter Brett
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
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA
;;

(define-module (gschem hook)

  ;; Import C definitions
  #:use-module (gschem core hook))

;; This module defines a number of hooks that can be used to run
;; arbitrary Scheme code following a variety of user actions.  Note
;; that hook functions should not normally modify their arguments.

;; add-objects-hook
;;
;; Called after objects are added to the page, at their initial
;; creation.  Argument is a list of the objects being added.
(define-public add-objects-hook %add-objects-hook)

;; remove-objects-hook
;;
;; Called after objects are removed from the page.  Argument is a list
;; of the objects being removed.
(define-public remove-objects-hook %remove-objects-hook)

;; move-objects-hook
;;
;; Called after objects are moved.  Argument is a list of the objects
;; that were mirrored.
(define-public move-objects-hook %move-objects-hook)

;; mirror-objects-hook
;;
;; Called after objects are mirrored.  Argument is a list of the
;; objects that were mirrored.
(define-public mirror-objects-hook %mirror-objects-hook)

;; rotate-objects-hook
;;
;; Called after objects are rotated.  Argument is a list of the
;; objects that were rotated.
(define-public rotate-objects-hook %rotate-objects-hook)

;; paste-objects-hook
;;
;; Called after objects are pasted to the page, either via "Edit->Copy
;; Mode" or similar, or via buffers, or via the clipboard.  Argument
;; is a list of the objects that were pasted.
(define-public paste-objects-hook %paste-objects-hook)

;; attach-attribs-hook
;;
;; Called after attributes are attached to something.  The argument is
;; a list of the attributes that were attached.
(define-public attach-attribs-hook %attach-attribs-hook)

;; detach-attribs-hook
;;
;; Called after attributes are detached from something.  The argument
;; is a list of the attributes that were detached.
(define-public detach-attribs-hook %detach-attribs-hook)

;; select-objects-hook
;;
;; Called after objects are added to the selection.  The argument is a
;; list of objects that were selected.
(define-public select-objects-hook %select-objects-hook)

;; deselect-objects-hook
;;
;; Called after objects are removed from the selection.  The argument
;; is a list of objects that were deselected.
(define-public deselect-objects-hook %deselect-objects-hook)

;; new-page-hook
;;
;; Called when a new page is created.  The argument is the new page.
(define-public new-page-hook %new-page-hook)
