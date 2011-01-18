;;; gEDA - GPL Electronic Design Automation
;;; gnetlist back end for Osmond PCB Design
;;; Copyright (C) 2007-2010 John P. Doty
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

; Export a design to Osmond PCB

(define (osmond filename)
	(set-current-output-port (open-output-file filename))
	(for-each osmond:part packages)
	(for-each osmond:signal all-unique-nets))


; The first section of the file consists of a list of packages,
; one per line. For example:
; Part 0603 { Name R4 }

(define (osmond:part package)
	(format #t
		"Part ~A { Name ~A }\n"
		(gnetlist:get-package-attribute package "footprint")
		package))


; The next section of the file consists of a list of nets.
; Each entry takes two lines. For example:
; Signal "unnamed_net6"
;   { R4-1 R3-2 C3-2 }

(define (osmond:signal net)
	(format #t "Signal \"~A\"\n  {" net)
	(for-each osmond:pin (gnetlist:get-all-connections net))
	(format #t " }\n"))


; gnetlist represents a connection as a two-element list of the form:
; (refdes pinnumber)
; Convert to " refdes-pinnumber"

(define (osmond:pin connection)
	(format #t
		" ~A-~A"
		(car connection)	; refdes
		(cadr connection)))	; pin number
