;; gEDA - GPL Electronic Design Automation
;; gnetlist - gEDA Schematic Capture - Scheme API
;; Copyright (C) 2011 Peter Brett <peter@peter-b.co.uk>
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

;; This module provides an API to assist backends which wish to
;; provide command-line gnetlist options via the `-O' argument.  The
;; API consists of two functions:
;;
;;   `backend-getopt', which accepts a grammar and the set of `-O'
;;   arguments, and extracts the options.
;;
;;   `backend-option-ref', which is used to look-up an option in the
;;   structure returned by `backend-getopt'.

(define-module (gnetlist backend-getopt)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 receive)
  #:export (backend-getopt backend-option-ref))


(define (backend-getopt args grammar)
  "backend-getopt ARGS GRAMMAR

Parse the command-line `-O' arguments in ARGS against the given
backend option GRAMMAR, returning an option structure that can be
passed to backend-option-ref.

The GRAMMAR argument is expected to be a list of this form:

  `((OPTION (PROPERTY VALUE) ...) ...)'

where each OPTION is a symbol denoting the option name.

For each option, there may be a list of arbitrarily many
property/value pairs.  The order of the pairs is not important, but
every property may only appear once in the property list.  The
following table lists the possible properties:

  `(required? BOOL)'
        If BOOL is true, the option is required. `backend-getopt' will
        raise an error if it is not found in ARGS.

  `(value BOOL)'
        If BOOL is `#t', the option requires a value; if it is `#f',
        it does not; and if it is the symbol `optional', the option
        may appear in ARGS with or without a value.

  `(predicate FUNC)'
        If the option accepts a value (i.e. you specified `(value #t)'
        or `(value optional)' for this option), then `backend-getopt'
        will apply FUNC to the value, and raise an error if it returns
        `#f'. FUNC should be a procedure which accepts a string and
        returns a boolean value.

Normally, you will want to pass the result of calling
`gnetlist:get-backend-arguments' as the ARGS parameter.

If `backend-getopt' finds a problem with ARGS, it raises an error with
the key `option-error'."
  (let ((options '()))
    ;; First pass: process options
    (for-each
     (lambda (arg)
       (receive (name value)
           (split-arg arg)
         (set! options
               (assoc-set! options name (process-arg name value grammar)))))
     args)

    ;; Second pass: ensure required options have been provided
    (for-each
     (lambda (grammar-entry)
       (let ((name (car grammar-entry))
             (spec (cdr grammar-entry)))
         (and (opt-property spec 'required? #f)
              (or (backend-option-ref options name)
                  (option-error
                   "Backend option '~A' must be specified." name)))))
     grammar)

    ;; Return options
    options))


(define* (backend-option-ref options key #:optional default)
  "backend-option-ref OPTIONS KEY [DEFAULT]

Search OPTIONS for a backend option named KEY and return its value,
if found.  If the option has no value, but was given, return `#t'.  If
the option was not given, return DEFAULT, or if DEFAULT was not
specified, `#f'.  OPTIONS must be the result of a call to
`backend-getopt'."
  (or (assoc-ref options key) default))


(define (option-error message . args)
  "Raise an error due to a bad option."
  (scm-error 'option-error "backend-getopt" message args #f))


(define (split-arg arg)
  "Split an `-O' argument into name and value.  The name is assumed to
be all characters in ARG up to the first `=' or ` '.  The name is
returned as a symbol, and the value (if present) as a string.  If
argument has no value component, the value is returned as #f."
  (let ((idx (string-index arg (char-set #\space #\=))))
    (case idx
      ((0)  (option-error "Invalid backend option syntax '~A'." arg))
      ((#f) (values (string->symbol arg) #f))
      (else (values (string->symbol (substring arg 0 idx))
                    (substring arg (1+ idx)))))))


(define (opt-spec name grammar)
  "Search GRAMMAR for an option specification for NAME."
  (let ((s (find (lambda (x) (eqv? name (car x))) grammar)))
    (and s (cdr s)))) ; Throw away the name of the option.


(define (opt-property spec property default)
  "Search SPEC (obtained using find-opt-spec) for the given option
PROPERTY.  Returns the value of that property, or default if the
property wasn't present in SPEC."
  (let ((p (find (lambda (x) (eqv? property (car x))) spec)))
    (if p (cadr p) default))) ; Throw away the name of the property.
                              ; If property not found, use default.


(define (process-arg name value grammar)
  "Validates the given `-O' argument NAME & VALUE against GRAMMAR,
returning the value to be returned to the user code."
  (let ((spec (opt-spec name grammar)))
    ; Is this a valid argument?
    (or spec
        (option-error "Unrecognized backend option '~A'." name))

    ; Check that a value was provided, if one was required, or vice
    ; versa.
    (case (opt-property spec 'value #f)
      ((optional) #t)
      ((#f) (and value (option-error
                     "Backend option '~A' doesn't allow an argument." name)))
      (else (or value (option-error
                     "Backend option '~A' requires an argument." name))))

    ; If a value-verification predicate was provided, use it to verify
    ; the value.
    (let ((pred? (opt-property spec 'predicate #f)))
      (and pred? value
           (or (pred? value)
               (option-error
                "Invalid argument '~A' to backend option '~A'."
                value name))))

    ; If a value was provided, return it, otherwise #t.
    (or value #t)))
