;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: common procedures for address parsing
;;;Date: Mon Aug 24, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009 Marco Maggi <marcomaggi@gna.org>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


(library (email addresses common)
  (export

    unquote-string

    ;; domain data type
    make-<domain>
    <domain>?			<domain>?/or-false
    assert-<domain>		assert-<domain>/or-false
    <domain>-subdomains		<domain>-literal?

    ;; local part data type
    make-<local-part>
    <local-part>?
    <local-part>-subparts

    ;; addr-spec data type
    make-<addr-spec>
    <addr-spec>?
    <addr-spec>-local-part
    <addr-spec>-domain

    ;; route data type
    make-<route>
    <route>?
    <route>-domains

    ;; route address data type
    make-<mailbox>
    <mailbox>?
    <mailbox>-display-name
    <mailbox>-route
    <mailbox>-addr-spec

    ;; group data type
    make-<group>
    <group>?
    <group>-display-name
    <group>-mailboxes)
  (import (rnrs)
    (records)
    (strings)
    (lists))


;;;; helpers

(define %empty-string		"")
(define %at-string		"@")
(define %colon-string		":")
(define %comma-string		",")
(define %dot-string		".")
(define %semicolon-string	";")
(define %space-string		" ")
(define %open-angle-string	"<")
(define %close-angle-string	">")
(define %open-paren-string	"(")
(define %close-paren-string	")")
(define %open-bracket-string	"[")
(define %close-bracket-string	"]")
(define %double-quote-string	"\"")
(define %double-close-paren-string "))")


(define (unquote-string string)
  ;;Remove  the quoting  backslash characters  from STRING.   Return the
  ;;clean string.  See the tests in the test suite.
  ;;
  (let-values (((len)		(string-length string))
	       ((port getter)	(open-string-output-port)))
    (do ((i 0 (+ 1 i)))
	((= i len)
	 (getter))
      (let ((ch (string-ref string i))
	    (i1 (+ 1 i)))
	(when (and (char=? #\\ ch)
		   (not (= len i1)))
	  (set! i i1)
	  (set! ch (string-ref string i)))
	(put-char port ch)))))

(define (display-name->string display-name)
  ;;Return  a   string  representation  of   DISPLAY-NAME  suitable  for
  ;;composing the string representation of an address.
  ;;
  (if (string-any #\, display-name)
      (string-append %double-quote-string display-name %double-quote-string)
    display-name))


;;;; address domain record

(define-record-type <domain>
  (fields (immutable literal?)		;boolean
	  (immutable subdomains)))	;list of strings

(define (<domain>?/or-false obj)
  (or (not obj) (<domain>? obj)))

(define (assert-<domain> obj)
  (assert (<domain>? obj))
  (assert (boolean? (<domain>-literal? obj)))
  (let ((v (<domain>-subdomains obj)))
    (assert (list? v))
    (assert (for-all string? v))))

(define (assert-<domain>/or-false obj)
  (unless (not obj)
    (assert-<domain> obj)))


;;;; address local part record

(define-record-type <local-part>
  (fields (immutable subparts)))



;;;; address <addr-spec> record

(define-record-type <addr-spec>
  (fields (immutable local-part)
	  (immutable domain)))



;;;; route record

(define-record-type <route>
  (fields (immutable domains)))



;;;; mailbox record

(define-record-type <mailbox>
  (fields (immutable display-name) ;string or #f
	  (immutable route)	   ;route record or #f
	  (immutable addr-spec)))  ;addr-spec record



;;;; group record

(define-record-type <group>
  (fields (immutable display-name)
	  (immutable mailboxes)))



;;;; NOS stuff

(declare-method (object->string (o <domain>))
  (let ((str (string-join (<domain>-subdomains o) %dot-string)))
    (if (<domain>-literal? o)
	(string-append %open-bracket-string
		       str
		       %close-bracket-string)
      str)))

(declare-method (object->string (o <local-part>))
  (string-join (<local-part>-subparts o) %dot-string))

(declare-method (object->string (o <addr-spec>))
  (string-append (object->string (<addr-spec>-local-part o))
		 %at-string
		 (object->string (<addr-spec>-domain o))))

(declare-method (object->string (o <route>))
  (call-with-string-output-port
      (lambda (port)
	(define (%display thing)
	  (display thing port))
	(define (display-domain dom)
	  (unless (<domain>-literal? dom)
	    (%display %at-string))
	  (%display (object->string dom)))
	(let ((domains (<route>-domains o)))
	  (unless (null? domains)
	    (display-domain (car domains))
	    (let loop ((domains (cdr domains)))
	      (unless (null? domains)
		(let ((dom (car domains)))
		  (%display %comma-string)
		  (display-domain dom)
		  (loop (cdr domains))))))))))

(declare-method (object->string (o <mailbox>))
  (let ((display-name (<mailbox>-display-name o)))
    (string-append (if display-name
		       ;;Take care of quoting  the phrase if it contains
		       ;;a comma.
		       (string-append (display-name->string display-name) %space-string)
		     %empty-string)
		   %open-angle-string
		   (let ((route (<mailbox>-route o)))
		     (if route
			 (string-append (object->string route) %colon-string)
		       %empty-string))
		   (object->string (<mailbox>-addr-spec o))
		   %close-angle-string)))

(declare-method (object->string (o <group>))
  (string-append (object->string (<group>-display-name o))
		 %colon-string %space-string
		 (string-join (map object->string (<group>-mailboxes o)) ", ")
		 %semicolon-string))


;;;; done

)

;;; end of file
