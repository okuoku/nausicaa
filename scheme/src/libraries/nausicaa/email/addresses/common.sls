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
;;;Copyright (c) 2009, 2010, 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!r6rs
(library (nausicaa email addresses common)
  (export

    unquote-string

    ;; domain data type
    <domain>
    make-<domain>
    <domain>?			<domain>?/or-false
    assert-<domain>		assert-<domain>/or-false
    <domain>-subdomains		<domain>-literal?

    ;; local part data type
    <local-part>
    make-<local-part>
    <local-part>?
    <local-part>-subparts

    ;; addr-spec data type
    <addr-spec>
    make-<addr-spec>
    <addr-spec>?
    <addr-spec>-local-part
    <addr-spec>-domain

    ;; route data type
    <route>
    make-<route>
    <route>?
    <route>-domains

    ;; route address data type
    <mailbox>
    make-<mailbox>
    <mailbox>?
    <mailbox>-display-name
    <mailbox>-route
    <mailbox>-addr-spec

    ;; group data type
    <group>
    make-<group>
    <group>?
    <group>-display-name
    <group>-mailboxes)
  (import (nausicaa)
    (nausicaa generics)
    (nausicaa generics object-to-string)
    (nausicaa strings)
    (nausicaa lists))


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

(define-class <domain>
  (nongenerative nausicaa:email:addresses:common:<domain>)
  (fields (immutable literal?)		    ;boolean
	  (immutable (subdomains <list>)))) ;list of strings

(define (<domain>?/or-false obj)
  (or (not obj) (<domain>? obj)))

(define (assert-<domain> (D <domain>))
  (assert (is-a? D <domain>))
  (assert (boolean? D.literal?))
  (assert (list? D.subdomains))
  (assert (for-all string? D.subdomains)))

(define (assert-<domain>/or-false obj)
  (unless (not obj)
    (assert-<domain> obj)))

(define-method (object->string (o <domain>))
  (let ((str (string-join o.subdomains %dot-string)))
    (if o.literal?
	(string-append %open-bracket-string
		       str
		       %close-bracket-string)
      str)))


;;;; address local part record

(define-class <local-part>
  (nongenerative nausicaa:email:addresses:common:<local-part>)
  (fields (immutable (subparts <list>))))

(define-method (object->string (o <local-part>))
  (string-join o.subparts %dot-string))


;;;; address <addr-spec> record

(define-class <addr-spec>
  (nongenerative nausicaa:email:addresses:common:<addr-spec>)
  (fields (immutable (local-part <local-part>))
	  (immutable (domain <domain>))))

(define-method (object->string (o <addr-spec>))
  (string-append (object->string o.local-part)
		 %at-string
		 (object->string o.domain)))


;;;; route record

(define-class <route>
  (nongenerative nausicaa:email:addresses:common:<route>)
  (fields (immutable (domains <list>))))

(define-method (object->string (o <route>))
  (call-with-string-output-port
      (lambda (port)
	(define (%display thing)
	  (display thing port))
	(define (display-domain dom)
	  (unless (<domain>-literal? dom)
	    (%display %at-string))
	  (%display (object->string dom)))
	(unless (null? o.domains)
	  (display-domain (car o.domains))
	  (let loop ((domains (cdr o.domains)))
	    (unless (null? domains)
	      (let ((dom (car domains)))
		(%display %comma-string)
		(display-domain dom)
		(loop (cdr domains)))))))))


;;;; mailbox record

(define-class <mailbox>
  (nongenerative nausicaa:email:addresses:common:<mailbox>)
  (fields (immutable (display-name <string>))	;string or #f
	  (immutable (route <route>))		;route record or #f
	  (immutable (addr-spec <addr-spec>))))	;addr-spec record

(define-method (object->string (o <mailbox>))
  (string-append (if o.display-name
		     ;;Take care of quoting  the phrase if it contains
		     ;;a comma.
		     (string-append (display-name->string o.display-name) %space-string)
		   %empty-string)
		 %open-angle-string
		 (if o.route
		     (string-append (object->string o.route) %colon-string)
		   %empty-string)
		 (object->string o.addr-spec)
		 %close-angle-string))


;;;; group record

(define-class <group>
  (nongenerative nausicaa:email:addresses:common:<group>)
  (fields (immutable (display-name <string>))
	  (immutable (mailboxes <list>))))

(define-method (object->string (o <group>))
  (string-append (object->string o.display-name)
		 %colon-string %space-string
		 (string-join (map (lambda (mb)
				     (object->string mb)) o.mailboxes) ", ")
		 %semicolon-string))


;;;; done

)

;;; end of file
