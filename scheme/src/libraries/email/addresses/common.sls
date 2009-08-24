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
    make-domain			domain?
    domain-subdomains		domain-literal?
    domain-display		domain-write		domain->string

    ;; local part data type
    make-local-part		local-part?
    local-part-subparts
    local-part-display		local-part-write	local-part->string

    ;; addr-spec data type
    make-addr-spec		addr-spec?
    addr-spec-local-part	addr-spec-domain
    addr-spec-display		addr-spec-write		addr-spec->string

    ;; route data type
    make-route			route?			route-domains
    route-display		route-write		route->string

    ;; route address data type
    make-mailbox		mailbox?
    mailbox-display-name	mailbox-route		mailbox-addr-spec
    mailbox-display		mailbox-write		mailbox->string

    ;; group data type
    make-group			group?
    group-display-name		group-mailboxes
    group-display		group-write		group->string
    )
  (import (rnrs)
    (strings))


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


;;;; address domain record

(define-record-type domain
  (fields (immutable literal?)
	  (immutable subdomains)))

(define domain-display
  (case-lambda
   ((self)
    (domain-display self (current-output-port)))
   ((self port)
    (display (string-append "#<domain -- " (domain->string self) %close-angle-string)
	     port))))

(define domain-write
  (case-lambda
   ((self)
    (domain-display self (current-output-port)))
   ((self port)
    (display (if (domain-literal? self)
		 "(make-domain #t (quote "
	       "(make-domain #f (quote ") port)
    (write (domain-subdomains self) port)
    (display %double-close-paren-string port))))

(define (domain->string self)
  (let ((str (string-join (domain-subdomains self) %dot-string)))
    (if (domain-literal? self)
	(string-append %open-bracket-string
		       str
		       %close-bracket-string)
      str)))


;;;; address local part record

(define-record-type local-part
  (fields (immutable subparts)))

(define local-part-display
  (case-lambda
   ((self)
    (local-part-display self (current-output-port)))
   ((self port)
    (display (string-append "#<local-part -- "
			    (string-join (local-part-subparts self) %dot-string)
			    %close-angle-string)
	     port))))

(define local-part-write
  (case-lambda
   ((self)
    (local-part-display self (current-output-port)))
   ((self port)
    (display "(make-local-part (quote " port)
    (write (local-part-subparts self) port)
    (display %double-close-paren-string port))))

(define (local-part->string self)
  (string-join (local-part-subparts self) %dot-string))


;;;; address addr-spec record

(define-record-type addr-spec
  (fields (immutable local-part)
	  (immutable domain)))

(define addr-spec-display
  (case-lambda
   ((addr-spec)
    (addr-spec-display addr-spec (current-output-port)))
   ((addr-spec port)
    (display (string-append "#<addr-spec -- "
			    (addr-spec->string addr-spec)
			    %close-angle-string)
	     port))))

(define addr-spec-write
  (case-lambda
   ((addr-spec)
    (addr-spec-display addr-spec (current-output-port)))
   ((addr-spec port)
    (display "(make-addr-spec " port)
    (local-part-write (addr-spec-local-part addr-spec) port)
    (display %space-string port)
    (domain-write (addr-spec-domain addr-spec) port)
    (display %close-paren-string port))))

(define (addr-spec->string addr-spec)
  (string-append (local-part->string (addr-spec-local-part addr-spec))
		 %at-string
		 (domain->string (addr-spec-domain addr-spec))))


;;;; route record

(define-record-type route
  (fields (immutable domains)))

(define route-display
  (case-lambda
   ((self)
    (route-display self (current-output-port)))
   ((self port)
    (display (string-append "#<route -- " (route->string self) %close-angle-string) port))))

(define route-write
  (case-lambda
   ((self)
    (route-display self (current-output-port)))
   ((self port)
    (display "(make-route (list " port)
    (map (lambda (dom)
	   (domain-write dom port)
	   (display " " port))
      (route-domains self))
    (display "))" port))))

(define (route->string self)
  (string-join (map domain->string (route-domains self)) %comma-string))


;;;; mailbox record

(define-record-type mailbox
  (fields (immutable display-name) ;string or #f
	  (immutable route)	   ;route record or #f
	  (immutable addr-spec)))  ;addr-spec record

(define mailbox-display
  (case-lambda
   ((self)
    (mailbox-display self (current-output-port)))
   ((self port)
    (display (string-append "#<mailbox -- "
			    (mailbox->string self)
			    %close-angle-string)
	     port))))

(define mailbox-write
  (case-lambda
   ((self)
    (mailbox-write self (current-output-port)))
   ((self port)
    (display "(make-mailbox " port)
    (write (mailbox-display-name self) port)
    (display %space-string port)
    (let ((route (mailbox-route self)))
      (when route
	(route-write route port)))
    (display %space-string port)
    (addr-spec-write (mailbox-addr-spec self) port)
    (display %close-paren-string port))))

(define (mailbox->string self)
  (let ((display-name (mailbox-display-name self)))
    (string-append (if display-name
		       (string-append %double-quote-string
				      display-name
				      %double-quote-string
				      %space-string)
		     %empty-string)
		   %open-angle-string
		   (let ((route (mailbox-route self)))
		     (if route
			 (string-append (route->string route) %colon-string)
		       ""))
		   (addr-spec->string (mailbox-addr-spec self))
		   %close-angle-string)))


;;;; group record

(define-record-type group
  (fields (immutable display-name)
	  (immutable mailboxes)))

(define group-display
  (case-lambda
   ((self)
    (group-display self (current-output-port)))
   ((self port)
    (display (string-append "#<group -- "
			    (group->string self)
			    %close-angle-string)
	     port))))

(define group-write
  (case-lambda
   ((self)
    (group-display self (current-output-port)))
   ((self port)
    (display "(make-group " port)
    (write (group-display-name self) port)
    (map (lambda (mbox)
	   (mailbox-write mbox port)
	   (newline port))
      (group-mailboxes self))
    (display %close-angle-string port))))

(define (group->string self)
  (string-append %double-quote-string
		 (group-display-name self)
		 %double-quote-string
		 %space-string %colon-string
		 (string-join (map mailbox->string (group-mailboxes self)) ", ")
		 %colon-string))

;;;; done

)

;;; end of file
