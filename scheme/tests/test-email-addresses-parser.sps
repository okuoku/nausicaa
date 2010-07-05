;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for the email addresses parser
;;;Date: Thu Jul 30, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009, 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(import (nausicaa)
  (generics object-to-string)
  (checks)
  (email addresses)
  (silex lexer)
  (strings))

(check-set-mode! 'report-failed)
(display "*** testing email addresses\n")


(parameterise ((check-test-name 'simple-mailboxes))

  (define (doit string)
    (let* ((IS		(lexer-make-IS (:string string) (:counters 'all)))
	   (lexer	(make-address-lexer IS))
	   (parser	(make-address-parser))
	   (handler	(lambda (msg tok) (list 'error-handler msg tok)))
	   (result	(parser lexer handler)))
      (string-join (map (lambda (o) (object->string o)) result) ", ")))

;;; --------------------------------------------------------------------

  (check	;without agle brackets
      (doit "simons@rhein.de")
    => "<simons@rhein.de>")

  (check	;with angle brackets
      (doit "<simons@rhein.de>")
    => "<simons@rhein.de>")

  (check	;with atom phrase
      (doit "Peter Simons <simons@rhein.de>")
    => "Peter Simons <simons@rhein.de>")

  (check	;with quoted-text phrase
      (doit "\"Peter Simons\" <simons@rhein.de>")
    => "Peter Simons <simons@rhein.de>")

  (check	;with CRLF sequence
      (doit "Peter Simons\r\n <simons@rhein.de>")
    => "Peter Simons <simons@rhein.de>")

  (check	;with weird phrase
      (doit "=?ISO-8859-15?Q?Andr=E9s_Garc=EDa?= <fandom@spamme.telefonica.net>")
    => "=?ISO-8859-15?Q?Andr=E9s_Garc=EDa?= <fandom@spamme.telefonica.net>")

  (check	;with weird phrase
      (doit "=?iso-8859-1?q?Ulrich_Sch=F6bel?= <ulrich@outvert.com>")
    => "=?iso-8859-1?q?Ulrich_Sch=F6bel?= <ulrich@outvert.com>")

  (check	;with quoted-text phrase requiring double quotes (because of comma)
      (doit " \"Steve Redler IV, Tcl2006 Conference Program Chair\" <steve@sr-tech.com>")
    => "\"Steve Redler IV, Tcl2006 Conference Program Chair\" <steve@sr-tech.com>")

  (check	;with comment
      (doit "\"Peter Simons\" (Peter Simons) <simons@rhein.de>")
    => "Peter Simons <simons@rhein.de>")

  (check	;with domain literal
      (doit "simons@[1.2.3.4]")
    => "<simons@[1.2.3.4]>")

  #t)


(parameterise ((check-test-name 'route-mailboxes))

  (define (doit string)
    (let* ((IS		(lexer-make-IS (:string string) (:counters 'all)))
	   (lexer	(make-address-lexer IS))
	   (parser	(make-address-parser))
	   (handler	(lambda (msg tok) (list 'error-handler msg tok)))
	   (result	(parser lexer handler)))
      (string-join (map (lambda (o) (object->string o)) result) ", ")))

;;; --------------------------------------------------------------------

  (check
      (parameterise ((debugging #t))
	(doit "<@here.it:simons@rhein.de>"))
    => "<@here.it:simons@rhein.de>")

  (check
      (parameterise ((debugging #t))
	(doit "<@here.it, @there.us:simons@rhein.de>"))
    => "<@here.it,@there.us:simons@rhein.de>")

  #t)


(parameterise ((check-test-name 'groups))

  (define (doit string)
    (let* ((IS		(lexer-make-IS (:string string) (:counters 'all)))
	   (lexer	(make-address-lexer IS))
	   (parser	(make-address-parser))
	   (handler	(lambda (msg tok) (list 'error-handler msg tok)))
	   (result	(parser lexer handler)))
      (string-join (map (lambda (o) (object->string o)) result) ", ")))

;;; --------------------------------------------------------------------

  (check	;display name only
      (doit "the group:;")
    => "the group: ;")

  (check	;display name only
      (doit "\"the group\":   ;")
    => "the group: ;")

  (check	;commas festival
      (doit "\"the group\": ,,, ,,, ,,, , , , , , , ,,,,  ;")
    => "the group: ;")

  (check	;one mailbox
      (doit "the group: marco.maggi@here.it;")
    => "the group: <marco.maggi@here.it>;")

  (check	;two mailboxes
      (doit "the group: marco.maggi@here.it, <marco.maggi@there.it>;")
    => "the group: <marco.maggi@here.it>, <marco.maggi@there.it>;")

  (check	;three mailboxes
      (doit "the group: marco.maggi@here.it, <marco.maggi@there.it>, \r
	Marco Maggi <mrc.mgg@here.it>;")
    => "the group: <marco.maggi@here.it>, <marco.maggi@there.it>, Marco Maggi <mrc.mgg@here.it>;")

  (check	;three mailboxes with commas festival
      (doit "the group: ,, marco.maggi@here.it, <marco.maggi@there.it>, ,, , , , , ,\r
	Marco Maggi <mrc.mgg@here.it>, ,,,,,,, ,, ,,, ,;")
    => "the group: <marco.maggi@here.it>, <marco.maggi@there.it>, Marco Maggi <mrc.mgg@here.it>;")

 #t)


(parameterise ((check-test-name 'complex-addresses))

  (define (doit string)
    (let* ((IS		(lexer-make-IS (:string string) (:counters 'all)))
	   (lexer	(make-address-lexer IS))
	   (parser	(make-address-parser))
	   (handler	(lambda (msg tok) (list 'error-handler msg tok)))
	   (result	(parser lexer handler)))
      (string-join (map (lambda (o) (object->string o)) result) ", ")))

;;; --------------------------------------------------------------------

  (check	;commas festival
      (doit ",,, , , , ,,, ,  ,, , , , , ,,,")
    => "")

  (check	;a list of mailboxes
      (doit "marco.maggi@here.it, <marco.maggi@there.it>")
    => "<marco.maggi@here.it>, <marco.maggi@there.it>")

  (check	;a group and a mailbox
      (doit "ciao: marco.maggi@here.it;, <marco.maggi@there.it>")
    => "ciao: <marco.maggi@here.it>;, <marco.maggi@there.it>")

  (check	;a mailbox and a group
      (doit "<marco.maggi@there.it>, ciao: marco.maggi@here.it;")
    =>  "<marco.maggi@there.it>, ciao: <marco.maggi@here.it>;")

  (check	;a group and a group
      (doit "hello: <marco.maggi@there.it>;, ciao: marco.maggi@here.it;")
    =>  "hello: <marco.maggi@there.it>;, ciao: <marco.maggi@here.it>;")

  (check	;a group and a group with empty commas
      (doit "hello: <marco.maggi@there.it>;,,,,, ciao: marco.maggi@here.it;")
    =>  "hello: <marco.maggi@there.it>;, ciao: <marco.maggi@here.it>;")

  (check	;a group and a group with empty commas
      (doit "hello: <marco.maggi@there.it>;,,,,, ciao: marco.maggi@here.it;,,")
    =>  "hello: <marco.maggi@there.it>;, ciao: <marco.maggi@here.it>;")

  (check
      (doit "testing my parser : peter.simons@gmd.de, (peter.)simons@rhein.de ,,,, <simons@ieee.org>;")
    => "testing my parser: <peter.simons@gmd.de>, <simons@rhein.de>, <simons@ieee.org>;")

  (check
      (doit "testing my parser\r
 : peter.simons@gmd.de, (peter.)simons@rhein.de ,,,\r
 , <simons@ieee.org>;")
    => "testing my parser: <peter.simons@gmd.de>, <simons@rhein.de>, <simons@ieee.org>;")

  #;(check
      (doit "testing my parser : peter.simons@gmd.de,\r
            (peter.)simons@rhein.de ,,,,,\r
         testing my parser <simons@ieee.org>,\r
         it rules <@peti.gmd.de,@listserv.gmd.de:simons @ cys .de>\r
         ;\r
         ,\r
         peter.simons@acm.org")
    => "testing my parser: <peter.simons@gmd.de>, <simons@rhein.de>, testing my parser <simons@ieee.org>, it rules <@peti.gmd.de,@listserv.gmd.de:simons@cys.de>;, <peter.simons@acm.org>")

  #t)


(parametrise ((check-test-name 'examples))

  (define (error-handler message token)
    (error #f message token))

  (define (doit string)
    (let* ((IS      (lexer-make-IS (:string string)
				   (:counters 'all)))
	   (lexer   (make-address-lexer IS))
	   (parser  (make-address-parser)))
      (parser lexer error-handler)))

;;; --------------------------------------------------------------------

  (let ((result (doit "marco.maggi@here.it, <marco.maggi@there.it>")))

    (check
	(length result)
      => 2)

    (check
	(<mailbox>? (car result))
      => #t)

    (check
	(<mailbox>? (cadr result))
      => #t)

    (check
	(object->string (car result))
      => "<marco.maggi@here.it>")

    (check
	(object->string (cadr result))
      => "<marco.maggi@there.it>")

    #f)

;;; --------------------------------------------------------------------

  (let ((result (doit "the group: marco.maggi@here.it, <marco.maggi@there.it>, \r
	Marco Maggi <mrc.mgg@here.it>;")))

    (check
	(length result)
      => 1)

    (check
	(<group>? (car result))
      => #t)

    (let* ((group	(car result))
	   (mailboxes	(<group>-mailboxes group)))

      (check
	  (length mailboxes)
	=> 3)

      (check
	  (<group>-display-name group)
	=> "the group")

      (check
	  (object->string (car mailboxes))
	=> "<marco.maggi@here.it>")

      (check
	  (object->string (cadr mailboxes))
	=> "<marco.maggi@there.it>")

      (check
	  (object->string (<mailbox>-addr-spec (caddr mailboxes)))
	=> "mrc.mgg@here.it")

      #f)
    #f)
  #t)


(parametrise ((check-test-name 'valid-addresses))

  (define (error-handler message token)
    (error #f message token))

  (define (doit string)
    (let* ((IS      (lexer-make-IS (:string string) (:counters 'all)))
	   (lexer   (make-address-lexer IS))
	   (parser  (make-address-parser)))
      (parser lexer error-handler)))

;;; --------------------------------------------------------------------

  (define-syntax check-it
    (syntax-rules ()
      ((_ ?address)
       (let ((o (doit ?address)))
	 (check (is-a? (car o) <mailbox>) => #t)))))

;;; The following email addresses come from:
;;;
;;; http://fightingforalostcause.net/misc/2006/compare-email-regex.php


  (check-it "l3tt3rsAndNumb3rs@domain.com")
  (check-it "has-dash@domain.com")
  (check-it "hasApostrophe.o'leary@domain.org")
  (check-it "uncommonTLD@domain.museum")
  (check-it "uncommonTLD@domain.travel")
  (check-it "uncommonTLD@domain.mobi")
  (check-it "countryCodeTLD@domain.uk")
  (check-it "countryCodeTLD@domain.rw")
  (check-it "lettersInDomain@911.com")
  (check-it "underscore_inLocal@domain.net")
  (check-it "IPInsteadOfDomain@127.0.0.1")
;  (check-it "IPAndPort@127.0.0.1:25")
  (check-it "subdomain@sub.domain.com")
  (check-it "local@dash-inDomain.com")
  (check-it "dot.inLocal@foo.com")
  (check-it "a@singleLetterLocal.org")
  (check-it "singleLetterDomain@x.org")
  (check-it "&*=?^+{}'~@validCharsInLocal.net")
  (check-it "foor@bar.newTLD")


  #t)


(parametrise ((check-test-name 'invalid-addresses))

  (define (error-handler message token)
    (error #f message token))

  (define (doit string)
    (let* ((IS      (lexer-make-IS (:string string) (:counters 'all)))
	   (lexer   (make-address-lexer IS))
	   (parser  (make-address-parser)))
      (parser lexer error-handler)))

;;; --------------------------------------------------------------------

  (define-syntax check-it
    (syntax-rules ()
      ((_ ?address)
       (check
	   (guard (E (else #t))
	     (doit ?address))
	 => #t))))

;;; The following email addresses come from:
;;;
;;; http://fightingforalostcause.net/misc/2006/compare-email-regex.php

  (check-it "missingDomain@.com")
  (check-it "@missingLocal.org")
  (check-it "missingatSign.net")

;;;No dot should be allowed, else we discard "marco@localhost".
;;;
;;;  (check-it "missingDot@com")

  (check-it "two@@signs.com")
  (check-it "colonButNoPort@127.0.0.1:")

;;;This has  what appears  to be  a domain literal  with 5  numbers; but
;;;domain literals are enclosed in [...], which this is not.
;;;
;;;  (check-it "someone-else@127.0.0.1.26")

  (check-it ".localStartsWithDot@domain.com")
  (check-it "localEndsWithDot.@domain.com")
  (check-it "two..consecutiveDots@domain.com")

;;; You sure that these are invalid?
;;;
;;;  (check-it "domainStartsWithDash@-domain.com")
;;;  (check-it "domainEndsWithDash@domain-.com")
;;;  (check-it "numbersInTLD@domain.c0m")
;;;  (check-it "local@SecondLevelDomainNamesAreInvalidIfTheyAreLongerThan64Charactersss.org")

  (check-it "missingTLD@domain.")
  (check-it "! \"#$%(),/;<>[]`|@invalidCharsInLocal.org")
  (check-it "invalidCharsInDomain@! \"#$%(),/;<>_[]`|.org")

  #t)


;;;; done

(check-report)

;;; end of file
