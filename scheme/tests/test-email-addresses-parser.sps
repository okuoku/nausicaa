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


(import (nausicaa)
  (checks)
  (email addresses)
  (silex lexer)
  (strings)
  (records))

(check-set-mode! 'report-failed)
(display "*** testing email addresses\n")


(parameterise ((check-test-name 'simple-mailboxes))

  (define (doit string)
    (let* ((IS		(lexer-make-IS :string string :counters 'all))
	   (lexer	(make-address-lexer IS))
	   (parser	(make-address-parser))
	   (handler	(lambda (msg tok) (list 'error-handler msg tok)))
	   (result	(parser lexer handler)))
      (string-join (map object->string result) ", ")))

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
    (let* ((IS		(lexer-make-IS :string string :counters 'all))
	   (lexer	(make-address-lexer IS))
	   (parser	(make-address-parser))
	   (handler	(lambda (msg tok) (list 'error-handler msg tok)))
	   (result	(parser lexer handler)))
      (string-join (map object->string result) ", ")))

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
    (let* ((IS		(lexer-make-IS :string string :counters 'all))
	   (lexer	(make-address-lexer IS))
	   (parser	(make-address-parser))
	   (handler	(lambda (msg tok) (list 'error-handler msg tok)))
	   (result	(parser lexer handler)))
      (string-join (map object->string result) ", ")))

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
    (let* ((IS		(lexer-make-IS :string string :counters 'all))
	   (lexer	(make-address-lexer IS))
	   (parser	(make-address-parser))
	   (handler	(lambda (msg tok) (list 'error-handler msg tok)))
	   (result	(parser lexer handler)))
      (string-join (map object->string result) ", ")))

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
    (let* ((IS      (lexer-make-IS :string string
				   :counters 'all))
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


;;;; done

(check-report)

;;; end of file
