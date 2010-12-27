;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for URI library
;;;Date: Wed Jun  2, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(import (nausicaa)
  (nausicaa uri)
  (prefix (nausicaa uri low) uri:)
  (nausicaa checks))

(check-set-mode! 'report-failed)
(display "*** testing URI\n")


(parametrise ((check-test-name	'string/bytevector))

  (check (uri:to-string (uri:to-bytevector ""))			=> "")
  (check (uri:to-string (uri:to-bytevector "ciao"))		=> "ciao")
  (check (uri:to-string (uri:to-bytevector "ci%3fa%3do"))	=> "ci%3fa%3do")

  #t)


(parametrise ((check-test-name	'percent-encoding))

  (let ()

    (define-inline (doit ch str)
      (check (uri:percent-encode ch  (:string-result? #t)) => str)
      (check (uri:percent-decode str (:string-result? #t)) => (string ch)))

    (doit #\. ".")
    (doit #\- "-")
    (doit #\_ "_")
    (doit #\~ "~")
    (doit #\% "%25")
    (doit #\? "%3f")
    (doit #\= "%3d")
    (doit #\# "%23")

    #f)

  (let ()

    (define-inline (doit ch str)
      (check
	  (uri:percent-encode ch
			      (:string-result? #t)
			      (:char-selector (lambda (chi)
						   (memv (integer->char chi)
							 '(#\. #\- #\_ #\~ #\%
							   #\: #\/ #\?
							   #\# #\[ #\]
							   #\@ #\\ #\!
							   #\$ #\& #\'
							   #\( #\) #\*
							   #\+ #\, #\;
							   #\=))
						   )))
	=> str)
      (check (uri:percent-decode str (:string-result? #t)) => (string ch)))

    (doit #\. "%2e")
    (doit #\- "%2d")
    (doit #\_ "%5f")
    (doit #\~ "%7e")
    (doit #\% "%25")
    (doit #\? "%3f")
    (doit #\= "%3d")
    (doit #\# "%23")

    #f)

;;; --------------------------------------------------------------------

  (let ()

    (define-inline (doit dec enc)
      (check (uri:percent-encode dec (:string-result? #t)) => enc)
      (check (uri:percent-decode enc (:string-result? #t)) => dec))

    (doit "" "")
    (doit "ciao" "ciao")
    (doit "cia=o" "cia%3do")
    (doit "ci?a=o" "ci%3fa%3do")

    #f)

  (check
      (uri:percent-encode "ciao")
    => '#vu8(99 105 97 111))

  (check
      (uri:percent-decode '#vu8(99 105 97 111))
    => '#vu8(99 105 97 111))

  (check
      (uri:percent-decode '#vu8(99 105 97 111) (:string-result? #t))
    => "ciao")

;;; --------------------------------------------------------------------

  (check
      (uri:normalise-percent-encoded-string "")
    => "")

  (check
      (uri:normalise-percent-encoded-string "ciao")
    => "ciao")

  (check
      (uri:normalise-percent-encoded-string "cia%3do")
    => "cia%3do")

  (check
      (uri:normalise-percent-encoded-string "ci%3fa%3do")
    => "ci%3fa%3do")

  (check
      (uri:normalise-percent-encoded-string "%7eciao")
    => "~ciao")

  (check
      (uri:normalise-percent-encoded-string "ci%5fao")
    => "ci_ao")

;;; --------------------------------------------------------------------

  (check
      (uri:normalise-percent-encoded-bytevector (uri:to-bytevector ""))
    => (uri:to-bytevector ""))

  (check
      (uri:normalise-percent-encoded-bytevector (uri:to-bytevector "ciao"))
    => (uri:to-bytevector "ciao"))

  (check
      (uri:normalise-percent-encoded-bytevector (uri:to-bytevector "cia%3do"))
    => (uri:to-bytevector "cia%3do"))

  (check
      (uri:normalise-percent-encoded-bytevector (uri:to-bytevector "ci%3fa%3do"))
    => (uri:to-bytevector "ci%3fa%3do"))

  (check
      (uri:normalise-percent-encoded-bytevector (uri:to-bytevector "%7eciao"))
    => (uri:to-bytevector "~ciao"))

  (check
      (uri:normalise-percent-encoded-bytevector (uri:to-bytevector "ci%5fao"))
    => (uri:to-bytevector "ci_ao"))

  #t)


(parametrise ((check-test-name	'class-output))

  (define scheme	(string->utf8 "http"))
  (define authority	(string->utf8 "www.spiffy.org"))
  (define path		(map string->utf8 '("the" "path" "name")))
  (define query		(string->utf8 "question=answer"))
  (define fragment	(string->utf8 "anchor-point"))

  (define uri-string	"http://www.spiffy.org/the/path/name?question%3danswer#anchor-point")
  (define uri-bv	(string->utf8 uri-string))

  (define-syntax doit
    (syntax-rules ()
      ((_ ?var ?expected . ?body)
       (check
	   (let (((?var <uri>)
		  (make <uri>
		    (:decoded-scheme	scheme)
		    (:decoded-authority	authority)
		    (:decoded-path	path)
		    (:decoded-query	query)
		    (:decoded-fragment	fragment))))
	     . ?body)
	 => ?expected))))

;;; --------------------------------------------------------------------

  (doit o uri-string
	o.string)

  (doit o uri-bv
	o.bytevector)

  #t)


;;;; done

(check-report)

;;; end of file
