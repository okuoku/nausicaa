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


(import (nausicaa)
  (uri)
  (prefix (uri low) uri:)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing URI\n")


(parametrise ((check-test-name	'percent-encoding))

  (check (uri:to-string (uri:to-utf8 ""))		=> "")
  (check (uri:to-string (uri:to-utf8 "ciao"))		=> "ciao")
  (check (uri:to-string (uri:to-utf8 "ci%3fa%3do"))	=> "ci%3fa%3do")

;;; --------------------------------------------------------------------

  (let ()

    (define-inline (doit ch str)
      (check (uri:percent-encode ch)  => str)
      (check (uri:percent-decode str) => (string ch)))

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
	  (uri:percent-encode ch (:char-selector (lambda (chi)
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
      (check (uri:percent-decode str) => (string ch)))

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
      (check (uri:percent-encode dec) => enc)
      (check (uri:percent-decode enc) => dec))

    (doit "" "")
    (doit "ciao" "ciao")
    (doit "cia=o" "cia%3do")
    (doit "ci?a=o" "ci%3fa%3do")

    #f)

  (check
      (uri:percent-encode "ciao" (:bytevector-result? #t))
    => '#vu8(99 105 97 111))

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

  #t)


;;;; done

(check-report)

;;; end of file
