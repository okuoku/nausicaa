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
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing URI\n")


(parametrise ((check-test-name	'percent-encoding))

  (let ()

    (define-inline (doit ch str)
      (check (percent-decode-string str) => ch)
      (check (percent-encode-char   ch)  => str))

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
      (check (string->percent-encoded-string dec) => enc)
      (check (percent-encoded-string->string enc) => dec))

    (doit "" "")
    (doit "ciao" "ciao")
    (doit "cia=o" "cia%3do")
    (doit "ci?a=o" "ci%3fa%3do")

    #f)

;;; --------------------------------------------------------------------

  (check
      (normalise-percent-encoded-string "")
    => "")

  (check
      (normalise-percent-encoded-string "ciao")
    => "ciao")

  (check
      (normalise-percent-encoded-string "cia%3do")
    => "cia%3do")

  (check
      (normalise-percent-encoded-string "ci%3fa%3do")
    => "ci%3fa%3do")

  (check
      (normalise-percent-encoded-string "%7eciao")
    => "~ciao")

  (check
      (normalise-percent-encoded-string "ci%5fao")
    => "ci_ao")


  #t)


;;;; done

(check-report)

;;; end of file
