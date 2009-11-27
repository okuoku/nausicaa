;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Iconv
;;;Contents: tests for Iconv
;;;Date: Fri Nov 27, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (compensations)
  (foreign i18n iconv)
  (foreign i18n iconv compensated)
  (foreign memory)
  (only (foreign ffi sizeof) words-bigendian)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing Iconv\n")


(parametrise ((check-test-name	'iconv))

  (check
      (with-compensations
	(let* ((ctx	(iconv-open/c (iconv-encoding UTF-16)
				      (iconv-encoding UTF-8)))
	       (in	(bytevector->memblock (string->utf8 "ciao") malloc/c))
	       (ou	(malloc-memblock/c 16))
	       (in-tail	(memblock-shallow-clone in))
	       (ou-tail	(memblock-shallow-clone ou)))
	  (iconv! ctx ou-tail in-tail)
	  (iconv! ctx ou-tail)
	  (<memblock>-size in-tail)))
    => 0)

  (check
      (with-compensations
	(let* ((ctx	(iconv-open/c (iconv-encoding UTF-16)
				      (iconv-encoding UTF-8 TRANSLIT IGNORE)))
	       (in	(bytevector->memblock (string->utf8 "ciao") malloc/c))
	       (ou	(malloc-memblock/c 16))
	       (in-tail	(memblock-shallow-clone in))
	       (ou-tail	(memblock-shallow-clone ou)))
	  (iconv! ctx ou-tail in-tail)
	  (iconv! ctx ou-tail)
	  (<memblock>-size in-tail)))
    => 0)

;;; --------------------------------------------------------------------

  (check
      (with-compensations
	(let* ((ctx	(iconv-open/c (iconv-encoding UTF-16)
				      (iconv-encoding UTF-8)))
	       (in	(bytevector->memblock (string->utf8 "ciao") malloc/c))
	       (ou	(malloc-memblock/c 16))
	       (in-tail	(memblock-shallow-clone in))
	       (ou-tail	(memblock-shallow-clone ou)))
	  (iconv! ctx ou-tail in-tail)
	  (iconv! ctx ou-tail)
	  (let* ((ou-head	(memblock&tail-head ou ou-tail))
		 (bv		(memblock->bytevector ou-head)))
	    (utf16->string bv (if words-bigendian 'big 'little)))))
    => "ciao")

  (check
    (with-compensations
      (let* ((ctx1	(iconv-open/c (iconv-encoding UTF-32)
				      (iconv-encoding UTF-8)))
	     (A		(bytevector->memblock (string->utf8 "ciao") malloc/c))
	     (B		(malloc-memblock/c 64))
	     (A-tail	(memblock-shallow-clone A))
	     (B-tail	(memblock-shallow-clone B)))
	(iconv! ctx1 B-tail A-tail)
	(iconv! ctx1 B-tail)
	(let* ((B-head	(memblock&tail-head B B-tail))
	       (ctx2	(iconv-open/c (iconv-encoding UTF-8)
				      (iconv-encoding UTF-32)))
	       (C	(malloc-memblock/c 64))
	       (C-tail	(memblock-shallow-clone C)))
	  (iconv! ctx2 C-tail B-head)
	  (iconv! ctx2 C-tail)
	  (let ((C-head (memblock&tail-head C C-tail)))
	    (utf8->string (memblock->bytevector C-head))))))
    => "ciao")

  #t)


;;;; done

(check-report)

;;; end of file
