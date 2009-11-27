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

  (define (copy-memblock mb)
    (make-<memblock> (<memblock>-pointer mb)
		     (<memblock>-size    mb)))

  (define (done-memblock base cursor)
    (make-<memblock> (<memblock>-pointer base)
		     (- (<memblock>-size base)
			(<memblock>-size cursor))))

  (define memblock-null
    (make-<memblock> pointer-null 0))

  (check
      (with-compensations
	(let* ((ctx		(iconv-open/c (iconv-encoding UTF-8)
					      (iconv-encoding UTF-16)))
	       (in		(bytevector->memblock (string->utf8 "ciao") malloc/c))
	       (ou		(malloc-memblock/c 16))
	       (in-cursor	(copy-memblock in))
	       (ou-cursor	(copy-memblock ou)))
	  (iconv! ctx in-cursor ou-cursor)
	  (<memblock>-size in-cursor)))
    => 0)

  (check
      (with-compensations
	(let* ((ctx		(iconv-open/c (iconv-encoding UTF-8 TRANSLIT IGNORE)
					      (iconv-encoding UTF-16)))
	       (in		(bytevector->memblock (string->utf8 "ciao") malloc/c))
	       (ou		(malloc-memblock/c 16))
	       (in-cursor	(copy-memblock in))
	       (ou-cursor	(copy-memblock ou)))
	  (iconv! ctx in-cursor ou-cursor)
	  (<memblock>-size in-cursor)))
    => 0)

  (check
      (with-compensations
	(let* ((ctx		(iconv-open/c (iconv-encoding UTF-8)
					      (iconv-encoding UTF-16)))
	       (in		(bytevector->memblock (string->utf8 "ciao") malloc/c))
	       (ou		(malloc-memblock/c 16))
	       (in-cursor	(copy-memblock in))
	       (ou-cursor	(copy-memblock ou)))
	  (iconv! ctx in-cursor ou-cursor)
	  (let* ((ou-done	(done-memblock ou ou-cursor))
		 (bv		(memblock->bytevector ou-done)))
	    (utf16->string bv (if words-bigendian 'big 'little)))))
    => "ciao")

  (check 'this
      (with-compensations
	(let* ((ctx1		(iconv-open/c (iconv-encoding UTF-8)
					      (iconv-encoding UTF-32)))

	       (A		(bytevector->memblock (string->utf8 "ciao") malloc/c))
	       (B		(malloc-memblock/c 16))
	       (A-cursor	(copy-memblock A))
	       (B-cursor	(copy-memblock B)))
	  (iconv! ctx1 A-cursor B-cursor)
(write (<memblock>-size A-cursor))(newline)
(write (<memblock>-size B-cursor))(newline)
	  (iconv! ctx1 memblock-null B-cursor)
(write (<memblock>-size B-cursor))(newline)
	  (let* ((ctx2		(iconv-open/c (iconv-encoding UTF-32)
					      (iconv-encoding UTF-8)))
		 (B-done	(done-memblock B B-cursor))
		 (C		(malloc-memblock/c 16))
		 (C-cursor	(copy-memblock C)))
	    (iconv! ctx2 B-done C-cursor)
	    (let ((C-done (done-memblock C C-cursor)))
	      (utf8->string (memblock->bytevector C-done))))))
    => "ciao")

  #t)


;;;; done

(check-report)

;;; end of file
