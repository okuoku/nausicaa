;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for ascii armor of bytevectors
;;;Date: Sun Jan 24, 2010
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
  (armor base16)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing ASCII armor\n")


(parametrise ((check-test-name	'base16))

  (check	;upper case
      (let* ((ctx (make-<base16-encode-ctx> #t))
	     (src '#vu8(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
	     (dst (make-bytevector (base16-encode-length (bytevector-length src)))))
	(base16-encode-update! ctx dst 0 src 0 (bytevector-length src))
	(utf8->string dst))
    => "000102030405060708090A0B0C0D0E0F")

  (check	;lower case
      (let* ((ctx (make-<base16-encode-ctx> #f))
	     (src '#vu8(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
	     (dst (make-bytevector (base16-encode-length (bytevector-length src)))))
	(base16-encode-update! ctx dst 0 src 0 (bytevector-length src))
	(utf8->string dst))
    => "000102030405060708090a0b0c0d0e0f")

  (check
      (let* ((ctx (make-<base16-encode-ctx> #t))
	     (src '#vu8(16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31))
	     (dst (make-bytevector (base16-encode-length (bytevector-length src)))))
	(base16-encode-update! ctx dst 0 src 0 (bytevector-length src))
	(utf8->string dst))
    => "101112131415161718191A1B1C1D1E1F")

;;; --------------------------------------------------------------------

  (check	;upper case
      (let* ((ctx (make-<base16-decode-ctx> #f))
	     (src (string->utf8 "000102030405060708090A0B0C0D0E0F"))
	     (dst (make-bytevector (base16-decode-length (bytevector-length src)))))
	(let ((result (base16-decode-update! ctx dst 0 src 0 (bytevector-length src))))
	  (list result dst)))
    => '(16 #vu8(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)))

  (check	;lower case
      (let* ((ctx (make-<base16-decode-ctx> #f))
	     (src (string->utf8 "000102030405060708090a0b0c0d0e0f"))
	     (dst (make-bytevector (base16-decode-length (bytevector-length src)))))
	(base16-decode-update! ctx dst 0 src 0 (bytevector-length src))
	dst)
    => '#vu8(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))

  (check
      (let* ((ctx (make-<base16-decode-ctx> #f))
	     (src (string->utf8 "101112131415161718191A1B1C1D1E1F"))
	     (dst (make-bytevector (base16-decode-length (bytevector-length src)))))
	(base16-decode-update! ctx dst 0 src 0 (bytevector-length src))
	dst)
    => '#vu8(16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31))

  (check	;allow blanks
      (let* ((ctx (make-<base16-decode-ctx> #t))
	     (src (string->utf8 "101 11213141  516171819\n1A\t1B1C\r\r1D1E1F"))
	     (dst (make-bytevector (base16-decode-length (bytevector-length src)) 0)))
	(let ((result (base16-decode-update! ctx dst 0 src 0 (bytevector-length src))))
	  (list result dst)))
    => '(16 #vu8(16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31
		;overallocated for blanks
		    0 0 0 0)))

  (check	;disallow blanks
      (guard (E (else #f))
	(let* ((ctx (make-<base16-decode-ctx> #f))
	       (src (string->utf8 "101 11213141  516171819\n1A\t1B1C\r\r1D1E1F"))
	       (dst (make-bytevector (base16-decode-length (bytevector-length src)) 0)))
	  (base16-decode-update! ctx dst 0 src 0 (bytevector-length src))
	  dst))
    => #f)

  #t)


;;;; done

(check-report)

;;; end of file
