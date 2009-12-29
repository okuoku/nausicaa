;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Gcrypt
;;;Contents: test high-level API
;;;Date: Sat Dec 26, 2009
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
  (foreign cstrings)
  (foreign crypto gcrypt)
  (foreign crypto gcrypt compensated)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing Gcrypt platform\n")

(assert (gcry-check-version))
(gcry-control/int GCRYCTL_DISABLE_SECMEM 0)
(gcry-control/int GCRYCTL_INITIALIZATION_FINISHED 0)


(parametrise ((check-test-name	'enum))

  (check (gcry-cipher-algo->value (gcry-cipher-algo cast5))	=> GCRY_CIPHER_CAST5)
  (check (gcry-cipher-algo->value (gcry-cipher-algo blowfish))	=> GCRY_CIPHER_BLOWFISH)

  (check (value->gcry-cipher-algo GCRY_CIPHER_CAST5)	(=> enum-set=?) (gcry-cipher-algo cast5))
  (check (value->gcry-cipher-algo GCRY_CIPHER_BLOWFISH)	(=> enum-set=?) (gcry-cipher-algo blowfish))

;;; --------------------------------------------------------------------

  (check (gcry-cipher-mode->value (gcry-cipher-mode ecb))	=> GCRY_CIPHER_MODE_ECB)
  (check (gcry-cipher-mode->value (gcry-cipher-mode ctr))	=> GCRY_CIPHER_MODE_CTR)

  (check (value->gcry-cipher-mode GCRY_CIPHER_MODE_ECB)	(=> enum-set=?) (gcry-cipher-mode ecb))
  (check (value->gcry-cipher-mode GCRY_CIPHER_MODE_CTR)	(=> enum-set=?) (gcry-cipher-mode ctr))

;;; --------------------------------------------------------------------

  (check (gcry-cipher-flags->value (gcry-cipher-flags secure))		=> GCRY_CIPHER_SECURE)
  (check (gcry-cipher-flags->value (gcry-cipher-flags enable-sync))	=> GCRY_CIPHER_ENABLE_SYNC)
  (check (gcry-cipher-flags->value (gcry-cipher-flags cbc-cts))		=> GCRY_CIPHER_CBC_CTS)
  (check (gcry-cipher-flags->value (gcry-cipher-flags cbc-mac))		=> GCRY_CIPHER_CBC_MAC)

  (check
      (gcry-cipher-flags->value (gcry-cipher-flags secure enable-sync))
    => (bitwise-ior GCRY_CIPHER_SECURE GCRY_CIPHER_ENABLE_SYNC))




  #t)


(parametrise ((check-test-name	'symmetric))

  (check
      (with-compensations
	(define (make)
	  (begin0-let ((hd (gcry-cipher-open/c (gcry-cipher-algo arcfour)
					       (gcry-cipher-mode stream)
					       (gcry-cipher-flags))))
	    (gcry-cipher-setkey hd "ciao, ciao")))
	(let ((enc (make))
	      (dec (make)))
	  (let ((bv (gcry-cipher-encrypt* enc "you complete my fate")))
	    (utf8->string (gcry-cipher-decrypt* dec bv)))))
    => "you complete my fate")

  (let ((str "you complete my fate0123"))
;;;           012345678901234567890123
    (check
	(with-compensations
	  (define (make)
	    (begin0-let ((hd (gcry-cipher-open/c (gcry-cipher-algo des)
						 (gcry-cipher-mode ecb)
						 (gcry-cipher-flags))))
	      (gcry-cipher-setkey hd "12345678")
	      (gcry-cipher-setiv  hd "hello678")))
	  (let ((enc (make))
		(dec (make)))
	    (let ((bv (gcry-cipher-encrypt* enc str)))
	      (utf8->string (gcry-cipher-decrypt* dec bv)))))
      => str))

  (let ((str "you complete my fate0123"))
;;;           012345678901234567890123
    (check
	(with-compensations
	  (define (make)
	    (begin0-let ((hd (gcry-cipher-open/c (gcry-cipher-algo des)
						 (gcry-cipher-mode ctr)
						 (gcry-cipher-flags))))
	      (gcry-cipher-setkey hd "12345678")
	      (gcry-cipher-setiv  hd "hello678")
	      (gcry-cipher-setctr hd "salut678")))
	  (let ((enc (make))
		(dec (make)))
	    (let ((bv (gcry-cipher-encrypt* enc str)))
	      (utf8->string (gcry-cipher-decrypt* dec bv)))))
      => str))

  (let ((str "you complete my fate0123"))
;;;           012345678901234567890123
    (check
	(with-compensations
	  (define (make)
	    (begin0-let ((hd (gcry-cipher-open/c (gcry-cipher-algo des)
						 (gcry-cipher-mode ofb)
						 (gcry-cipher-flags))))
	      (gcry-cipher-setkey hd "12345678")
	      (gcry-cipher-setiv  hd "hello678")))
	  (let ((enc (make))
		(dec (make)))
	    (let ((bv (gcry-cipher-encrypt* enc str)))
	      (utf8->string (gcry-cipher-decrypt* dec bv)))))
      => str))

  #t)


;;;; done

(check-report)

;;; end of file
