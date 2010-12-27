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
  (nausicaa armor quoted-printable)
  (nausicaa checks))

(check-set-mode! 'report-failed)
(display "*** testing ASCII armor quoted-printable\n")


;;;; helpers

(define (subbytevector src start past)
  (let ((dst (make-bytevector (- past start))))
    (do ((i 0 (+ 1 i))
	 (j start (+ 1 j)))
	((= j past)
	 dst)
      (bytevector-u8-set! dst i (bytevector-u8-ref src j)))))

(define encoding
  (make-parameter #f))


;;;; qprint encoding and decoding routines

(define (encode binary)
  ;;Encode BINARY which  must be a Scheme string  or bytevector.  Return
  ;;two values:  (1) the resulting boolean from  the encoding functions,
  ;;(2) a string representing the encoded data.
  ;;
  (let* ((ctx		(make-<qprint-encode-ctx> (encoding)))
	 (src		(if (string? binary) (string->utf8 binary) binary))
	 (src-len	(bytevector-length src))
	 (dst		(make-bytevector (* 3 src-len) 0)))
    (receive (dst-next src-next)
	(qprint-encode-update! ctx dst 0 src 0 src-len)
      (receive (result dst-next src-next)
	  (qprint-encode-final! ctx dst dst-next src src-next src-len)
	(list result (utf8->string (subbytevector dst 0 dst-next)))))))

(define (decode binary string-result?)
  ;;Decode BINARY which  must be a Scheme string  or bytevector.  Return
  ;;two  values: the  boolean result  from the  decoding  functions, the
  ;;decoded data.
  ;;
  ;;If STRING-RESULT?  is  true, the decoded data is  returned as Scheme
  ;;string; else it is returned as Scheme bytevector.
  ;;
  (define (%output dst dst-past)
    (let ((bv (subbytevector dst 0 dst-past)))
      (if string-result?
	  (utf8->string bv)
	bv)))
  (let* ((ctx		(make-<qprint-decode-ctx>))
	 (src		(if (string? binary)
			    (string->utf8 binary)
			  binary))
	 (src-len	(bytevector-length src))
	 (dst		(make-bytevector (* 3 src-len) 0)))
    (receive (result dst-next src-next)
	(qprint-decode-update! ctx dst 0 src 0 src-len)
      (if result
	  (list result (%output dst dst-next))
	(receive (result dst-next src-next)
	    (qprint-decode-final! ctx dst dst-next src src-next src-len)
	  (list result (%output dst dst-next)))))))


(parametrise ((check-test-name	'upper)
	      (encoding		'default))

  (let ((plain	'#vu8(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
	(ascii	"=00=01=02=03=04=05=06=07=08=09=0A=0B=0C=0D=0E=0F"))
    (check (encode plain)	=> `(#t ,ascii))
    (check (decode ascii #f)	=> `(#t ,plain)))

  (let ((plain	'#vu8(16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31))
	(ascii	"=10=11=12=13=14=15=16=17=18=19=1A=1B=1C=1D=1E=1F"))
    (check (encode plain)	=> `(#t ,ascii))
    (check (decode ascii #f)	=> `(#t ,plain)))

  (let ((plain	(string->utf8 "ciao"))
	(ascii	"ciao"))
    (check (encode plain)	=> `(#t ,ascii))
    (check (decode ascii #f)	=> `(#t ,plain)))

  (let ((plain	(string->utf8 "ciao\x1;"))
	(ascii	"ciao=01"))
    (check (encode plain)	=> `(#t ,ascii))
    (check (decode ascii #f)	=> `(#t ,plain)))

  (let ((plain	(string->utf8 "c"))
	(ascii	"c"))
    (check (encode plain)	=> `(#t ,ascii))
    (check (decode ascii #f)	=> `(#t ,plain)))

  (let ((plain	(string->utf8 "c\x1;"))
	(ascii	"c=01"))
    (check (encode plain)	=> `(#t ,ascii))
    (check (decode ascii #f)	=> `(#t ,plain)))

  (let ((plain	(string->utf8 "!\"#$@[\\]^`{|}~"))
	(ascii	"!\"#$@[\\]^`{|}~"))
    (check (encode plain)	=> `(#t ,ascii))
    (check (decode ascii #f)	=> `(#t ,plain)))

  #t)


(parametrise ((check-test-name	'upper)
	      (encoding		'strong))

  (let ((plain	'#vu8(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
	(ascii	"=00=01=02=03=04=05=06=07=08=09=0A=0B=0C=0D=0E=0F"))
    (check (encode plain)	=> `(#t ,ascii))
    (check (decode ascii #f)	=> `(#t ,plain)))

  (let ((plain	'#vu8(16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31))
	(ascii	"=10=11=12=13=14=15=16=17=18=19=1A=1B=1C=1D=1E=1F"))
    (check (encode plain)	=> `(#t ,ascii))
    (check (decode ascii #f)	=> `(#t ,plain)))

  (let ((plain	(string->utf8 "ciao"))
	(ascii	"ciao"))
    (check (encode plain)	=> `(#t ,ascii))
    (check (decode ascii #f)	=> `(#t ,plain)))

  (let ((plain	(string->utf8 "ciao\x1;"))
	(ascii	"ciao=01"))
    (check (encode plain)	=> `(#t ,ascii))
    (check (decode ascii #f)	=> `(#t ,plain)))

  (let ((plain	(string->utf8 "c"))
	(ascii	"c"))
    (check (encode plain)	=> `(#t ,ascii))
    (check (decode ascii #f)	=> `(#t ,plain)))

  (let ((plain	(string->utf8 "c\x1;"))
	(ascii	"c=01"))
    (check (encode plain)	=> `(#t ,ascii))
    (check (decode ascii #f)	=> `(#t ,plain)))

  (let ((plain	(string->utf8 "!\"#$@[\\]^`{|}~"))
	(ascii	"=21=22=23=24=40=5B=5C=5D=5E=60=7B=7C=7D=7E"))
    (check (encode plain)	=> `(#t ,ascii))
    (check (decode ascii #f)	=> `(#t ,plain)))

  #t)


;;;; done

(check-report)

;;; end of file
