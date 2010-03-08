;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for base91 ASCII armor of bytevectors
;;;Date: Mon Mar  8, 2010
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
  (armor base91)
  (armor conditions)
  (checks)
  (parameters))

(check-set-mode! 'report-failed)
(display "*** testing ASCII armor base91\n")


;;;; helpers

(define (subbytevector src start past)
  (let ((dst (make-bytevector (- past start))))
    (do ((i 0 (+ 1 i))
	 (j start (+ 1 j)))
	((= j past)
	 dst)
      (bytevector-u8-set! dst i (bytevector-u8-ref src j)))))


;;;; base91 encoding and decoding routines

(define (encode binary)
  ;;Encode BINARY which  must be a Scheme string  or bytevector.  Return
  ;;two values:  (1) the resulting boolean from  the encoding functions,
  ;;(2) a string representing the encoded data.
  ;;
  (let* ((ctx		(make-<base91-encode-ctx>))
	 (src		(if (string? binary) (string->utf8 binary) binary))
	 (src-len	(bytevector-length src))
	 (dst		(make-bytevector (base91-encode-length src-len) 0)))
    (receive (dst-next src-next)
	(base91-encode-update! ctx dst 0 src 0 src-len)
      (receive (result dst-next src-next)
	  (base91-encode-final! ctx dst dst-next src src-next src-len)
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
  (let* ((ctx		(make-<base91-decode-ctx>))
	 (src		(if (string? binary)
			    (string->utf8 binary)
			  binary))
	 (src-len	(bytevector-length src))
	 (dst		(make-bytevector (base91-decode-length src-len) 0)))
    (receive (result dst-next src-next)
	(base91-decode-update! ctx dst 0 src 0 src-len)
      (if result
	  (list result (%output dst dst-next))
	(receive (result dst-next src-next)
	    (base91-decode-final! ctx dst dst-next src src-next src-len)
	  (list result (%output dst dst-next)))))))


(parametrise ((check-test-name	'one)
	      (debugging	#t))

  (let ((a "") (b ""))
    (check (encode a)		=> `(#t ,b))
    (check (decode b #t)	=> `(#t ,a)))

  (let ((a "\x0;") (b "AA"))
    (check (encode a)		=> `(#t ,b))
    (check (decode b #t)	=> `(#t ,a)))

  (let ((a "\x0;\x0;") (b "AAA"))
    (check (encode a)		=> `(#t ,b))
    (check (decode b #t)	=> `(#t ,a)))

  (let ((a "\x0;\x0;\x0;") (b "AAAA"))
    (check (encode a)		=> `(#t ,b))
    (check (decode b #t)	=> `(#t ,a)))

  (let ((a "\x0;\x0;\x0;\x0;") (b "AAAAA"))
    (check (encode a)		=> `(#t ,b))
    (check (decode b #t)	=> `(#t ,a)))

  (let ((a "h") (b "NB"))
    (check (encode a)		=> `(#t ,b))
    (check (decode b #t)	=> `(#t ,a)))

  (let ((a "he") (b "TPD"))
    (check (encode a)		=> `(#t ,b))
    (check (decode b #t)	=> `(#t ,a)))

  (let ((a "hel") (b "TPwJ"))
    (check (encode a)		=> `(#t ,b))
    (check (decode b #t)	=> `(#t ,a)))

  (let ((a "hell") (b "TPwJb"))
    (check (encode a)		=> `(#t ,b))
    (check (decode b #t)	=> `(#t ,a)))

  (let ((a "hello") (b "TPwJh>A"))
    (check (encode a)		=> `(#t ,b))
    (check (decode b #t)	=> `(#t ,a)))

  (let ((a "ciao") (b "laH<b"))
    (check (encode a)		=> `(#t ,b))
    (check (decode b #t)	=> `(#t ,a)))

  (let ((a "Le Poete est semblable au prince des nuees Qui hante la tempete e se rit de l'archer; Exile sul le sol au milieu des huees, Ses ailes de geant l'empechent de marcher.")
  	(b "]O=Ca>&Y<RU0HylLeP52U<jNG$ztw8lL?zZ2_*UC4!a.`)apQPNKF5T1#F\"u>x9jKizI9[h;+$#/pEzj/1^IY@:Y$F20nu>N$z(Id,k$9yN?6x8j/1dK/Wue$y(&*EOiaBJgW<oe6U:yTmlLU8mfO[}A:dO[eGwS60GFTjKB0ft)_Y$Fp3$8wnyJ!f6=yC4!ztEQUo2X_18gA"))
    (check (encode a)		=> `(#t ,b))
    (check (decode b #t)	=> `(#t ,a)))

  #t)


;;;; done

(check-report)

;;; end of file
