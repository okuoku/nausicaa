;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for ascii armor of bytevectors
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
  (armor ascii85)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing ASCII armor ascii85\n")


;;;; helpers

(define (subbytevector src start past)
  (let ((dst (make-bytevector (- past start))))
    (do ((i 0 (+ 1 i))
	 (j start (+ 1 j)))
	((= j past)
	 dst)
      (bytevector-u8-set! dst i (bytevector-u8-ref src j)))))


;;;; ascii85 encoding and decoding routines

(define (encode binary)
  ;;Encode BINARY which  must be a Scheme string  or bytevector.  Return
  ;;two values:  (1) the resulting boolean from  the encoding functions,
  ;;(2) a string representing the encoded data.
  ;;
  (let* ((ctx		(make-<ascii85-encode-ctx>))
	 (src		(if (string? binary) (string->utf8 binary) binary))
	 (src-len	(bytevector-length src))
	 (dst		(make-bytevector (ascii85-encode-length src-len) 0)))
    (receive (dst-next src-next)
	(ascii85-encode-update! ctx dst 0 src 0 src-len)
      (receive (result dst-next src-next)
	  (ascii85-encode-final! ctx dst dst-next src src-next src-len)
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
  (let* ((ctx		(make-<ascii85-decode-ctx>))
	 (src		(if (string? binary)
			    (string->utf8 binary)
			  binary))
	 (src-len	(bytevector-length src))
	 (dst		(make-bytevector (ascii85-decode-length src-len) 0)))
    (receive (result dst-next src-next)
	(ascii85-decode-update! ctx dst 0 src 0 src-len)
      (if result
	  (list result (%output dst dst-next))
	(receive (result dst-next src-next)
	    (ascii85-decode-final! ctx dst dst-next src src-next src-len)
	  (list result (%output dst dst-next)))))))


(parametrise ((check-test-name	'notilde-ascii85)
	      (debugging	#t))

;;;Test vectors were generated with the web-utility at:
;;;
;;;  <http://www.webutils.pl/index.php?idx=ascii85>
;;;
;;;see also the Wikipedia page.

  (let ((a "") (b ""))
    (check (encode a)		=> `(#t ,b))
    (check (decode b #t)	=> `(#t ,a)))

  (let ((a "\x0;") (b "!!"))
    (check (encode a)		=> `(#t ,b))
    (check (decode b #t)	=> `(#t ,a)))

  (let ((a "\x0;\x0;") (b "!!!"))
    (check (encode a)		=> `(#t ,b))
    (check (decode b #t)	=> `(#t ,a)))

  (let ((a "\x0;\x0;\x0;") (b "!!!!"))
    (check (encode a)		=> `(#t ,b))
    (check (decode b #t)	=> `(#t ,a)))

  (let ((a "\x0;\x0;\x0;\x0;") (b "z"))
    (check (encode a)		=> `(#t ,b))
    (check (decode b #t)	=> `(#t ,a)))

  (let ((a "h") (b "BE"))
    (check (encode a)		=> `(#t ,b))
    (check (decode b #t)	=> `(#t ,a)))

  (let ((a "he") (b "BOq"))
    (check (encode a)		=> `(#t ,b))
    (check (decode b #t)	=> `(#t ,a)))

  (let ((a "hel") (b "BOtu"))
    (check (encode a)		=> `(#t ,b))
    (check (decode b #t)	=> `(#t ,a)))

  (let ((a "hell") (b "BOu!r"))
    (check (encode a)		=> `(#t ,b))
    (check (decode b #t)	=> `(#t ,a)))

  (let ((a "hello") (b "BOu!rDZ"))
    (check (encode a)		=> `(#t ,b))
    (check (decode b #t)	=> `(#t ,a)))

  (let ((a "ciao") (b "@qf@i"))
    (check (encode a)		=> `(#t ,b))
    (check (decode b #t)	=> `(#t ,a)))

  (let ((a "Le Poete est semblable au prince des nuees Qui hante la tempete e se rit de l'archer; Exile sul le sol au milieu des huees, Ses ailes de geant l'empechent de marcher.")
  	(b "9P#>CDe4$%+D#V9+EM+2@VfI^Ch4_tFWbXDBl7El+Co&)+Du=5ATJtkF_Mt3@;^0u+DbI/FCf<.ATVK+AKZ&*+ED1<+Co%+CaWY3@q]Fo4!6t:Bl%?'F*2:ACh4`1DepP)FWbO8Ch[I'+Co&)+D>n/ATKCF;e:\"m@;0OhF!,\")+D57oDKI\";-Y7.6ARfCbDKI\"3AKYhuEarcoE\\7"))
    (check (encode a)		=> `(#t ,b))
    (check (decode b #t)	=> `(#t ,a)))

  #t)


(parametrise ((check-test-name	'ascii85)
	      (debugging	#t))

;;;Test vectors were generated with the web-utility at:
;;;
;;;  <http://www.webutils.pl/index.php?idx=ascii85>
;;;
;;;see also the Wikipedia page.

  ;; (let ((a "") (b "<~~>"))
  ;;   (check (encode a)		=> b)
  ;;   (check (decode b #t)	=> a))

  ;; (let ((a "\x0;") (b "<~!!~>"))
  ;;   (check (encode a)	=> b)
  ;;   (check (decode b #t)	=> a))

  ;; (let ((a "\x0;\x0;") (b "<~!!!~>"))
  ;;   (check (encode a)	=> b)
  ;;   (check (decode b #t)	=> a))

  ;; (let ((a "\x0;\x0;\x0;") (b "<~!!!!~>"))
  ;;   (check (encode a)	=> b)
  ;;   (check (decode b #t)	=> a))

  ;; (let ((a "\x0;\x0;\x0;\x0;") (b "<~z~>"))
  ;;   (check (encode a)	=> b)
  ;;   (check (decode b #t)	=> a))

  ;; (let ((a "h") (b "<~BE~>"))
  ;;   (check (encode a)	=> b)
  ;;   (check (decode b #t)	=> a))

  ;; (let ((a "he") (b "<~BOq~>"))
  ;;   (check (encode a)	=> b)
  ;;   (check (decode b #t)	=> a))

  ;; (let ((a "hel") (b "<~BOtu~>"))
  ;;   (check (encode a)	=> b)
  ;;   (check (decode b #t)	=> a))

  ;; (let ((a "hell") (b "<~BOu!r~>"))
  ;;   (check (encode a)	=> b)
  ;;   (check (decode b #t)	=> a))

  ;; (let ((a "hello") (b "<~BOu!rDZ~>"))
  ;;   (check (encode a)	=> b)
  ;;   (check (decode b #t)	=> a))

  ;; (let ((a "ciao") (b "<~@qf@i~>"))
  ;;   (check (encode a)	=> b)
  ;;   (check (decode b #t)	=> a))

  ;; (let ((a "Le Poete est semblable au prince des nuees Qui hante la tempete e se rit de l'archer; Exile sul le sol au milieu des huees, Ses ailes de geant l'empechent de marcher.")
  ;; 	(b "<~9P#>CDe4$%+D#V9+EM+2@VfI^Ch4_tFWbXDBl7El+Co&)+Du=5ATJtkF_Mt3@;^0u+DbI/FCf<.ATVK+AKZ&*+ED1<+Co%+CaWY3@q]Fo4!6t:Bl%?'F*2:ACh4`1DepP)FWbO8Ch[I'+Co&)+D>n/ATKCF;e:\"m@;0OhF!,\")+D57oDKI\";-Y7.6ARfCbDKI\"3AKYhuEarcoE\\7~>"))
  ;;   (check (encode a)	=> b)
  ;;   (check (decode b #t)	=> a))

  #t)


(parametrise ((check-test-name	'misc))

  (check
      (let ((bv (make-bytevector 4 0)))
	(list (ascii85-encode-opening! (make-<ascii85-encode-ctx>) bv 1)
	      bv))
    => '(3 #vu8(0 60 126 0)))

  (check
      (let ((bv (make-bytevector 4 0)))
	(ascii85-encode-opening! (make-<ascii85-encode-ctx>) bv 3))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let ((bv (make-bytevector 4 0)))
	(list (ascii85-encode-closing! (make-<ascii85-encode-ctx>) bv 1)
	      bv))
    => '(3 #vu8(0 126 62 0)))

  (check
      (let ((bv (make-bytevector 4 0)))
	(ascii85-encode-closing! (make-<ascii85-encode-ctx>) bv 3))
    => #f)

  #t)


;;;; done

(check-report)

;;; end of file
