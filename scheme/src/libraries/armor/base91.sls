;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: ASCII armor base91
;;;Date: Tue Jan 26, 2010
;;;
;;;Abstract
;;;
;;;	This  is an  implementation  of base91  encoding.  The  encoding
;;;	specification  is by Joachim  Henke <j-o@users.sourceforge.net>,
;;;	published in the basE91 C language software package; the code in
;;;	this library is derived from the one in basE91.
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

;;;Original C language license terms
;;;---------------------------------
;;;
;;;Copyright (c) 2000-2005 Joachim Henke <j-o@users.sourceforge.net>
;;;
;;;This  software is provided  'as-is', without  any express  or implied
;;;warranty. In no event will the authors be held liable for any damages
;;;arising from the use of this software.
;;;
;;;Permission is granted to anyone to use this software for any purpose,
;;;including commercial  applications, and to alter  it and redistribute
;;;it freely, subject to the following restrictions:
;;;
;;;1. The origin  of this software must not  be misrepresented; you must
;;;   not claim  that you wrote the  original software. If  you use this
;;;   software  in   a  product,   an  acknowledgment  in   the  product
;;;   documentation would be appreciated but is not required.
;;;
;;;2. Altered source  versions must be plainly marked  as such, and must
;;;   not be misrepresented as being the original software.
;;;
;;;3. This  notice  may not  be  removed  or  altered from  any  source
;;;   distribution.


(library (armor base91)
  (export

    <base91-encode-ctx>				<base91-decode-ctx>
    make-<base91-encode-ctx>			make-<base91-decode-ctx>
    <base91-encode-ctx>?			<base91-decode-ctx>?

    base91-encode-update!			base91-decode-update!
    base91-encode-final!			base91-decode-final!
    base91-encode-flushed?			base91-decode-flushed?

    base91-encode-length			base91-decode-length
    base91-encode-update-length			base91-decode-update-length
    base91-encode-final-length			base91-decode-final-length

    armored-byte-of-base91?)
  (import (rnrs)
    (language-extensions)
    (classes)
    (armor conditions))


(define-class <base91-encode-ctx>
  (nongenerative nausicaa:armor:base91:<base91-encode-ctx>)
  (fields (mutable queue-length)
	  (mutable bit-queue))
  (protocol (lambda (make-<top>)
	      (lambda ()
		((make-<top>) 0 0)))))

(define-class <base91-decode-ctx>
  (nongenerative nausicaa:armor:base91:<base91-decode-ctx>)
  (fields (mutable queue-length)
	  (mutable bit-queue)
	  (mutable val))
  (protocol (lambda (make-<top>)
	      (lambda ()
		((make-<top>) 0 0 #f)))))


;;;; encoding tables

;;The following is the encoding table for base91 as specified by Joachim
;;Henke.

(define encode-alphabet-base91
  '#(#\A #\B #\C   #\D #\E #\F   #\G #\H #\I   #\J    ;;  0 -  9
     #\K #\L #\M   #\N #\O #\P   #\Q #\R #\S   #\T    ;; 10 - 19
     #\U #\V #\W   #\X #\Y #\Z   #\a #\b #\c   #\d    ;; 20 - 29
     #\e #\f #\g   #\h #\i #\j   #\k #\l #\m   #\n    ;; 30 - 39
     #\o #\p #\q   #\r #\s #\t   #\u #\v #\w   #\x    ;; 40 - 49
     #\y #\z #\0   #\1 #\2 #\3   #\4 #\5 #\6   #\7    ;; 50 - 59
     #\8 #\9 #\!   #\# #\$ #\%   #\& #\( #\)   #\*    ;; 60 - 69
     #\+ #\, #\.   #\/ #\: #\;   #\< #\= #\>   #\?    ;; 70 - 79
     #\@ #\[ #\]   #\^ #\_ #\`   #\{ #\| #\}   #\~    ;; 80 - 89
     #\"))                                            ;; 90

(define encode-table-base91
  (vector-map char->integer encode-alphabet-base91))


;;;; decoding tables

(define (alphabet->table! alphabet table)
  (dotimes (i (vector-length alphabet) table)
    (vector-set! table (char->integer (vector-ref alphabet i)) i)))

;;Decoding tables for base 91  as specified by Joachim Henke: disallowed
;;input characters  have a  value of #f;  only 128 chars,  as everything
;;above 127 (#x80) is #f.

(define decode-table-base91
  (alphabet->table! encode-alphabet-base91 (make-vector 128 #f)))

(define (armored-byte-of-base91? byte)
  (and (integer? byte)
       (exact? byte)
       (<= 0 byte 127)
       (vector-ref decode-table-base91 byte)))

;; (define decode-table
;;   '#(#xff #xff #xff #xff #xff #xff #xff #xff #xff #xff
;; 	  #xff #xff #xff #xff #xff #xff #xff #xff #xff #xff
;; 	  #xff #xff #xff #xff #xff #xff #xff #xff #xff #xff
;; 	  #xff #xff #xff #x3e #x5a #x3f #x40 #x41 #x42 #xff
;; 	  #x43 #x44 #x45 #x46 #x47 #xff #x48 #x49 #x34 #x35
;; 	  #x36 #x37 #x38 #x39 #x3a #x3b #x3c #x3d #x4a #x4b
;; 	  #x4c #x4d #x4e #x4f #x50 #x00 #x01 #x02 #x03 #x04
;; 	  #x05 #x06 #x07 #x08 #x09 #x0a #x0b #x0c #x0d #x0e
;; 	  #x0f #x10 #x11 #x12 #x13 #x14 #x15 #x16 #x17 #x18
;; 	  #x19 #x51 #xff #x52 #x53 #x54 #x55 #x1a #x1b #x1c
;; 	  #x1d #x1e #x1f #x20 #x21 #x22 #x23 #x24 #x25 #x26
;; 	  #x27 #x28 #x29 #x2a #x2b #x2c #x2d #x2e #x2f #x30
;; 	  #x31 #x32 #x33 #x56 #x57 #x58 #x59 #xff #xff #xff
;; 	  #xff #xff #xff #xff #xff #xff #xff #xff #xff #xff
;; 	  #xff #xff #xff #xff #xff #xff #xff #xff #xff #xff
;; 	  #xff #xff #xff #xff #xff #xff #xff #xff #xff #xff
;; 	  #xff #xff #xff #xff #xff #xff #xff #xff #xff #xff
;; 	  #xff #xff #xff #xff #xff #xff #xff #xff #xff #xff
;; 	  #xff #xff #xff #xff #xff #xff #xff #xff #xff #xff
;; 	  #xff #xff #xff #xff #xff #xff #xff #xff #xff #xff
;; 	  #xff #xff #xff #xff #xff #xff #xff #xff #xff #xff
;; 	  #xff #xff #xff #xff #xff #xff #xff #xff #xff #xff
;; 	  #xff #xff #xff #xff #xff #xff #xff #xff #xff #xff
;; 	  #xff #xff #xff #xff #xff #xff #xff #xff #xff #xff
;; 	  #xff #xff #xff #xff #xff #xff #xff #xff #xff #xff
;; 	  #xff #xff #xff #xff #xff #xff))


;;;; helpers

(define-inline (lower8 ?number)
  (bitwise-and #xFF ?number))

(define-inline (lower13 ?number)
  (bitwise-and #x1FFF ?number))

(define-inline (lower14 ?number)
  (bitwise-and #x3FFF ?number))

(define-inline (lower24 ?number)
  (bitwise-and #xFFFFFF ?number))

(define-inline (enqueue-byte! bit-queue queue-length byte)
  (set! bit-queue (bitwise-ior bit-queue (<< byte queue-length)))
  (incr! queue-length 8))

(define-inline (dequeue-8-bits! bit-queue queue-length)
  (set! bit-queue (>> bit-queue 8))
  (decr! queue-length 8))

(define-inline (dequeue-13-bits! bit-queue queue-length)
  (set! bit-queue (>> bit-queue 13))
  (decr! queue-length 13))

(define-inline (dequeue-14-bits! bit-queue queue-length)
  (set! bit-queue (>> bit-queue 14))
  (decr! queue-length 14))

(define << bitwise-arithmetic-shift-left)
(define >> bitwise-arithmetic-shift-right)

(define (div1 a b)
  (let ((r (div a b)))
    (if (zero? r) 1 r)))

(define (>>1 a b)
  (let ((r (>> a b)))
    (if (zero? r) 1 r)))

(define (>>3 a b)
  (let ((r (>> a b)))
    (if (< r 3) 3 r)))


;;;; encoded output length estimation
;;
;;Every 13 or 14 bits of binary data, 16 bits of ASCII characters.
;;

(define (base91-encode-update-length len)
  ;;Return the  minimum number  of bytes required  in the  output vector
  ;;when BASE91-ENCODE-UPDATE!  is applied  to LEN bytes of binary input
  ;;data.  We assume the worst case  in which we output 16 bits for each
  ;;13 bits of input.
  ;;
  ;;  (div (* 16 (div (* 8 len) 13)) 8) = (* 2 (div (* 8 len) 13))
  ;;
  (<< (div1 (<< len 3) 13) 1))

(define (base91-encode-final-length len)
  ;;Return the  minimum number  of bytes required  in the  output vector
  ;;when BASE91-ENCODE-FINAL! is applied to an encoding context to flush
  ;;the buffered  bits.  LEN is ignored:  it is here only  to provide an
  ;;API equal to the one of base64.
  ;;
  2)

(define (base91-encode-length len)
  ;;Return the  minimum number  of bytes required  in the  output vector
  ;;when BASE91-ENCODE-FINAL!   is applied to LEN bytes  of binary input
  ;;data.    This    is   also    the   number   required    when   both
  ;;BASE91-ENCODE-UPDATE! and BASE91-ENCODE-FINAL! are used.
  ;;
  (+ (base91-encode-update-length len)
     (base91-encode-final-length #f)))


;;;; decoded output length estimation
;;
;;Every 13 or 14 bits of binary data, 16 bits of ASCII characters.
;;

(define (base91-decode-update-length len)
  ;;Return the  minimum number  of bytes required  in the  output vector
  ;;when BASE91-DECODE-UPDATE!   is applied  to LEN ASCII  characters of
  ;;input.  We assume the worst case in which we output 14 bits for each
  ;;16 bits of input; we need at least 3 bytes.
  ;;
  ;;  (div (* 14 (div (* 8 len) 16)) 8) = (div (* 14 (div len 2)) 8)
  ;;
  (>>3 (* 14 (>>1 len 1)) 3))

(define (base91-decode-final-length len)
  ;;Return the  minimum number  of bytes required  in the  output vector
  ;;when BASE91-DECODE-FINAL! is applied  to a decoding context to flush
  ;;the buffered  bits.  LEN is ignored:  it is here only  to provide an
  ;;API equal to the one of base64.
  ;;
  1)

(define (base91-decode-length len)
  ;;Return the  minimum number  of bytes required  in the  output vector
  ;;when  BASE91-DECODE-FINAL!  is  applied to  LEN ASCII  characters of
  ;;input.    This    is   also   the   number    required   when   both
  ;;BASE91-DECODE-UPDATE! and BASE91-DECODE-FINAL! are used.
  ;;
  (+ (base91-decode-update-length len)
     (+ 3 (base91-decode-final-length #f))))


(define (base91-encode-flushed? ctx)
  (zero? (<base91-encode-ctx>-queue-length ctx)))

(define (base91-decode-flushed? ctx)
  (not (<base91-decode-ctx>-val ctx)))


(define (base91-encode-update! ctx dst-bv dst-start src-bv src-start src-past)
  ;;Encode  binary data  from the  bytevector SRC-BV  starting  at index
  ;;SRC-START (included)  up to  index SRC-PAST (excluded);  store ASCII
  ;;characters   in  the   bytevector  DST-BV   starting   at  DST-START
  ;;(included); encoding is performed  according to the specification in
  ;;the context CTX.
  ;;
  ;;Return two values:
  ;;
  ;;(1) The index  of the next non-written byte in  DST-BV; if DST-BV is
  ;;left full, this value is the length of DST-BV.
  ;;
  ;;(2) The index of the next  non-read byte in SRC-BV; if all the bytes
  ;;from SRC-BV are consumed, this value is SRC-PAST.
  ;;

  (define bit-queue
    (<base91-encode-ctx>-bit-queue ctx))
  (define queue-length
    (<base91-encode-ctx>-queue-length ctx))

  (let loop ((i		dst-start)
	     (j		src-start)
	     (src-len	(- src-past src-start))
	     (dst-len	(- (bytevector-length dst-bv) dst-start)))

    (define-inline (*src)
      (begin0
	  (bytevector-u8-ref src-bv j)
	(incr! j)))

    (define-inline (*dst ?dummy ?expr)
      (bytevector-u8-set! dst-bv i (vector-ref encode-table-base91 ?expr))
      (incr! i))

    (if (or (zero? src-len) (< dst-len 2))
	(begin
	  (<base91-encode-ctx>-bit-queue-set!    ctx bit-queue)
	  (<base91-encode-ctx>-queue-length-set! ctx queue-length)
	  (values i j))
      (let ((byte (*src)))
	(enqueue-byte! bit-queue queue-length byte)
	(if (< 13 queue-length)
	    (let ((v (lower13 bit-queue)))
	      (if (< 88 v)
		  (dequeue-13-bits! bit-queue queue-length)
		(begin
		  (set! v (lower14 bit-queue))
		  (dequeue-14-bits! bit-queue queue-length)))
	      (*dst 0 (mod v 91))
	      (*dst 1 (div v 91))
	      (loop i j (- src-len 1) (- dst-len 2)))
	  (loop i j (- src-len 1) dst-len))))))


(define (base91-encode-final! ctx dst-bv dst-start src-bv src-start src-past)
  ;;Encode  binary data  from the  bytevector SRC-BV  starting  at index
  ;;SRC-START (included)  up to  index SRC-PAST (excluded);  store ASCII
  ;;characters   in  the   bytevector  DST-BV   starting   at  DST-START
  ;;(included); encoding is performed  according to the specification in
  ;;the context CTX.
  ;;
  ;;Return three values:
  ;;
  ;;(1) A boolean:  true if success, false if  the output bytevector was
  ;;filled before flushing all the bytes.
  ;;
  ;;(2) The index  of the next non-written byte in  DST-BV; if DST-BV is
  ;;left full, this value is the length of DST-BV.
  ;;
  ;;(3) The index of the next  non-read byte in SRC-BV; if all the bytes
  ;;from SRC-BV are consumed, this value is SRC-PAST.
  ;;

  (define bit-queue
    (<base91-encode-ctx>-bit-queue ctx))
  (define queue-length
    (<base91-encode-ctx>-queue-length ctx))

  (let loop ((i		dst-start)
	     (j		src-start)
	     (src-len	(- src-past src-start))
	     (dst-len	(- (bytevector-length dst-bv) dst-start)))

    (define-inline (*src)
      (begin0
	  (bytevector-u8-ref src-bv j)
	(incr! j)))

    (define-inline (*dst ?dummy ?expr)
      (bytevector-u8-set! dst-bv i (vector-ref encode-table-base91 ?expr))
      (incr! i))

    (cond ((zero? src-len)
	   ;;Flush the enqueued bits and finish.
	   (if (zero? queue-length)
	       (values #t i j)
	     (let* ((test		(or (< 7 queue-length) (< 90 bit-queue)))
		    (required-dst-len	(if test 2 1)))
	       (if (< dst-len required-dst-len)
		   ;;Not enough room in the output buffer.
		   (begin
		     (<base91-encode-ctx>-bit-queue-set!    ctx bit-queue)
		     (<base91-encode-ctx>-queue-length-set! ctx queue-length)
		     (values #f i j))
		 (begin
		   (*dst 0 (mod bit-queue 91))
		   (when test
		     (*dst 1 (div bit-queue 91)))
		   (<base91-encode-ctx>-bit-queue-set!    ctx 0)
		   (<base91-encode-ctx>-queue-length-set! ctx 0)
		   (values #t i j))))))

	  ((< 2 dst-len)
	   ;;Consume one byte  and produce two bytes.  This  is the same
	   ;;as in BASE91-ENCODE-UPDATE!.
	   (let ((byte (*src)))
	     (enqueue-byte! bit-queue queue-length byte)
	     (if (< 13 queue-length)
		 (let ((v (lower13 bit-queue)))
		   (if (< 88 v)
		       (dequeue-13-bits! bit-queue queue-length)
		     (begin
		       (set! v (lower14 bit-queue))
		       (dequeue-14-bits! bit-queue queue-length)))
		   (*dst 0 (mod v 91))
		   (*dst 1 (div v 91))
		   (loop i j (- src-len 1) (- dst-len 2)))
	       (loop i j (- src-len 1) dst-len))))

	  (else
	   ;;Not enough room in the output buffer.
	   (<base91-encode-ctx>-bit-queue-set!    ctx bit-queue)
	   (<base91-encode-ctx>-queue-length-set! ctx queue-length)
	   (values #f i j)))))


(define (base91-decode-update! ctx dst-bv dst-start src-bv src-start src-past)
  ;;Decode ASCII characters from the bytevector SRC-BV starting at index
  ;;SRC-START (included)  up to index SRC-PAST  (excluded); store binary
  ;;data  bytes   in  the   bytevector  DST-BV  starting   at  DST-START
  ;;(included); decoding is performed  according to the specification in
  ;;the context CTX.
  ;;
  ;;Return three values:
  ;;
  ;;(1) A boolean  always false.  It is here to provide  an API equal to
  ;;the one of the base64 decoder.
  ;;
  ;;(2) The index  of the next non-written byte in  DST-BV; if DST-BV is
  ;;filled to the end, this value is the length of DST-BV.
  ;;
  ;;(3)  The index  of the  next  non-read byte  in SRC-BV;  if all  the
  ;;characters from SRC-BV are consumed, this value is SRC-PAST.
  ;;

  (define bit-queue
    (<base91-decode-ctx>-bit-queue ctx))
  (define queue-length
    (<base91-decode-ctx>-queue-length ctx))
  (define val
    (<base91-decode-ctx>-val ctx))

  (define (%error-invalid-input-byte byte)
    (raise
     (condition (make-armor-invalid-input-byte-condition)
		(make-who-condition 'base91-decode-update!)
		(make-message-condition "invalid input byte while decoding base91 bytevector")
		(make-irritants-condition byte))))

  (define (%decode char)
    (if (zero? (bitwise-and #x80 char))
	(let ((byte (vector-ref decode-table-base91 char)))
	  (or byte (%error-invalid-input-byte char)))
      (%error-invalid-input-byte char)))

  (let loop ((i		dst-start)
	     (j		src-start)
	     (src-len	(- src-past src-start))
	     (dst-len	(- (bytevector-length dst-bv) dst-start)))

    (define-inline (*src)
      (begin0
	  (bytevector-u8-ref src-bv j)
	(incr! j)))

    (define-inline (*dst ?expr)
      (bytevector-u8-set! dst-bv i (lower8 ?expr))
      (incr! i))

    (if (or (zero? src-len) (< dst-len 3))
	(begin
	  (<base91-decode-ctx>-bit-queue-set!    ctx bit-queue)
	  (<base91-decode-ctx>-queue-length-set! ctx queue-length)
	  (<base91-decode-ctx>-val-set!          ctx val)
	  (values #f i j))

      (let ((byte (%decode (*src))))
	(cond ((not val) ;start next value
	       (set! val byte)
	       (loop i j (- src-len 1) dst-len))

	      (else
	       (let ((dst-count 0))
		 (incr! val (* 91 byte))
		 (set! bit-queue (lower24 (bitwise-ior bit-queue (<< val queue-length))))
		 (incr! queue-length (if (< 88 (lower13 val))
					 13
				       14))
		 (while (< 7 queue-length)
		   (*dst bit-queue)
		   (incr! dst-count)
		   (dequeue-8-bits! bit-queue queue-length))
		 (set! val #f)
		 (loop i j (- src-len 1) (- dst-len dst-count)))))))))


(define (base91-decode-final! ctx dst-bv dst-start src-bv src-start src-past)
  ;;Decode ASCII characters from the bytevector SRC-BV starting at index
  ;;SRC-START (included)  up to index SRC-PAST  (excluded); store binary
  ;;data  bytes   in  the   bytevector  DST-BV  starting   at  DST-START
  ;;(included); decoding is performed  according to the specification in
  ;;the context CTX.
  ;;
  ;;Return three values:
  ;;
  ;;(1) A boolean:  true if success, false if  the output bytevector was
  ;;filled before flushing all the bytes.
  ;;
  ;;(2) The index  of the next non-written byte in  DST-BV; if DST-BV is
  ;;filled to the end, this value is the length of DST-BV.
  ;;
  ;;(3) The index of the next  non-read byte in SRC-BV; if all the bytes
  ;;from SRC-BV are consumed, this value is SRC-PAST.
  ;;

  (define bit-queue
    (<base91-decode-ctx>-bit-queue ctx))
  (define queue-length
    (<base91-decode-ctx>-queue-length ctx))
  (define val
    (<base91-decode-ctx>-val ctx))

  (define who 'base91-decode-final!)

  (define (%error-invalid-input-length)
    (raise
     (condition (make-armor-invalid-input-length-condition)
		(make-who-condition who)
		(make-message-condition "invalid input length while decoding base91 bytevector"))))

  (define (%error-invalid-input-byte byte)
    (raise
     (condition (make-armor-invalid-input-byte-condition)
		(make-who-condition who)
		(make-message-condition "invalid input byte while decoding base91 bytevector")
		(make-irritants-condition byte))))

  (let loop ((i		dst-start)
	     (j		src-start)
	     (src-len	(- src-past src-start))
	     (dst-len	(- (bytevector-length dst-bv) dst-start)))

    (define-inline (*src)
      (let ((char (bytevector-u8-ref src-bv j)))
	(if (zero? (bitwise-and #x80 char))
	    (begin0-let ((byte (vector-ref decode-table-base91 char)))
	      (if byte
		  (incr! j)
		(%error-invalid-input-byte char)))
	  (%error-invalid-input-byte char))))

    (define-inline (*dst ?expr)
      (bytevector-u8-set! dst-bv i (lower8 ?expr))
      (incr! i))

    (cond ((zero? src-len)
	   (cond ((not val)
		  (values #t i j))
		 ((zero? dst-len)
		  (<base91-decode-ctx>-bit-queue-set!    ctx bit-queue)
		  (<base91-decode-ctx>-queue-length-set! ctx queue-length)
		  (<base91-decode-ctx>-val-set!          ctx val)
		  (values #f i j))
		 (else
		  (*dst (bitwise-ior bit-queue (<< val queue-length)))
		  (<base91-decode-ctx>-val-set! ctx #f)
		  (values #t i j))))

	  ((< dst-len 3)
	   (<base91-decode-ctx>-bit-queue-set!    ctx bit-queue)
	   (<base91-decode-ctx>-queue-length-set! ctx queue-length)
	   (<base91-decode-ctx>-val-set!          ctx val)
	   (values #f i j))

	  (else
	   ;;The following is like BASE91-DECODE-UPDATE!.
	   (let ((byte (*src)))
	     (cond ((not val) ;start next value
		    (set! val byte)
		    (loop i j (- src-len 1) dst-len))

		   (else
		    (let ((dst-count 0))
		      (incr! val (* 91 byte))
		      (set! bit-queue (lower24 (bitwise-ior bit-queue (<< val queue-length))))
		      (incr! queue-length (if (< 88 (lower13 val))
					      13
					    14))
		      (while (< 7 queue-length)
			(*dst bit-queue)
			(incr! dst-count)
			(dequeue-8-bits! bit-queue queue-length))
		      (set! val #f)
		      (loop i j (- src-len 1) (- dst-len dst-count))))))))))


;;;; done

)

;;; end of file
