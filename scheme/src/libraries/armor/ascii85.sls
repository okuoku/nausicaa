;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: ascii armor with ascii85 format
;;;Date: Tue Jan 26, 2010
;;;
;;;Abstract
;;;
;;;	This  code is  a  Scheme swirling  of  C language  code from  an
;;;	ASCII85 distribution by Paul Haahr which claims that the code is
;;;	in   the  public  domain,   although  he   did  not   write  it.
;;;	http://www.webcom.com/~haahr/
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


(library (armor ascii85)
  (export

    <ascii85-encode-ctx>		<ascii85-decode-ctx>
    make-<ascii85-encode-ctx>		make-<ascii85-decode-ctx>
    <ascii85-encode-ctx>?		<ascii85-decode-ctx>?

    ascii85-encode-update!		ascii85-decode-update!
    ascii85-encode-final!		ascii85-decode-final!

    ascii85-encode-length		ascii85-decode-length
    ascii85-encode-update-length	ascii85-decode-update-length
    ascii85-encode-final-length		ascii85-decode-final-length

    ascii85-encode-block-length		ascii85-decode-block-length

    ascii85-encode-opening!
    ascii85-encode-closing!
    armored-byte-of-ascii85?)
  (import (rnrs)
    (armor conditions)
    (language-extensions))


(define-record-type <ascii85-encode-ctx>)
(define-record-type <ascii85-decode-ctx>)


(define less-than-char		(char->integer #\<))
(define greater-than-char	(char->integer #\>))
(define tilde-char		(char->integer #\~))
(define bang-char		(char->integer #\!))	;the smallest char in the alphabet
(define u-char			(char->integer #\u))	;the greatest char in the alphabet
(define z-char			(char->integer #\z))	;used to encode 4 zero bytes

(define table
  (vector (* 85 85 85 85)
	  (* 85 85 85)
	  (* 85 85)
	  85
	  1))

(define-syntax pow85
  (syntax-rules ()
    ((_ ?idx)
     (vector-ref table ?idx))))

;; (define blanks
;;   (map char->integer '(#\newline #\return #\tab #\vtab #\space #\page)))

(define << bitwise-arithmetic-shift-left)
(define >> bitwise-arithmetic-shift-right)

(define-inline (lower8 ?number)
  (bitwise-and #xFF ?number))


;;;; encoded output length estimation
;;
;;Every 4 bytes of binary data, 5 bytes of ASCII characters.
;;

(define ascii85-encode-block-length 4)

(define (ascii85-encode-update-length len)
  ;;Return the minimum number of bytes required in the output bytevector
  ;;when ASCII85-ENCODE-UPDATE!  is applied to LEN bytes of binary input
  ;;data.  Notice that the function will consume input in 4-bytes blocks
  ;;only and produce output in  5-bytes blocks only, so the return value
  ;;is always zero or an exact multiple of 5.
  ;;
  ;; (* 5 (div len 4))
  ;;
  (if (<= ascii85-encode-block-length len)
      (* ascii85-decode-block-length (>> len 2))
    0))

(define (ascii85-encode-final-length len)
  ;;Return the  minimum number  of bytes required  in the  output vector
  ;;when ASCII85-ENCODE-FINAL!   is applied to  LEN < 4 bytes  of binary
  ;;input  data.  The  returned value  does  NOT take  into account  the
  ;;ending "~>" sequence.  If LEN is invalid: the return value is #f.
  ;;
  (case len
    ((0)	0)
    ((1)	2)
    ((2)	3)
    ((3)	4)
    (else	#f)))

(define (ascii85-encode-length len)
  ;;Return the  minimum number  of bytes required  in the  output vector
  ;;when ASCII85-ENCODE-FINAL!   is applied to LEN bytes  of binary input
  ;;data.    This    is   also    the   number   required    when   both
  ;;ASCII85-ENCODE-UPDATE! and ASCII85-ENCODE-FINAL! are used.
  ;;
  (+ (ascii85-encode-update-length len)
     (ascii85-encode-final-length (mod len ascii85-encode-block-length))))


;;;; decoded output length estimation
;;
;;Every 4 bytes of binary data, 5 bytes of ASCII characters.
;;

(define ascii85-decode-block-length 5)

(define (ascii85-decode-update-length len)
  ;;Return the  minimum number  of bytes required  in the  output vector
  ;;when ASCII85-DECODE-UPDATE!   is applied to LEN  ASCII characters of
  ;;input.   Notice that  the  function will  consume  input in  5-bytes
  ;;blocks only and produce output in 4-bytes blocks only, so the return
  ;;value is always zero or an exact multiple of 4.
  ;;
  ;;This function  does NOT  take into account  that an  input character
  ;;could be #\z, corresponding to 4 output bytes.
  ;;
  (if (<= ascii85-decode-block-length len)
      (* ascii85-encode-block-length (div len ascii85-decode-block-length))
    0))

(define (ascii85-decode-final-length len)
  ;;Return the  minimum number  of bytes required  in the  output vector
  ;;when ASCII85-DECODE-FINAL!   is applied to LEN <  5 ASCII characters
  ;;of  input.  All  the arguments  between 0  and 4  are valid;  if the
  ;;argument is not valid the return value is #f.
  ;;
  (case len
    ((0)	0)
    ((1)	ascii85-encode-block-length)
    ((2)	1)
    ((3)	2)
    ((4)	3)
    (else	#f)))

(define (ascii85-decode-length len)
  ;;Return the  minimum number  of bytes required  in the  output vector
  ;;when ASCII85-DECODE-FINAL!   is applied  to LEN ASCII  characters of
  ;;input.    This    is   also   the   number    required   when   both
  ;;ASCII85-DECODE-UPDATE! and ASCII85-DECODE-FINAL! are used.
  ;;
  ;;This function  does NOT  take into account  that an  input character
  ;;could be #\z, corresponding to 4 output bytes.
  ;;
  (let ((final-len (ascii85-decode-final-length (mod len ascii85-decode-block-length))))
    (if final-len
	(+ final-len (ascii85-decode-update-length len))
      #f)))


(define (%encode dst-bv dst-start tuple count)
  ;;Encode the  bit-tuple TUPLE of COUNT bytes  storing ASCII characters
  ;;in  the bytevector  DST-BV starting  at index  DST-START (included).
  ;;DST-BV must have at least 5 bytes of room starting at DST-START.
  ;;
  ;;Return the number of characters written to DST-BV.
  ;;
  ;;(1) Compute the moduli:
  ;;
  ;;      TUPLE			M0 = TUPLE mod 85
  ;;	  D0 = TUPLE / 85	M1 = D0 mod 85
  ;;	  D1 = D0 / 85		M2 = D1 mod 85
  ;;	  D2 = D0 / 85		M3 = D2 mod 85
  ;;	  D3 = D0 / 85		M4 = D3 mod 85
  ;;
  ;;(2) Character integers are computed:
  ;;
  ;;      C0 = 33 + M0
  ;;      C1 = 33 + M1
  ;;      C2 = 33 + M2
  ;;      C3 = 33 + M3
  ;;      C4 = 33 + M4
  ;;
  ;;(3) Store the moduli in reverse order:
  ;;
  ;;      COUNT = 4	=>	#vu8(... C4 C3 C2 C1 C0 ...)
  ;;      COUNT = 3	=>	#vu8(... C4 C3 C2 C1 ...)
  ;;      COUNT = 2	=>	#vu8(... C4 C3 C2 ...)
  ;;      COUNT = 1	=>	#vu8(... C4 C3 ...)
  ;;                                     ^
  ;;                                 DST-START
  ;;
  ;;    notice that we MUST compute all the 5 moduli before storing them
  ;;    in DST-BV.
  ;;
  (let ((tuple (bytevector-u32-ref tuple 0 (endianness big))))
    (if (and (= ascii85-encode-block-length count) (zero? tuple))
	(begin
	  (bytevector-u8-set! dst-bv dst-start z-char)
	  1)
      (let ((moduli (let ((moduli (make-vector 5)))
		      (do ((k 0 (+ 1 k))
			   (tuple tuple (div tuple 85)))
			  ((= k 5)
			   moduli)
			(vector-set! moduli k (mod tuple 85))))))
	(do ((k 0 (+ 1 k))
	     (j 4 (- j 1))
	     (i dst-start (+ 1 i)))
	    ((< count k)
	     k)
	  (bytevector-u8-set! dst-bv i
			      ;; store moduli in reverse order
			      (+ bang-char (vector-ref moduli j))))))))


(define (ascii85-encode-opening! ctx dst-bv dst-start)
  ;;Writes  the opening sequence  to the  bytevector DST-BV  starting at
  ;;index  DST-START   (included).   Return   the  index  of   the  next
  ;;non-written byte  in DST-BV, or  #f if there  is not enough  room in
  ;;DST-BV.
  ;;
  (let ((dst-len (- (bytevector-length dst-bv) dst-start)))
    (if (< dst-len 2)
	#f
      (begin
	(bytevector-u8-set! dst-bv dst-start       less-than-char)
	(bytevector-u8-set! dst-bv (+ 1 dst-start) tilde-char)
	(+ 2 dst-start)))))

(define (ascii85-encode-closing! ctx dst-bv dst-start)
  ;;Writes  the closing sequence  to the  bytevector DST-BV  starting at
  ;;index  DST-START   (included).   Return   the  index  of   the  next
  ;;non-written byte  in DST-BV, or  #f if there  is not enough  room in
  ;;DST-BV.
  ;;
  (let ((dst-len (- (bytevector-length dst-bv) dst-start)))
    (if (< dst-len 2)
	#f
      (begin
	(bytevector-u8-set! dst-bv dst-start       tilde-char)
	(bytevector-u8-set! dst-bv (+ 1 dst-start) greater-than-char)
	(+ 2 dst-start)))))

(define (armored-byte-of-ascii85? int)
  (or (= int z-char)
      (<= bang-char int u-char)))


(define (ascii85-encode-update! ctx dst-bv dst-start src-bv src-start src-past)
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
  ;;Binary   data   bytes   are   read   from  SRC-BV   in   chunks   of
  ;;ASCII85-ENCODE-BLOCK-LENGTH  bytes; ASCII  characters are  written to
  ;;DST-BV in chunks of ASCII85-DECODE-BLOCK-LENGTH bytes.
  ;;

  (let loop ((i		dst-start)
	     (j		src-start)
	     (src-len	(- src-past src-start))
	     (dst-len	(- (bytevector-length dst-bv) dst-start)))
    (if (or (< src-len ascii85-encode-block-length)
	    (< dst-len ascii85-decode-block-length))
	(values i j)
      (let ((tuple (make-bytevector 4)))

	(define-inline (*src ?idx)
	  (bytevector-u8-ref src-bv ?idx))

	(define-inline (*tuple ?idx ?expr)
	  (bytevector-u8-set! tuple ?idx ?expr))

	(*tuple 0 (*src j))
	(let ((j (+ 1 j)))
	  (*tuple 1 (*src j))
	  (let ((j (+ 1 j)))
	    (*tuple 2 (*src j))
	    (let ((j (+ 1 j)))
	      (*tuple 3 (*src j))
	      (let ((delta (%encode dst-bv i tuple 4)))
		(loop (+ i delta) (+ 1 j)
		      (- src-len 4) (- dst-len delta))))))))))


(define (ascii85-encode-final! ctx dst-bv dst-start src-bv src-start src-past)
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
  ;;Binary   data   bytes   are   read   from  SRC-BV   in   chunks   of
  ;;ASCII85-ENCODE-BLOCK-LENGTH  bytes; ASCII  characters are  written to
  ;;DST-BV in  chunks of ASCII85-DECODE-BLOCK-LENGTH  bytes.  After that,
  ;;the  leftover binary  data bytes  are read  from SRC-BV  and encoded
  ;;ASCII characters are written to DST-BV.
  ;;

  (let loop ((i		dst-start)
	     (j		src-start)
	     (src-len	(- src-past src-start))
	     (dst-len	(- (bytevector-length dst-bv) dst-start)))

    (define-inline (*src)
      (begin0
	  (bytevector-u8-ref src-bv j)
	(incr! j)))

    (cond ((zero? src-len)
	   (values #t i j))

	  ((<= ascii85-encode-block-length src-len)
	   (when (< dst-len ascii85-decode-block-length)
	     (values #f i j))
	   (let ((tuple (make-bytevector 4)))

	     (define-inline (*tuple ?idx ?expr)
	       (bytevector-u8-set! tuple ?idx ?expr))

	     (*tuple 0 (*src))
	     (*tuple 1 (*src))
	     (*tuple 2 (*src))
	     (*tuple 3 (*src))
	     (let ((delta (%encode dst-bv i tuple 4)))
	       (loop (+ i delta) j
		     (- src-len 4) (- dst-len delta)))))

	  (else
	   (when (< dst-len (ascii85-encode-final-length src-len))
	     (values #f i j))
	   (let ((tuple (make-bytevector 4 0)))

	     (define-inline (*tuple ?idx ?expr)
	       (bytevector-u8-set! tuple ?idx ?expr))

	     (do ((k 0 (+ 1 k)))
		 ((= k src-len)
		  (values #t (+ i (%encode dst-bv i tuple k)) j))
	       (*tuple k (*src)))
	     )))))


(define (ascii85-decode-update! ctx dst-bv dst-start src-bv src-start src-past)
  ;;Decode ASCII characters from the bytevector SRC-BV starting at index
  ;;SRC-START (included)  up to index SRC-PAST  (excluded); store binary
  ;;data  bytes   in  the   bytevector  DST-BV  starting   at  DST-START
  ;;(included); decoding is performed  according to the specification in
  ;;the context CTX.
  ;;
  ;;Return three values:
  ;;
  ;;(1) A boolean  always false.  It is here only to  make the API equal
  ;;to the one of base64.
  ;;
  ;;(2) The index  of the next non-written byte in  DST-BV; if DST-BV is
  ;;filled to the end, this value is the length of DST-BV.
  ;;
  ;;(3)  The index  of the  next  non-read byte  in SRC-BV;  if all  the
  ;;characters from SRC-BV are consumed, this value is SRC-PAST.
  ;;
  ;;ASCII   characters    are   read   from   SRC-BV    in   chunks   of
  ;;ASCII85-DECODE-BLOCK-LENGTH bytes;  binary data bytes  are written to
  ;;DST-BV in chunks of ASCII85-ENCODE-BLOCK-LENGTH bytes.
  ;;

  (define who 'ascii85-decode-update!)

  (define (%error-invalid-input-byte byte)
    (raise
     (condition (make-armor-invalid-input-byte-condition)
		(make-who-condition who)
		(make-message-condition "invalid input byte while decoding ascii85 bytevector")
		(make-irritants-condition byte))))

  (define (%error-invalid-z-char . src-list)
    (raise
     (condition (make-armor-invalid-input-byte-condition)
		(make-who-condition who)
		(make-message-condition
		 "invalid z character inside 5-tuple while decoding ascii85 bytevector")
		(make-irritants-condition (string (map integer->char src-list))))))

  (define (%validate-byte byte)
    (when (or (< byte bang-char) (< u-char byte))
      (%error-invalid-input-byte byte)))

  (let loop ((i		dst-start)
	     (j		src-start)
	     (src-len	(- src-past src-start))
	     (dst-len	(- (bytevector-length dst-bv) dst-start)))

    (define-inline (*src)
      (begin0
	  (bytevector-u8-ref src-bv j)
	(incr! j)))

    (define-inline (*dst ?dummy ?expr)
      (bytevector-u8-set! dst-bv i (lower8 ?expr))
      (incr! i))

    (cond ((or (< src-len ascii85-decode-block-length)
	       (< dst-len ascii85-encode-block-length))
	   (values #f i j))

	  (else
	   (let ((src0 (*src)))
	     (if (= src0 z-char)
		 (begin
		   (*dst 0 0)
		   (*dst 1 0)
		   (*dst 2 0)
		   (*dst 3 0)
		   (loop i j (- src-len 1) (- dst-len 4)))
	       (let ((src1 (*src))
		     (src2 (*src))
		     (src3 (*src))
		     (src4 (*src)))
		 (when (or (= src1 z-char) (= src2 z-char)
			   (= src3 z-char) (= src4 z-char))
		   (%error-invalid-z-char src0 src1 src2 src3 src4))
		 (%validate-byte src0)
		 (%validate-byte src1)
		 (%validate-byte src2)
		 (%validate-byte src3)
		 (%validate-byte src4)
		 (let ((tuple (+ (* (- src0 bang-char) (pow85 0))
				 (* (- src1 bang-char) (pow85 1))
				 (* (- src2 bang-char) (pow85 2))
				 (* (- src3 bang-char) (pow85 3))
				 (- src4 bang-char))))
		   (bytevector-u32-set! dst-bv i tuple (endianness big))
		   (incr! i 4)
		   ;; (*dst 0 (>> tuple 24))
		   ;; (*dst 1 (>> tuple 16))
		   ;; (*dst 2 (>> tuple  8))
		   ;; (*dst 3     tuple)
		   (loop i j (- src-len 5) (- dst-len 4))))))))))


(define (ascii85-decode-final! ctx dst-bv dst-start src-bv src-start src-past)
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
  ;;(3)  The index  of the  next  non-read byte  in SRC-BV;  if all  the
  ;;characters from SRC-BV are consumed, this value is SRC-PAST.
  ;;
  ;;ASCII   characters    are   read   from   SRC-BV    in   chunks   of
  ;;ASCII85-DECODE-BLOCK-LENGTH bytes; binary  data bytes are written to
  ;;DST-BV in chunks  of ASCII85-ENCODE-BLOCK-LENGTH bytes.  After that,
  ;;the leftover input bytes are encoded.
  ;;

  (define who 'ascii85-decode-final!)

  (define (%error-invalid-input-length)
    (raise
     (condition (make-armor-invalid-input-length-condition)
		(make-who-condition who)
		(make-message-condition "invalid input length while decoding ascii85 bytevector"))))

  (define (%error-invalid-input-byte byte)
    (raise
     (condition (make-armor-invalid-input-byte-condition)
		(make-who-condition who)
		(make-message-condition "invalid input byte while decoding ascii85 bytevector")
		(make-irritants-condition byte))))

  (define (%error-invalid-z-char . src-list)
    (raise
     (condition (make-armor-invalid-input-byte-condition)
		(make-who-condition who)
		(make-message-condition
		 "invalid z character inside 5-tuple while decoding ascii85 bytevector")
		(make-irritants-condition (string (map integer->char src-list))))))

  (define (%validate-byte byte)
    (when (or (< byte bang-char) (< u-char byte))
      (%error-invalid-input-byte byte)))

  (let loop ((i		dst-start)
	     (j		src-start)
	     (src-len	(- src-past src-start))
	     (dst-len	(- (bytevector-length dst-bv) dst-start)))

    (define-inline (*src)
      (begin0
	  (bytevector-u8-ref src-bv j)
	(incr! j)))

    (define-inline (*dst ?dummy ?expr)
      (bytevector-u8-set! dst-bv i (lower8 ?expr))
      (incr! i))

    (cond ((zero? src-len)
	   (values #t i j))

	  ((>= src-len ascii85-decode-block-length)
	   (when (< dst-len ascii85-encode-block-length)
	     (values #f i j))
	   (let ((src0 (*src)))
	     (if (= src0 z-char)
		 (begin
		   (*dst 0 0)
		   (*dst 1 0)
		   (*dst 2 0)
		   (*dst 3 0)
		   (loop i j (- src-len 1) (- dst-len 4)))
	       (let ((src1 (*src))
		     (src2 (*src))
		     (src3 (*src))
		     (src4 (*src)))
		 (when (or (= src1 z-char) (= src2 z-char)
			   (= src3 z-char) (= src4 z-char))
		   (%error-invalid-z-char src0 src1 src2 src3 src4))
		 (%validate-byte src0)
		 (%validate-byte src1)
		 (%validate-byte src2)
		 (%validate-byte src3)
		 (%validate-byte src4)
		 (let ((tuple (+ (* (- src0 bang-char) (pow85 0))
				 (* (- src1 bang-char) (pow85 1))
				 (* (- src2 bang-char) (pow85 2))
				 (* (- src3 bang-char) (pow85 3))
				 (- src4 bang-char))))
		   (*dst 0 (>> tuple 24))
		   (*dst 1 (>> tuple 16))
		   (*dst 2 (>> tuple  8))
		   (*dst 3     tuple)
		   (loop i j (- src-len 5) (- dst-len 4)))))))

	  ((= src-len 4)
	   (when (< dst-len 3)
	    (values #f i j))
	   (let ((src0 (*src)))
	     (if (= src0 z-char)
		 (begin
		   (*dst 0 0)
		   (*dst 1 0)
		   (*dst 2 0)
		   (*dst 3 0)
		   (loop i j (- src-len 1) (- dst-len 4)))
	       (let ((src1 (*src))
		     (src2 (*src))
		     (src3 (*src)))
		 (when (or (= src1 z-char) (= src2 z-char)
			   (= src3 z-char))
		   (%error-invalid-z-char src0 src1 src2 src3))
		 (%validate-byte src0)
		 (%validate-byte src1)
		 (%validate-byte src2)
		 (%validate-byte src3)
		 (let ((tuple (+ (* (- src0 bang-char) (pow85 0))
				 (* (- src1 bang-char) (pow85 1))
				 (* (- src2 bang-char) (pow85 2))
				 (* (- src3 bang-char) (pow85 3))
				 (pow85 3))))
		   (*dst 0 (>> tuple 24))
		   (*dst 1 (>> tuple 16))
		   (*dst 2 (>> tuple  8))
		   (loop i j (- src-len 4) (- dst-len 3)))))))

	  ((= src-len 3)
	   (when (< dst-len 2)
	    (values #f i j))
	   (let ((src0 (*src)))
	     (if (= src0 z-char)
		 (begin
		   (*dst 0 0)
		   (*dst 1 0)
		   (*dst 2 0)
		   (*dst 3 0)
		   (loop i j (- src-len 1) (- dst-len 4)))
	       (let ((src1 (*src))
		     (src2 (*src)))
		 (when (or (= src1 z-char) (= src2 z-char))
		   (%error-invalid-z-char src0 src1 src2))
		 (%validate-byte src0)
		 (%validate-byte src1)
		 (%validate-byte src2)
		 (let ((tuple (+ (* (- src0 bang-char) (pow85 0))
				 (* (- src1 bang-char) (pow85 1))
				 (* (- src2 bang-char) (pow85 2))
				 (pow85 2))))
		   (*dst 0 (>> tuple 24))
		   (*dst 1 (>> tuple 16))
		   (loop i j (- src-len 3) (- dst-len 2)))))))

	  ((= src-len 2)
	   (when (< dst-len 1)
	    (values #f i j))
	   (let ((src0 (*src)))
	     (if (= src0 z-char)
		 (begin
		   (*dst 0 0)
		   (*dst 1 0)
		   (*dst 2 0)
		   (*dst 3 0)
		   (loop i j (- src-len 1) (- dst-len 4)))
	       (let ((src1 (*src)))
		 (when (= src1 z-char)
		   (%error-invalid-z-char src0 src1))
		 (%validate-byte src0)
		 (%validate-byte src1)
		 (let ((tuple (+ (* (- src0 bang-char) (pow85 0))
				 (* (- src1 bang-char) (pow85 1))
				 (pow85 1))))
		   (*dst 0 (>> tuple 24))
		   (loop i j (- src-len 2) (- dst-len 1)))))))

	  ((= src-len 1)
	   (let ((src0 (*src)))
	     (if (= src0 z-char)
		 (if (< dst-len ascii85-encode-block-length)
		     (values #f i j)
		   (begin
		     (*dst 0 0)
		     (*dst 1 0)
		     (*dst 2 0)
		     (*dst 3 0)
		     (values #t i j)))
	       (%error-invalid-input-length))))

	  (else
	   (%error-invalid-input-length)))))


;;;; done

)

;;; end of file
