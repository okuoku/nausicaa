;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: base16 encoding/deconding
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


(library (armor base16)
  (export
    <base16-encode-ctx>				<base16-decode-ctx>
    make-<base16-encode-ctx>			make-<base16-decode-ctx>
    <base16-encode-ctx>?			<base16-decode-ctx>?
    <base16-encode-ctx>-encoding-case		<base16-decode-ctx>-encoding-case
    <base16-encode-ctx>-table			<base16-decode-ctx>-table

    base16-encode-update!			base16-decode-update!
    base16-encode-final!			base16-decode-final!

    base16-encode-length			base16-decode-length
    base16-encode-update-length			base16-decode-update-length
    base16-encode-final-length			base16-decode-final-length

						base16-decode-block-length

    armored-byte-of-base16/upper-case?
    armored-byte-of-base16/mixed-case?
    armored-byte-of-base16/lower-case?)
  (import (rnrs)
    (language-extensions)
    (armor conditions))


(define-record-type <base16-encode-ctx>
  (fields (immutable encoding-case)
	  (immutable table))
  (protocol
   (lambda (maker)
     (lambda (encoding-case)
       ;;ENCODING-CASE must be a Scheme symbol among: upper, lower.
       ;;
       (let* ((encoding-case	(if (memq encoding-case '(upper lower))
				    encoding-case
				  (assertion-violation 'make-<base16-encode-ctx>
				    "invalid input case selection for base16 encoder"
				    encoding-case)))
	      (table		(case encoding-case
				  ((upper)	encode-table-base16/upper-case)
				  ((lower)	encode-table-base16/lower-case))))

	 (maker encoding-case table))))))


(define-record-type <base16-decode-ctx>
  (fields (immutable encoding-case)
	  (immutable table))
  (protocol
   (lambda (maker)
     (lambda (encoding-case)
       ;;ENCODING-CASE must be a Scheme symbol among: upper, lower.
       ;;
       (let* ((encoding-case	(if (memq encoding-case '(upper lower mixed))
				    encoding-case
				  (assertion-violation 'make-<base16-decode-ctx>
				    "invalid input case selection for base16 decoder"
				    encoding-case)))
	      (table		(case encoding-case
				  ((upper)	decode-table-base16/upper-case)
				  ((mixed)	decode-table-base16/mixed-case)
				  ((lower)	decode-table-base16/lower-case))))

	 (maker encoding-case table))))))


;;;; encoding tables

;;The following  is the  encoding table for  base16 as specified  by RFC
;;4648, upper case.

(define encode-alphabet-base16/upper-case
  '#(#\0 #\1 #\2   #\3 #\4 #\5   #\6 #\7 #\8   #\9    ;;  0 -  9
     #\A #\B #\C   #\D #\E #\F))                      ;; 10 - 15

(define encode-table-base16/upper-case
  (vector-map char->integer encode-alphabet-base16/upper-case))

;;The following  is the  encoding table for  base16 as specified  by RFC
;;4648, lower case.

(define encode-alphabet-base16/lower-case
  '#(#\0 #\1 #\2   #\3 #\4 #\5   #\6 #\7 #\8   #\9    ;;  0 -  9
     #\a #\b #\c   #\d #\e #\f))                      ;; 10 - 15

(define encode-table-base16/lower-case
  (vector-map char->integer encode-alphabet-base16/lower-case))


;;;; decoding tables

(define (alphabet->table! alphabet table)
  (dotimes (i (vector-length alphabet) table)
    (vector-set! table (char->integer (vector-ref alphabet i)) i)))

;;Decoding tables for base 16:  disallowed input characters have a value
;;of #f; only 128 chars, as everything above 127 (#x80) is #f.

(define decode-table-base16/lower-case
  (alphabet->table! encode-alphabet-base16/lower-case (make-vector 128 #f)))

(define decode-table-base16/upper-case
  (alphabet->table! encode-alphabet-base16/upper-case (make-vector 128 #f)))

(define decode-table-base16/mixed-case
  (alphabet->table! encode-alphabet-base16/upper-case
		    (alphabet->table! encode-alphabet-base16/lower-case
				      (make-vector 128 #f))))

(define (armored-byte-of-base16/lower-case? byte)
  (and (integer? byte)
       (exact? byte)
       (<= 0 byte 127)
       (vector-ref decode-table-base16/lower-case byte)))

(define (armored-byte-of-base16/mixed-case? byte)
  (and (integer? byte)
       (exact? byte)
       (<= 0 byte 127)
       (vector-ref decode-table-base16/mixed-case byte)))

(define (armored-byte-of-base16/upper-case? byte)
  (and (integer? byte)
       (exact? byte)
       (<= 0 byte 127)
       (vector-ref decode-table-base16/upper-case byte)))


;;;; helpers

(define-macro (lower4 ?number)
  (bitwise-and #b00001111 ?number))

(define-macro (lower8 ?number)
  (bitwise-and #xFF ?number))

(define << bitwise-arithmetic-shift-left)
(define >> bitwise-arithmetic-shift-right)


;;;; encoded output length estimation
;;
;;Every 8 bits  of binary data, 16 bits of  ASCII characters; every byte
;;of binary data, 2 bytes of ASCII characters.
;;

(define (base16-encode-update-length len)
  ;;Return the  minimum number  of bytes required  in the  output vector
  ;;when BASE16-ENCODE-UPDATE!  is applied  to LEN bytes of binary input
  ;;data.  Notice  that the function  will consume input  in single-byte
  ;;blocks  and produce  output in  2-bytes blocks  only, so  the return
  ;;value is always zero or an exact multiple of 2.
  ;;
  (<< len 1))

(define base16-encode-final-length	base16-encode-update-length)
(define base16-encode-length		base16-encode-update-length)


;;;; decoded output length estimation
;;
;;Every 8 bits  of binary data, 16 bits of  ASCII characters; every byte
;;of binary data, 2 bytes of ASCII characters.
;;

(define base16-decode-block-length 2)

(define (base16-decode-update-length len)
  ;;Return the  minimum number  of bytes required  in the  output vector
  ;;when BASE16-DECODE-UPDATE!   is applied  to LEN ASCII  characters of
  ;;input.   Notice that  the  function will  consume  input in  2-bytes
  ;;blocks only and produce output  in single-byte blocks.
  ;;
  (if (<= base16-decode-block-length len)
      (>> len 1)
    0))

(define (base16-decode-final-length len)
  ;;Return the  minimum number  of bytes required  in the  output vector
  ;;when  BASE16-DECODE-FINAL!  is  applied to  LEN ASCII  characters of
  ;;input.  LEN must be a multiple of 2, else the return value is #f.
  ;;
  (if (zero? (mod len 2))
      (>> len 1)
    #f))

;;Return the minimum number of  bytes required in the output vector when
;;BASE16-DECODE-FINAL!   is applied  to LEN  ASCII characters  of input.
;;This is  also the number required when  both BASE16-DECODE-UPDATE! and
;;BASE16-DECODE-FINAL! are used.  LEN must  be a multiple of 2, else the
;;return value is #f.
(define base16-decode-length base16-decode-final-length)


(define (base16-encode-update! ctx dst-bv dst-start src-bv src-start src-past)
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
  ;;Binary data bytes are read  from SRC-BV in single-byte blocks; ASCII
  ;;characters    are     written    to    DST-BV     in    chunks    of
  ;;BASE16-DECODE-BLOCK-LENGTH bytes.
  ;;

  (define table
    (<base16-encode-ctx>-table ctx))

  (let loop ((i		dst-start)
	     (j		src-start)
	     (src-len	(- src-past src-start))
	     (dst-len	(- (bytevector-length dst-bv) dst-start)))

    (define-macro (*src)
      (begin0-let ((byte (bytevector-u8-ref src-bv j)))
	(incr! j)))

    (define-macro (*dst ?dummy ?expr)
      (bytevector-u8-set! dst-bv i (vector-ref table (lower4 ?expr)))
      (incr! i))

    (if (or (zero? src-len)
	    (< dst-len base16-decode-block-length))
	(values i j)
      ;;    src0
      ;;
      ;; |76543210|
      ;; |--------|
      ;; |3210    | dst0
      ;; |    3210| dst1
      (let ((src0 (*src)))
	(*dst 0 (>> src0 4))
	(*dst 1     src0)
	(loop i j
	      (- src-len 1)
	      (- dst-len base16-decode-block-length))))))


(define (base16-encode-final! ctx dst-bv dst-start src-bv src-start src-past)
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
  ;;Binary data bytes are read  from SRC-BV in single-byte blocks; ASCII
  ;;characters    are     written    to    DST-BV     in    chunks    of
  ;;BASE16-DECODE-BLOCK-LENGTH bytes.
  ;;

  (define table
    (<base16-encode-ctx>-table ctx))

  (let loop ((i		dst-start)
	     (j		src-start)
	     (src-len	(- src-past src-start))
	     (dst-len	(- (bytevector-length dst-bv) dst-start)))

    (define-macro (*src)
      (begin0-let ((byte (bytevector-u8-ref src-bv j)))
	(incr! j)))

    (define-macro (*dst ?dummy ?expr)
      (bytevector-u8-set! dst-bv i (vector-ref table (lower4 ?expr)))
      (incr! i))

    (if (or (zero? src-len)
	    (< dst-len base16-decode-block-length))
	(values #t i j)
      ;;    src0
      ;;
      ;; |76543210|
      ;; |--------|
      ;; |3210    | dst0
      ;; |    3210| dst1
      (let ((src0 (*src)))
	(*dst 0 (>> src0 4))
	(*dst 1     src0)
	(loop i j
	      (- src-len 1)
	      (- dst-len base16-decode-block-length))))))


(define (base16-decode-update! ctx dst-bv dst-start src-bv src-start src-past)
  ;;Decode ASCII characters from the bytevector SRC-BV starting at index
  ;;SRC-START (included)  up to index SRC-PAST  (excluded); store binary
  ;;data  bytes   in  the   bytevector  DST-BV  starting   at  DST-START
  ;;(included); decoding is performed  according to the specification in
  ;;the context CTX.
  ;;
  ;;Return three values:
  ;;
  ;;(1) A boolean: true if padding  is turned on and a full padded block
  ;;was successfully decoded; false otherwise.
  ;;
  ;;(2) The index  of the next non-written byte in  DST-BV; if DST-BV is
  ;;filled to the end, this value is the length of DST-BV.
  ;;
  ;;(3)  The index  of the  next  non-read byte  in SRC-BV;  if all  the
  ;;characters from SRC-BV are consumed, this value is SRC-PAST.
  ;;
  ;;ASCII   characters    are   read   from   SRC-BV    in   chunks   of
  ;;BASE16-DECODE-BLOCK-LENGTH bytes;  binary data bytes  are written to
  ;;DST-BV in single-byte blocks.
  ;;

  (define table
    (<base16-decode-ctx>-table ctx))

  (define (%error-invalid-input-byte byte)
    (raise
     (condition (make-armor-invalid-input-byte-condition)
		(make-who-condition 'base16-decode-update!)
		(make-message-condition "invalid input byte while decoding base16 bytevector")
		(make-irritants-condition byte))))

  (define (%decode char)
    (if (zero? (bitwise-and #x80 char))
	(let ((byte (vector-ref table char)))
	  (or byte (%error-invalid-input-byte char)))
      (%error-invalid-input-byte char)))

  (let loop ((i		dst-start)
	     (j		src-start)
	     (src-len	(- src-past src-start))
	     (dst-len	(- (bytevector-length dst-bv) dst-start)))

    (define-macro (*src)
      (begin0
	  (%decode (bytevector-u8-ref src-bv j))
	(incr! j)))

    (define-macro (*dst ?dummy ?expr)
      (bytevector-u8-set! dst-bv i (lower8 ?expr))
      (incr! i))

    (if (or (< src-len base16-decode-block-length)
	    (zero? dst-len))
	(values #f i j)
      ;;    src0     src1
      ;;
      ;; |76543210|76543210|
      ;; |--------+--------|
      ;; |    7654|        | dst0
      ;; |        |    3210| dst1
      (let ((src0 (*src))
	    (src1 (*src)))
	(*dst 0 (bitwise-ior (<< src0 4) src1))
	(loop i j
	      (- src-len base16-decode-block-length)
	      (- dst-len 1))))))


(define (base16-decode-final! ctx dst-bv dst-start src-bv src-start src-past)
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
  ;;ASCII   characters    are   read   from   SRC-BV    in   chunks   of
  ;;BASE16-DECODE-BLOCK-LENGTH bytes;  binary data bytes  are written to
  ;;DST-BV in single-byte blocks.
  ;;

  (define table (<base16-decode-ctx>-table ctx))

  (define who 'base16-decode-final!)

  (define (%error-invalid-input-length)
    (raise
     (condition (make-armor-invalid-input-length-condition)
		(make-who-condition who)
		(make-message-condition "invalid input length while decoding base16 bytevector"))))

  (define (%error-invalid-input-byte byte)
    (raise
     (condition (make-armor-invalid-input-byte-condition)
		(make-who-condition who)
		(make-message-condition "invalid input byte while decoding base16 bytevector")
		(make-irritants-condition byte))))

  (let loop ((i		dst-start)
	     (j		src-start)
	     (src-len	(- src-past src-start))
	     (dst-len	(- (bytevector-length dst-bv) dst-start)))

    (define-macro (*src)
      (let ((char (bytevector-u8-ref src-bv j)))
	(if (zero? (bitwise-and #x80 char))
	    (begin0-let ((byte (vector-ref table char)))
	      (if byte
		  (incr! j)
		(%error-invalid-input-byte char)))
	  (%error-invalid-input-byte char))))

    (define-macro (*dst ?dummy ?expr)
      (bytevector-u8-set! dst-bv i (lower8 ?expr))
      (incr! i))

    (define-macro (*pad ?dummy)
      (bytevector-u8-set! dst-bv i pad-char)
      (incr! i))

    (cond ((zero? src-len)
	   (values #t i j))

	  ((zero? dst-len)
	   (values #f i j))

	  ((<= base16-decode-block-length src-len)
	   ;;    src0     src1
	   ;;
	   ;; |76543210|76543210|
	   ;; |--------+--------|
	   ;; |    7654|        | dst0
	   ;; |        |    3210| dst1
	   (let ((src0 (*src))
		 (src1 (*src)))
	     (*dst 0 (bitwise-ior (<< src0 4) src1))
	     (loop i j
		   (- src-len base16-decode-block-length)
		   (- dst-len 1))))

	  (else
	   (%error-invalid-input-length)))))


;;;; done

)

;;; end of file
