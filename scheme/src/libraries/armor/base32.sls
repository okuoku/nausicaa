;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: ASCII armor in base32 encoding
;;;Date: Wed Jan 27, 2010
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


(library (armor base32)
  (export

    <base32-encode-ctx>				<base32-decode-ctx>
    make-<base32-encode-ctx>			make-<base32-decode-ctx>
    <base32-encode-ctx>?			<base32-decode-ctx>?
    <base32-encode-ctx>-encoding		<base32-decode-ctx>-encoding
    <base32-encode-ctx>-encoding-case		<base32-decode-ctx>-encoding-case
    <base32-encode-ctx>-generate-padding?	<base32-decode-ctx>-expect-padding?
    <base32-encode-ctx>-pad-char		<base32-decode-ctx>-pad-char
    <base32-encode-ctx>-table			<base32-decode-ctx>-table

    base32-encode-update!			base32-decode-update!
    base32-encode-final!			base32-decode-final!

    base32-encode-length			base32-decode-length
    base32-encode-update-length			base32-decode-update-length
    base32-encode-final-length			base32-decode-final-length

    base32-encode-block-length			base32-decode-block-length

    armored-byte-of-base32/upper-case?		armored-byte-of-base32/hex/upper-case?
    armored-byte-of-base32/lower-case?		armored-byte-of-base32/hex/lower-case?
    armored-byte-of-base32/mixed-case?		armored-byte-of-base32/hex/mixed-case?)
  (import (rnrs)
    (language-extensions)
    (armor conditions))


(define-record-type <base32-encode-ctx>
  (fields (immutable encoding)
	  (immutable encoding-case)
	  (immutable generate-padding?)
	  (immutable pad-char)
	  (immutable table))
  (protocol
   (lambda (maker)
     (lambda (encoding generate-padding? encoding-case)
       ;;ENCODING must  be a Scheme symbol selecting  the encoding type:
       ;;base32,  rfc4648, base32/hex,  rfc2938.  The  argument  value is
       ;;normalised to one among: base32, base32/hex.
       ;;
       ;;If GENERATE-PADDING?   is true: the  output is padded  with "="
       ;;characters so that  its length is an exact  multiple of 8.  The
       ;;argument value is normalised to #t or #f.
       ;;
       ;;ENCODING-CASE must be a Scheme symbol among: upper, lower.
       ;;
       (let* ((encoding		(case encoding
				  ((base32 rfc4648)	'base32)
				  ((base32/hex rfc2938)	'base32/hex)
				  (else
				   (assertion-violation 'make-<base32-encode-ctx>
				     "invalid encoding selection for base32 encoder" encoding))))
	      (encoding-case	(case encoding-case
				  ((lower)	'lower)
				  ((upper)	'upper)
				  (else
				   (assertion-violation 'make-<base32-encode-ctx>
				     "invalid input case selection for base32 encoder"))))
	      (generate-padding? (if generate-padding? #t #f))
	      (table		(case encoding
				  ((base32)
				   (case encoding-case
				     ((upper)	encode-table-base32/upper-case)
				     ((lower)	encode-table-base32/lower-case)))
				  ((base32/hex)
				   (case encoding-case
				     ((upper)	encode-table-base32/hex/upper-case)
				     ((lower)	encode-table-base32/hex/lower-case))))))

	 (maker encoding encoding-case generate-padding? (char->integer #\=) table))))))


(define-record-type <base32-decode-ctx>
  (fields (immutable encoding)
	  (immutable encoding-case)
	  (immutable expect-padding?)
	  (immutable pad-char)
	  (immutable table))
  (protocol
   (lambda (maker)
     (lambda (encoding expect-padding? encoding-case)
       ;;ENCODING must  be a Scheme symbol selecting  the encoding type:
       ;;base32,  rfc4648, base32/hex,  rfc2938.  The  argument  value is
       ;;normalised to one among: base32, base32/hex.
       ;;
       ;;ENCODING-CASE must  be a  Scheme symbol selecting  the expected
       ;;case of the input string: lower, upper, mixed.
       ;;
       ;;If EXPECT-PADDING?  is true: the input is expected to be padded
       ;;with "=" characters so that  its length is an exact multiple of
       ;;8.  The argument value is normalised to #t or #f.
       ;;
       (let* ((encoding		(case encoding
				  ((base32 rfc4648)	'base32)
				  ((base32/hex rfc2938)	'base32/hex)
				  (else
				   (assertion-violation 'make-<base32-decode-ctx>
				     "invalid encoding selection for base32 decoder" encoding))))
	      (expect-padding?	(if expect-padding? #t #f))
	      (encoding-case	(case encoding-case
				  ((lower)	'lower)
				  ((upper)	'upper)
				  ((mixed)	'mixed)
				  (else
				   (assertion-violation 'make-<base32-decode-ctx>
				     "invalid input case selection for base32 decoder"))))
	      (table		(case encoding
				  ((base32)
				   (case encoding-case
				     ((lower)
				      decode-table-base32/lower-case)
				     ((upper)
				      decode-table-base32/upper-case)
				     ((mixed)
				      decode-table-base32/mixed-case)))
				  ((base32/hex)
				   (case encoding-case
				     ((lower)
				      decode-table-base32/hex/lower-case)
				     ((upper)
				      decode-table-base32/hex/upper-case)
				     ((mixed)
				      decode-table-base32/hex/mixed-case))))))

	 (maker encoding encoding-case expect-padding? (char->integer #\=) table))))))


;;;; encoding tables

;;The following  are the encode tables  for base 32 as  specified by RFC
;;4648, also known  as "base32": #\0 and #\1 (numbers  zero and one) are
;;skipped because  they are similar  to #\O and  #\l (upper case  oh and
;;lower case ell).

(define encode-alphabet-base32/upper-case
  '#(#\A #\B #\C   #\D #\E #\F   #\G #\H #\I   #\J    ;;  0 -  9
     #\K #\L #\M   #\N #\O #\P   #\Q #\R #\S   #\T    ;; 10 - 19
     #\U #\V #\W   #\X #\Y #\Z   #\2 #\3 #\4   #\5    ;; 20 - 29
     #\6 #\7))					      ;; 30 - 31

(define encode-alphabet-base32/lower-case
  '#(#\a #\b #\c   #\d #\e #\f   #\g #\h #\i   #\j    ;;  0 -  9
     #\k #\l #\m   #\n #\o #\p   #\q #\r #\s   #\t    ;; 10 - 19
     #\u #\v #\w   #\x #\y #\z   #\2 #\3 #\4   #\5    ;; 20 - 29
     #\6 #\7))					      ;; 30 - 31

(define encode-table-base32/upper-case
  (vector-map char->integer encode-alphabet-base32/upper-case))

(define encode-table-base32/lower-case
  (vector-map char->integer encode-alphabet-base32/lower-case))

;;The following  are the encode tables  for base 32 as  specified by RFC
;;2938,  also known as  "base32/hex": it  uses all  the digits  first and
;;excludes the characters #\W, #\X, #\Y, #\Z.

(define encode-alphabet-base32/hex/upper-case
  '#(#\0 #\1 #\2   #\3 #\4 #\5   #\6 #\7 #\8   #\9    ;;  0 -  9
     #\A #\B #\C   #\D #\E #\F   #\G #\H #\I   #\J    ;; 10 - 19
     #\K #\L #\M   #\N #\O #\P   #\Q #\R #\S   #\T    ;; 20 - 29
     #\U #\V))					      ;; 30 - 31

(define encode-alphabet-base32/hex/lower-case
  '#(#\0 #\1 #\2   #\3 #\4 #\5   #\6 #\7 #\8   #\9    ;;  0 -  9
     #\a #\b #\c   #\d #\e #\f   #\g #\h #\i   #\j    ;; 10 - 19
     #\k #\l #\m   #\n #\o #\p   #\q #\r #\s   #\t    ;; 20 - 29
     #\u #\v))					      ;; 30 - 31

(define encode-table-base32/hex/upper-case
  (vector-map char->integer encode-alphabet-base32/hex/upper-case))

(define encode-table-base32/hex/lower-case
  (vector-map char->integer encode-alphabet-base32/hex/lower-case))


;;;; decoding tables

(define (alphabet->table! alphabet table)
  (dotimes (i (vector-length alphabet) table)
    (vector-set! table (char->integer (vector-ref alphabet i)) i)))

;;Decoding tables for base 32 as specified by RFC 4648: disallowed input
;;characters have a value of #f; only 128 chars, as everything above 127
;;(#x80) is #f.

(define decode-table-base32/upper-case
  (alphabet->table! encode-alphabet-base32/upper-case (make-vector 128 #f)))

(define decode-table-base32/lower-case
  (alphabet->table! encode-alphabet-base32/lower-case (make-vector 128 #f)))

(define decode-table-base32/mixed-case
  (alphabet->table! encode-alphabet-base32/upper-case
		    (alphabet->table! encode-alphabet-base32/lower-case
				      (make-vector 128 #f))))

(define (armored-byte-of-base32/upper-case? byte)
  (and (integer? byte)
       (exact? byte)
       (<= 0 byte 127)
       (vector-ref decode-table-base32/upper-case byte)))

(define (armored-byte-of-base32/lower-case? byte)
  (and (integer? byte)
       (exact? byte)
       (<= 0 byte 127)
       (vector-ref decode-table-base32/lower-case byte)))

(define (armored-byte-of-base32/mixed-case? byte)
  (and (integer? byte)
       (exact? byte)
       (<= 0 byte 127)
       (vector-ref decode-table-base32/mixed-case byte)))

;;Decoding tables for base 32 as specified by RFC 2938: disallowed input
;;characters have a value of #f; only 128 chars, as everything above 127
;;(#x80) is #f.

(define decode-table-base32/hex/upper-case
  (alphabet->table! encode-alphabet-base32/hex/upper-case (make-vector 128 #f)))

(define decode-table-base32/hex/lower-case
  (alphabet->table! encode-alphabet-base32/hex/lower-case (make-vector 128 #f)))

(define decode-table-base32/hex/mixed-case
  (alphabet->table! encode-alphabet-base32/hex/upper-case
		    (alphabet->table! encode-alphabet-base32/hex/lower-case
				      (make-vector 128 #f))))

(define (armored-byte-of-base32/hex/upper-case? byte)
  (and (integer? byte)
       (exact? byte)
       (<= 0 byte 127)
       (vector-ref decode-table-base32/hex/upper-case byte)))

(define (armored-byte-of-base32/hex/lower-case? byte)
  (and (integer? byte)
       (exact? byte)
       (<= 0 byte 127)
       (vector-ref decode-table-base32/hex/lower-case byte)))

(define (armored-byte-of-base32/hex/mixed-case? byte)
  (and (integer? byte)
       (exact? byte)
       (<= 0 byte 127)
       (vector-ref decode-table-base32/hex/mixed-case byte)))



;;;; helpers

(define-inline (lower5 ?number)
  (bitwise-and #b00011111 ?number))

(define-inline (lower8 ?number)
  (bitwise-and #xFF ?number))

(define << bitwise-arithmetic-shift-left)
(define >> bitwise-arithmetic-shift-right)


;;;; encoded output length estimation
;;
;;Every 5 bits of binary data, 8 bits of ASCII characters; every 5 bytes
;;of binary data, 8 bytes of ASCII characters.
;;

(define base32-encode-block-length 5)

(define (base32-encode-update-length len)
  ;;Return the  minimum number  of bytes required  in the  output vector
  ;;when BASE32-ENCODE-UPDATE!  is applied  to LEN bytes of binary input
  ;;data.  Notice that the function will consume input in 5-bytes blocks
  ;;only and produce output in  8-bytes blocks only, so the return value
  ;;is always zero or an exact multiple of 8.
  ;;
  (if (<= base32-encode-block-length len)
      (* base32-decode-block-length (div len base32-encode-block-length))
    0))

(define base32-encode-final-length
  ;;Return the  minimum number  of bytes required  in the  output vector
  ;;when  BASE32-ENCODE-FINAL! is  applied to  LEN <  5 bytes  of binary
  ;;input data.  The formula is:
  ;;
  ;; (let ((nbits (* 8 len)))
  ;;   (+ (div nbits 5)
  ;;      (if (zero? (mod nbits 5)) 0 1)))
  ;;
  ;;but we  use precomputed  results.  If the  argument is  invalid: the
  ;;return value is #f.
  ;;
  (case-lambda
   ((len)
    (base32-encode-final-length len #f))
   ((len padding?)
    (if padding?
	(if (<= 0 len 4) base32-decode-block-length #f)
      (case len
	((0)	0)
	((1)	2)
	((2)	4)
	((3)	5)
	((4)	7)
	(else	#f))))))

(define base32-encode-length
  ;;Return the  minimum number  of bytes required  in the  output vector
  ;;when BASE32-ENCODE-FINAL!   is applied to LEN bytes  of binary input
  ;;data.    This    is   also    the   number   required    when   both
  ;;BASE32-ENCODE-UPDATE! and BASE32-ENCODE-FINAL! are used.
  ;;
  (case-lambda
   ((len)
    (base32-encode-length len #f))
   ((len padding?)
    (let ((final-len (base32-encode-final-length (mod len base32-encode-block-length) padding?)))
      (if final-len
	  (+ final-len (base32-encode-update-length len))
	#f)))))


;;;; decoded output length estimation
;;
;;Every 5 bits of binary data, 8 bits of ASCII characters; every 5 bytes
;;of binary data, 8 bytes of ASCII characters.
;;

(define base32-decode-block-length 8)

(define (base32-decode-update-length len)
  ;;Return the  minimum number  of bytes required  in the  output vector
  ;;when BASE32-DECODE-UPDATE!   is applied  to LEN ASCII  characters of
  ;;input.   Notice that  the  function will  consume  input in  8-bytes
  ;;blocks only and produce output in 5-bytes blocks only, so the return
  ;;value is always zero or an exact multiple of 5.
  ;;
  (if (<= base32-decode-block-length len)
      (* base32-encode-block-length (div len base32-decode-block-length))
    0))

(define base32-decode-final-length
  ;;Return the  minimum number  of bytes required  in the  output vector
  ;;when BASE32-DECODE-FINAL! is applied to  LEN < 8 ASCII characters of
  ;;input.
  ;;
  ;;Not all  the values between  0 and  7 are valid;  if a value  is not
  ;;valid the  return value  is #f.   If padding is  on, the  only valid
  ;;value is LEN=0.
  ;;
  ;;To verify if a generic length is valid, we can do:
  ;;
  ;;	(base32-decode-final-length
  ;;		(mod len base32-decode-block-length))
  ;;
  (case-lambda
   ((len)
    (base32-decode-final-length len #f))
   ((len padding?)
    (if padding?
	(if (zero? len) 0 #f)
      (case len
	((0)	0)
	((2)	1)
	((4)	2)
	((5)	3)
	((7)	4)
	(else	#f))))))

(define base32-decode-length
  ;;Return the  minimum number  of bytes required  in the  output vector
  ;;when  BASE32-DECODE-FINAL!  is  applied to  LEN ASCII  characters of
  ;;input.    This    is   also   the   number    required   when   both
  ;;BASE32-DECODE-UPDATE! and BASE32-DECODE-FINAL! are used.
  ;;
  (case-lambda
   ((len)
    (base32-decode-length len #f))
   ((len padding?)
    (let ((final-len (base32-decode-final-length (mod len base32-decode-block-length) padding?)))
      (if final-len
	  (+ final-len (base32-decode-update-length len))
	#f)))))


(define (base32-encode-update! ctx dst-bv dst-start src-bv src-start src-past)
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
  ;;Binary data bytes  are read from SRC-BV in chunks  of 5 bytes; ASCII
  ;;characters are written to DST-BV in chunks of 8 bytes.
  ;;

  (define table
    (<base32-encode-ctx>-table ctx))

  (let loop ((i		dst-start)
	     (j		src-start)
	     (src-len	(- src-past src-start))
	     (dst-len	(- (bytevector-length dst-bv) dst-start)))

    (define-inline (*src)
      (begin0-let ((byte (bytevector-u8-ref src-bv j)))
	(incr! j)))

    (define-inline (*dst ?dummy ?expr)
      (bytevector-u8-set! dst-bv i (vector-ref table (lower5 ?expr)))
      (incr! i))

    (if (or (< src-len 5) (< dst-len 8))
	(values i j)
      ;;    src0     src1     src2     src3     src4
      ;;
      ;; |76543210|76543210|76543210|76543210|76543210|
      ;; |--------+--------+--------+--------+--------|
      ;; |43210   |        |        |        |        | dst0
      ;; |     432|10      |        |        |        | dst1
      ;; |        |  43210 |        |        |        | dst2
      ;; |        |       4|3210    |        |        | dst3
      ;; |        |        |    4321|0       |        | dst4
      ;; |        |        |        | 43210  |        | dst5
      ;; |        |        |        |      43|210     | dst6
      ;; |        |        |        |        |   43210| dst7
      (let* ((src0 (*src))	;we need LET* to enforce the order
	     (src1 (*src))
	     (src2 (*src))
	     (src3 (*src))
	     (src4 (*src)))
	(*dst 0              (>> src0 3))
	(*dst 1 (bitwise-ior (<< src0 2) (>> src1 6)))
	(*dst 2              (>> src1 1))
	(*dst 3 (bitwise-ior (<< src1 4) (>> src2 4)))
	(*dst 4 (bitwise-ior (<< src2 1) (>> src3 7)))
	(*dst 5              (>> src3 2))
	(*dst 6 (bitwise-ior (<< src3 3) (>> src4 5)))
	(*dst 7              src4)
	(loop i j (- src-len 5) (- dst-len 8))))))


(define (base32-encode-final! ctx dst-bv dst-start src-bv src-start src-past)
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
  ;;Binary data bytes  are read from SRC-BV in chunks  of 5 bytes; ASCII
  ;;characters are written to DST-BV  in chunks of 8 bytes.  After that,
  ;;trailing  bytes from  SRC-BV are  encoded and  optionally  padded as
  ;;specified in CTX.
  ;;

  (define table
    (<base32-encode-ctx>-table ctx))
  (define generate-padding?
    (<base32-encode-ctx>-generate-padding? ctx))
  (define pad-char
    (<base32-encode-ctx>-pad-char ctx))

  (let loop ((i		dst-start)
	     (j		src-start)
	     (src-len	(- src-past src-start))
	     (dst-len	(- (bytevector-length dst-bv) dst-start)))

    (define-inline (*src)
      (begin0-let ((byte (bytevector-u8-ref src-bv j)))
	(incr! j)))

    (define-inline (*dst ?dummy ?expr)
      (bytevector-u8-set! dst-bv i (vector-ref table (lower5 ?expr)))
      (incr! i))

    (define-inline (*pad ?dummy)
      (bytevector-u8-set! dst-bv i pad-char)
      (incr! i))

    (cond ((zero? src-len)
	   (values #t i j))

	  ((<= 5 src-len)
	   ;;    src0     src1     src2     src3     src4
	   ;;
	   ;; |76543210|76543210|76543210|76543210|76543210|
	   ;; |--------+--------+--------+--------+--------|
	   ;; |43210   |        |        |        |        | dst0
	   ;; |     432|10      |        |        |        | dst1
	   ;; |        |  43210 |        |        |        | dst2
	   ;; |        |       4|3210    |        |        | dst3
	   ;; |        |        |    4321|0       |        | dst4
	   ;; |        |        |        | 43210  |        | dst5
	   ;; |        |        |        |      43|210     | dst6
	   ;; |        |        |        |        |   43210| dst7
	   (if (< dst-len 8)
	       (values #f i j)
	     (let* ((src0 (*src))	;we need LET* to enforce the order
		    (src1 (*src))
		    (src2 (*src))
		    (src3 (*src))
		    (src4 (*src)))
	       (*dst 0              (>> src0 3))
	       (*dst 1 (bitwise-ior (<< src0 2) (>> src1 6)))
	       (*dst 2              (>> src1 1))
	       (*dst 3 (bitwise-ior (<< src1 4) (>> src2 4)))
	       (*dst 4 (bitwise-ior (<< src2 1) (>> src3 7)))
	       (*dst 5              (>> src3 2))
	       (*dst 6 (bitwise-ior (<< src3 3) (>> src4 5)))
	       (*dst 7              src4)
	       (loop i j (- src-len 5) (- dst-len 8)))))

	  ((= 4 src-len)
	   ;;    src0     src1     src2     src3
	   ;;
	   ;; |76543210|76543210|76543210|76543210|
	   ;; |--------+--------+--------+--------+
	   ;; |43210   |        |        |        | dst0
	   ;; |     432|10      |        |        | dst1
	   ;; |        |  43210 |        |        | dst2
	   ;; |        |       4|3210    |        | dst3
	   ;; |        |        |    4321|0       | dst4
	   ;; |        |        |        | 43210  | dst5
	   ;; |        |        |        |      43|210 dst6
	   ;; |        |        |        |        |
	   (if (< dst-len (if generate-padding? 8 7))
	       (values #f i j)
	     (let* ((src0 (*src))	;we need LET* to enforce the order
		    (src1 (*src))
		    (src2 (*src))
		    (src3 (*src)))
	       (*dst 0              (>> src0 3))
	       (*dst 1 (bitwise-ior (<< src0 2) (>> src1 6)))
	       (*dst 2              (>> src1 1))
	       (*dst 3 (bitwise-ior (<< src1 4) (>> src2 4)))
	       (*dst 4 (bitwise-ior (<< src2 1) (>> src3 7)))
	       (*dst 5              (>> src3 2))
	       (*dst 6              (<< src3 3))
	       (when generate-padding?
		 (*pad 7))
	       (values #t i j))))

	  ((= 3 src-len)
	   ;;    src0     src1     src2
	   ;;
	   ;; |76543210|76543210|76543210|
	   ;; |--------+--------+--------+
	   ;; |43210   |        |        | dst0
	   ;; |     432|10      |        | dst1
	   ;; |        |  43210 |        | dst2
	   ;; |        |       4|3210    | dst3
	   ;; |        |        |    4321|0 dst4
	   ;; |        |        |        |
	   (if (< dst-len (if generate-padding? 8 5))
	       (values #f i j)
	     (let* ((src0 (*src))	;we need LET* to enforce the order
		    (src1 (*src))
		    (src2 (*src)))
	       (*dst 0              (>> src0 3))
	       (*dst 1 (bitwise-ior (<< src0 2) (>> src1 6)))
	       (*dst 2              (>> src1 1))
	       (*dst 3 (bitwise-ior (<< src1 4) (>> src2 4)))
	       (*dst 4              (<< src2 1))
	       (when generate-padding?
		 (*pad 5)
		 (*pad 6)
		 (*pad 7))
	       (values #t i j))))

	  ((= 2 src-len)
	   ;;    src0     src1
	   ;;
	   ;; |76543210|76543210|
	   ;; |--------+--------+
	   ;; |43210   |        | dst0
	   ;; |     432|10      | dst1
	   ;; |        |  43210 | dst2
	   ;; |        |       4|3210 dst3
	   ;; |        |        |
	   (if (< dst-len (if generate-padding? 8 4))
	       (values i j)
	     (let* ((src0 (*src))	;we need LET* to enforce the order
		    (src1 (*src)))
	       (*dst 0              (>> src0 3))
	       (*dst 1 (bitwise-ior (<< src0 2) (>> src1 6)))
	       (*dst 2              (>> src1 1))
	       (*dst 3              (<< src1 4))
	       (when generate-padding?
		 (*pad 4)
		 (*pad 5)
		 (*pad 6)
		 (*pad 7))
	       (values #t i j))))

	  ((= 1 src-len)
	   ;;   src0
	   ;;
	   ;; |76543210|
	   ;; |--------|
	   ;; |43210   | dst0
	   ;; |     432|10 dst1
	   ;; |        |
	   (if (< dst-len (if generate-padding? 8 2))
	       (values #f i j)
	     (let ((src0 (*src)))
	       (*dst 0              (>> src0 3))
	       (*dst 1              (<< src0 2))
	       (when generate-padding?
		 (*pad 2)
		 (*pad 3)
		 (*pad 4)
		 (*pad 5)
		 (*pad 6)
		 (*pad 7))
	       (values #t i j)))))))


(define (base32-decode-update! ctx dst-bv dst-start src-bv src-start src-past)
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
  ;;ASCII characters are  read from SRC-BV in chunks  of 8 bytes; binary
  ;;data bytes are written to DST-BV in chunks of 5 bytes.
  ;;

  (define table
    (<base32-decode-ctx>-table ctx))
  (define pad-char
    (<base32-decode-ctx>-pad-char ctx))
  (define expect-padding?
    (<base32-decode-ctx>-expect-padding? ctx))

  (define (%error-invalid-input-byte byte)
    (raise
     (condition (make-armor-invalid-input-byte-condition)
		(make-who-condition 'base32-decode-update!)
		(make-message-condition "invalid input byte while decoding base32 bytevector")
		(make-irritants-condition byte))))

  (define (%error-invalid-pad-area . chars)
    (raise
     (condition (make-armor-invalid-padding-condition)
		(make-who-condition 'base32-decode-update!)
		(make-message-condition "invalid padded block while decoding base32 bytevector")
		(make-irritants-condition (apply string (map integer->char chars))))))

  (define (%decode char)
    (if (zero? (bitwise-and #x80 char))
	(let ((byte (vector-ref table char)))
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

    (define-inline (*dst ?dummy ?expr)
      (bytevector-u8-set! dst-bv i (lower8 ?expr))
      (incr! i))

    (cond ((or (< src-len 8) (< dst-len 5))
	   (values #f i j))

	  (expect-padding?
	   ;;    src0     src1     src2     src3     src4     src5     src6     src7
	   ;;
	   ;; |76543210|76543210|76543210|76543210|76543210|76543210|76543210|76543210|
	   ;; |--------+--------+--------+--------+--------+--------+--------+--------|
	   ;; |   76543|   210  |        |        |        |        |        |        | dst0
	   ;; |        |      76|   54321|   0    |        |        |        |        | dst1
	   ;; |        |        |        |    7654|   3210 |        |        |        | dst2
	   ;; |        |        |        |        |       7|   65432|   10   |        | dst3
	   ;; |        |        |        |        |        |        |     765|   43210| dst4
	   (let* ((char0 (*src))	;we need LET* to enforce the order
		  (char1 (*src))
		  (char2 (*src))
		  (char3 (*src))
		  (char4 (*src))
		  (char5 (*src))
		  (char6 (*src))
		  (char7 (*src)))
	     (let ((pad0? (= char0 pad-char))
		   (pad1? (= char1 pad-char))
		   (pad2? (= char2 pad-char))
		   (pad3? (= char3 pad-char))
		   (pad4? (= char4 pad-char))
		   (pad5? (= char5 pad-char))
		   (pad6? (= char6 pad-char))
		   (pad7? (= char7 pad-char)))
	       (when (or pad0? pad1?
			 (and (not pad2?) pad3?)
			 (and (not pad5?) pad6?))
		 (%error-invalid-pad-area char0 char1 char2 char3 char4 char5 char6 char7))
	       (let ((src0 (%decode char0))
		     (src1 (%decode char1)))
		 (*dst 0 (bitwise-ior (<< src0 3) (>> src1 2)))
		 (if pad2?
		     (if (and pad4? pad5? pad7?)
			 (values #t i j)
		       (%error-invalid-pad-area char0 char1 char2 char3 char4 char5 char6 char7))
		   (let ((src2 (%decode char2))
			 (src3 (%decode char3)))
		     (*dst 1 (bitwise-ior (<< src1 6) (<< src2 1) (>> src3 4)))
		     (if pad4?
			 (if (and pad5? pad7?)
			     (values #t i j)
			   (%error-invalid-pad-area char0 char1 char2 char3 char4 char5 char6 char7))
		       (let ((src4 (%decode char4)))
			 (*dst 2 (bitwise-ior (<< src3 4) (>> src4 1)))
			 (if pad5?
			     (if pad7?
				 (values #t i j)
			       (%error-invalid-pad-area char0 char1 char2 char3 char4 char5 char6 char7))
			   (let ((src5 (%decode char5))
				 (src6 (%decode char6)))
			     (*dst 3 (bitwise-ior (<< src4 7) (<< src5 2) (>> src6 3)))
			     (if pad7?
				 (values #t i j)
			       (let ((src7 (%decode char7)))
				 (*dst 4 (bitwise-ior (<< src6 5) src7))
				 (loop i j (- src-len 8) (- dst-len 5))))))))))))))

	  (else
	   ;;    src0     src1     src2     src3     src4     src5     src6     src7
	   ;;
	   ;; |76543210|76543210|76543210|76543210|76543210|76543210|76543210|76543210|
	   ;; |--------+--------+--------+--------+--------+--------+--------+--------|
	   ;; |   76543|   210  |        |        |        |        |        |        | dst0
	   ;; |        |      76|   54321|   0    |        |        |        |        | dst1
	   ;; |        |        |        |    7654|   3210 |        |        |        | dst2
	   ;; |        |        |        |        |       7|   65432|   10   |        | dst3
	   ;; |        |        |        |        |        |        |     765|   43210| dst4
	   (let* ((src0 (%decode (*src)))	;we need LET* to enforce the order
		  (src1 (%decode (*src)))
		  (src2 (%decode (*src)))
		  (src3 (%decode (*src)))
		  (src4 (%decode (*src)))
		  (src5 (%decode (*src)))
		  (src6 (%decode (*src)))
		  (src7 (%decode (*src))))

	     (*dst 0 (bitwise-ior (<< src0 3) (>> src1 2)))
	     (*dst 1 (bitwise-ior (<< src1 6) (<< src2 1) (>> src3 4)))
	     (*dst 2 (bitwise-ior (<< src3 4) (>> src4 1)))
	     (*dst 3 (bitwise-ior (<< src4 7) (<< src5 2) (>> src6 3)))
	     (*dst 4 (bitwise-ior (<< src6 5) src7))
	     (loop i j (- src-len 8) (- dst-len 5)))))))


(define (base32-decode-final! ctx dst-bv dst-start src-bv src-start src-past)
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
  ;;ASCII characters are  read from SRC-BV in chunks  of 8 bytes; binary
  ;;data bytes are written to DST-BV  in chunks of 8 bytes.  If the last
  ;;characters are  in a chunk of  less than 8, they  are interpreted as
  ;;unpadded characters as specified in CTX.
  ;;

  (define who 'base32-decode-final!)

  (define (%error-invalid-input-length)
    (raise
     (condition (make-armor-invalid-input-length-condition)
		(make-who-condition who)
		(make-message-condition "invalid input length while decoding base32 bytevector"))))

  (if (<base32-decode-ctx>-expect-padding? ctx)

      (let ((src-len (- src-past src-start)))
	(cond ((zero? src-len)
	       (values #t dst-start src-start))
	      ((zero? (mod src-len 8))
	       (receive (finished? src-next dst-next)
		   (base32-decode-update! ctx dst-bv dst-start src-bv src-start src-past)
		 (cond (finished?		;;a padded block was successfully processed
			(values finished? src-next dst-next))
		       ((= src-next src-past)	;;all the input was successfully processed
			(values #t src-next dst-next))
		       (else			;;output bytevector full
			(values finished? src-next dst-next)))))
	      (else
	       (%error-invalid-input-length))))

    (let ((table (<base32-decode-ctx>-table ctx)))

      (define (%error-invalid-input-byte byte)
	(raise
	 (condition (make-armor-invalid-input-byte-condition)
		    (make-who-condition who)
		    (make-message-condition "invalid input byte while decoding base32 bytevector")
		    (make-irritants-condition byte))))

      (let loop ((i		dst-start)
		 (j		src-start)
		 (src-len	(- src-past src-start))
		 (dst-len	(- (bytevector-length dst-bv) dst-start)))

	(define-inline (*src)
	  (let ((char (bytevector-u8-ref src-bv j)))
	    (if (zero? (bitwise-and #x80 char))
		(begin0-let ((byte (vector-ref table char)))
		  (if byte
		      (incr! j)
		    (%error-invalid-input-byte char)))
	      (%error-invalid-input-byte char))))

	(define-inline (*dst ?dummy ?expr)
	  (bytevector-u8-set! dst-bv i (lower8 ?expr))
	  (incr! i))

	(define-inline (*pad ?dummy)
	  (bytevector-u8-set! dst-bv i pad-char)
	  (incr! i))

	(cond ((zero? src-len)
	       (values #t i j))

	      ((<= 8 src-len)
	       ;;    src0     src1     src2     src3     src4     src5     src6     src7
	       ;;
	       ;; |76543210|76543210|76543210|76543210|76543210|76543210|76543210|76543210|
	       ;; |--------+--------+--------+--------+--------+--------+--------+--------|
	       ;; |   76543|   210  |        |        |        |        |        |        | dst0
	       ;; |        |      76|   54321|   0    |        |        |        |        | dst1
	       ;; |        |        |        |    7654|   3210 |        |        |        | dst2
	       ;; |        |        |        |        |       7|   65432|   10   |        | dst3
	       ;; |        |        |        |        |        |        |     765|   43210| dst4
	       (if (< dst-len 5)
		   (values #f i j)
		 (let* ((src0 (*src))	;we need LET* to enforce the order
			(src1 (*src))
			(src2 (*src))
			(src3 (*src))
			(src4 (*src))
			(src5 (*src))
			(src6 (*src))
			(src7 (*src)))
		   (*dst 0 (bitwise-ior (<< src0 3) (>> src1 2)))
		   (*dst 1 (bitwise-ior (<< src1 6) (<< src2 1) (>> src3 4)))
		   (*dst 2 (bitwise-ior (<< src3 4) (>> src4 1)))
		   (*dst 3 (bitwise-ior (<< src4 7) (<< src5 2) (>> src6 3)))
		   (*dst 4 (bitwise-ior (<< src6 5) src7))
		   (loop i j (- src-len 8) (- dst-len 5)))))

	      ((= 7 src-len)
	       ;;    src0     src1     src2     src3     src4     src5     src6
	       ;;
	       ;; |76543210|76543210|76543210|76543210|76543210|76543210|76543210|
	       ;; |--------+--------+--------+--------+--------+--------+--------|
	       ;; |   76543|   210  |        |        |        |        |        | dst0
	       ;; |        |      76|   54321|   0    |        |        |        | dst1
	       ;; |        |        |        |    7654|   3210 |        |        | dst2
	       ;; |        |        |        |        |       7|   65432|   10   | dst3
	       (if (< dst-len 4)
		   (values #f i j)
		 (let* ((src0 (*src))	;we need LET* to enforce the order
			(src1 (*src))
			(src2 (*src))
			(src3 (*src))
			(src4 (*src))
			(src5 (*src))
			(src6 (*src)))
		   (*dst 0 (bitwise-ior (<< src0 3) (>> src1 2)))
		   (*dst 1 (bitwise-ior (<< src1 6) (<< src2 1) (>> src3 4)))
		   (*dst 2 (bitwise-ior (<< src3 4) (>> src4 1)))
		   (*dst 3 (bitwise-ior (<< src4 7) (<< src5 2) (>> src6 3)))
		   (values #t i j))))

	      ((= 5 src-len)
	       ;;    src0     src1     src2     src3     src4
	       ;;
	       ;; |76543210|76543210|76543210|76543210|76543210|
	       ;; |--------+--------+--------+--------+--------|
	       ;; |   76543|   210  |        |        |        | dst0
	       ;; |        |      76|   54321|   0    |        | dst1
	       ;; |        |        |        |    7654|   3210 | dst2
	       (if (< dst-len 3)
		   (values #f i j)
		 (let* ((src0 (*src))	;we need LET* to enforce the order
			(src1 (*src))
			(src2 (*src))
			(src3 (*src))
			(src4 (*src)))
		   (*dst 0 (bitwise-ior (<< src0 3) (>> src1 2)))
		   (*dst 1 (bitwise-ior (<< src1 6) (<< src2 1) (>> src3 4)))
		   (*dst 2 (bitwise-ior (<< src3 4) (>> src4 1)))
		   (values #t i j))))

	      ((= 4 src-len)
	       ;;    src0     src1     src2     src3
	       ;;
	       ;; |76543210|76543210|76543210|76543210|
	       ;; |--------+--------+--------+--------|
	       ;; |   76543|   210  |        |        | dst0
	       ;; |        |      76|   54321|   0    | dst1
	       (if (< dst-len 2)
		   (values #f i j)
		 (let* ((src0 (*src))	;we need LET* to enforce the order
			(src1 (*src))
			(src2 (*src))
			(src3 (*src)))
		   (*dst 0 (bitwise-ior (<< src0 3) (>> src1 2)))
		   (*dst 1 (bitwise-ior (<< src1 6) (<< src2 1) (>> src3 4)))
		   (values #t i j))))

	      ((= 2 src-len)
	       ;;    src0     src1
	       ;;
	       ;; |76543210|76543210|
	       ;; |--------+--------+
	       ;; |   76543|   210  | dst0
	       ;;
	       (if (< dst-len 1)
		   (values #f i j)
		 (let* ((src0 (*src))	;we need LET* to enforce the order
			(src1 (*src)))
		   (*dst 0 (bitwise-ior (<< src0 3) (>> src1 2)))
		   (values #t i j))))

	      (else
	       (%error-invalid-input-length)))))))


;;;; done

)

;;; end of file
