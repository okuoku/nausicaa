;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: ASCII armor in base64 encoding
;;;Date: Fri Mar  5, 2010
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


(library (armor base64)
  (export

    <base64-encode-ctx>				<base64-decode-ctx>
    make-<base64-encode-ctx>			make-<base64-decode-ctx>
    <base64-encode-ctx>?			<base64-decode-ctx>?
    <base64-encode-ctx>-encoding		<base64-decode-ctx>-encoding
    <base64-encode-ctx>-generate-padding?	<base64-decode-ctx>-expect-padding?
    <base64-encode-ctx>-pad-char		<base64-decode-ctx>-pad-char
    <base64-encode-ctx>-table			<base64-decode-ctx>-table

    base64-encode-update!			base64-decode-update!
    base64-encode-final!			base64-decode-final!

    base64-encode-length			base64-decode-length
    base64-encode-update-length			base64-decode-update-length
    base64-encode-final-length			base64-decode-final-length

    base64-encode-block-length			base64-decode-block-length

    armored-byte-of-base64?			armored-byte-of-base64/url?)
  (import (rnrs)
    (language-extensions)
    (armor conditions))


(define-record-type <base64-encode-ctx>
  (fields (immutable encoding)
	  (immutable generate-padding?)
	  (immutable pad-char)
	  (immutable table))
  (protocol
   (lambda (maker)
     (lambda (encoding generate-padding?)
       ;;ENCODING must  be a Scheme symbol selecting  the encoding type:
       ;;base64, base64/url.
       ;;
       ;;If GENERATE-PADDING?   is true: the  output is padded  with "="
       ;;characters so that  its length is an exact  multiple of 4.  The
       ;;argument value is normalised to #t or #f.
       ;;
       (let* ((encoding		(case encoding
				  ((base64)	'base64)
				  ((base64/url)	'base64/url)
				  (else
				   (assertion-violation 'make-<base64-encode-ctx>
				     "invalid encoding selection for base64 encoder" encoding))))
	      (generate-padding? (if generate-padding? #t #f))
	      (table		(case encoding
				  ((base64)	encode-table-base64)
				  ((base64hex)	encode-table-base64/url))))

	 (maker encoding generate-padding? (char->integer #\=) table))))))


(define-record-type <base64-decode-ctx>
  (fields (immutable encoding)
	  (immutable expect-padding?)
	  (immutable pad-char)
	  (immutable table))
  (protocol
   (lambda (maker)
     (lambda (encoding expect-padding?)
       ;;ENCODING must  be a Scheme symbol selecting  the encoding type:
       ;;base64, base64/url.
       ;;
       ;;If EXPECT-PADDING?  is true: the input is expected to be padded
       ;;with "=" characters so that  its length is an exact multiple of
       ;;4.  The argument value is normalised to #t or #f.
       ;;
       (let* ((encoding		(case encoding
				  ((base64)	'base64)
				  ((base64/url)	'base64/url)
				  (else
				   (assertion-violation 'make-<base64-decode-ctx>
				     "invalid encoding selection for base64 decoder" encoding))))
	      (expect-padding?	(if expect-padding? #t #f))
	      (table		(case encoding
				  ((base64)	decode-table-base64)
				  ((base64/url)	decode-table-base64/url))))

	 (maker encoding expect-padding? (char->integer #\=) table))))))


;;;; encoding tables

;;The following  is the  encoding table for  base64 as specified  by RFC
;;4648.

(define encode-alphabet-base64
  '#(#\A #\B #\C   #\D #\E #\F   #\G #\H #\I   #\J    ;;  0 -  9
     #\K #\L #\M   #\N #\O #\P   #\Q #\R #\S   #\T    ;; 10 - 19
     #\U #\V #\W   #\X #\Y #\Z   #\a #\b #\c   #\d    ;; 20 - 29
     #\e #\f #\g   #\h #\i #\j   #\k #\l #\m   #\n    ;; 30 - 31
     #\o #\p #\q   #\r #\s #\t   #\u #\v #\w   #\x    ;; 40 - 49
     #\y #\z #\0   #\1 #\2 #\3   #\4 #\5 #\6   #\7    ;; 50 - 59
     #\8 #\9 #\+   #\/))                              ;; 60 - 63

(define encode-table-base64
  (vector-map char->integer encode-alphabet-base64))

;;The following is  the encoding table for base64 to  be used with URLs;
;;it replaces  #\+ and  #\/ with #\-  and #\_ respectively.   When using
;;this table, NO padding should be added.

(define encode-alphabet-base64/url
  '#(#\A #\B #\C   #\D #\E #\F   #\G #\H #\I   #\J    ;;  0 -  9
     #\K #\L #\M   #\N #\O #\P   #\Q #\R #\S   #\T    ;; 10 - 19
     #\U #\V #\W   #\X #\Y #\Z   #\a #\b #\c   #\d    ;; 20 - 29
     #\e #\f #\g   #\h #\i #\j   #\k #\l #\m   #\n    ;; 30 - 31
     #\o #\p #\q   #\r #\s #\t   #\u #\v #\w   #\x    ;; 40 - 49
     #\y #\z #\0   #\1 #\2 #\3   #\4 #\5 #\6   #\7    ;; 50 - 59
     #\8 #\9 #\-   #\_))                              ;; 60 - 63

(define encode-table-base64/url
  (vector-map char->integer encode-alphabet-base64/url))


;;;; decoding tables

(define (alphabet->table! alphabet table)
  (dotimes (i (vector-length alphabet) table)
    (vector-set! table (char->integer (vector-ref alphabet i)) i)))

;;Decoding tables for base 64 as specified by RFC 4648: disallowed input
;;characters have a value of #f; only 128 chars, as everything above 127
;;(#x80) is #f.

(define decode-table-base64
  (alphabet->table! encode-alphabet-base64 (make-vector 128 #f)))

(define (armored-byte-of-base64? byte)
  (and (integer? byte)
       (exact? byte)
       (<= 0 byte 127)
       (vector-ref decode-table-base64 byte)))

;;Decoding tables for base 64 as specified by RFC 4648: disallowed input
;;characters have a value of #f; only 128 chars, as everything above 127
;;(#x80) is #f.

(define decode-table-base64/url
  (alphabet->table! encode-alphabet-base64/url (make-vector 128 #f)))

(define (armored-byte-of-base64/url? byte)
  (and (integer? byte)
       (exact? byte)
       (<= 0 byte 127)
       (vector-ref decode-table-base64/url byte)))


;;;; helpers

(define-inline (lower6 ?number)
  (bitwise-and #b00111111 ?number))

(define-inline (lower8 ?number)
  (bitwise-and #xFF ?number))

(define << bitwise-arithmetic-shift-left)
(define >> bitwise-arithmetic-shift-right)


;;;; encoded output length estimation
;;
;;Every 6 bits of binary data, 8 bits of ASCII characters; every 3 bytes
;;of binary data, 4 bytes of ASCII characters.
;;

(define base64-encode-block-length 3)

(define (base64-encode-update-length len)
  ;;Return the  minimum number  of bytes required  in the  output vector
  ;;when BASE64-ENCODE-UPDATE!  is applied  to LEN bytes of binary input
  ;;data.  Notice that the function will consume input in 3-bytes blocks
  ;;only and produce output in  4-bytes blocks only, so the return value
  ;;is always zero or an exact multiple of 4.
  ;;
  (if (<= base64-encode-block-length len)
      (* base64-decode-block-length (div len base64-encode-block-length))
    0))

(define base64-encode-final-length
  ;;Return the  minimum number  of bytes required  in the  output vector
  ;;when  BASE64-ENCODE-FINAL! is  applied to  LEN <  3 bytes  of binary
  ;;input data.  The formula is:
  ;;
  ;; (let ((nbits (* 8 len)))
  ;;   (+ (div nbits 6)
  ;;      (if (zero? (mod nbits 6)) 0 1)))
  ;;
  ;;but we  use precomputed  results.  If the  argument is  invalid: the
  ;;return value is #f.
  ;;
  (case-lambda
   ((len)
    (base64-encode-final-length len #f))
   ((len padding?)
    (case len
      ((0)	0)
      ((1)	(if padding? base64-decode-block-length 2))
      ((2)	(if padding? base64-decode-block-length 3))
      (else	#f)))))

(define base64-encode-length
  ;;Return the  minimum number  of bytes required  in the  output vector
  ;;when BASE64-ENCODE-FINAL!   is applied to LEN bytes  of binary input
  ;;data.    This    is   also    the   number   required    when   both
  ;;BASE64-ENCODE-UPDATE! and BASE64-ENCODE-FINAL! are used.
  ;;
  (case-lambda
   ((len)
    (base64-encode-length len #f))
   ((len padding?)
    (let ((final-len (base64-encode-final-length (mod len base64-encode-block-length) padding?)))
      (if final-len
	  (+ final-len (base64-encode-update-length len))
	#f)))))


;;;; decoded output length estimation
;;
;;Every 6 bits of binary data, 8 bits of ASCII characters; every 3 bytes
;;of binary data, 4 bytes of ASCII characters.
;;

(define base64-decode-block-length 4)

(define (base64-decode-update-length len)
  ;;Return the  minimum number  of bytes required  in the  output vector
  ;;when BASE64-DECODE-UPDATE!   is applied  to LEN ASCII  characters of
  ;;input.   Notice that  the  function will  consume  input in  4-bytes
  ;;blocks only and produce output in 3-bytes blocks only, so the return
  ;;value is always zero or an exact multiple of 3.
  ;;
  (if (<= base64-decode-block-length len)
      (* base64-encode-block-length (div len base64-decode-block-length))
    0))

(define base64-decode-final-length
  ;;Return the  minimum number  of bytes required  in the  output vector
  ;;when     BASE64-DECODE-FINAL!     is     applied    to     LEN     <
  ;;BASE64-DECODE-BLOCK-LENGTH ASCII characters of input.
  ;;
  ;;When  padding is  off: not  all the  arguments between  0 and  3 are
  ;;valid; if  the argument is not  valid the return value  is #f.  When
  ;;padding is on: the only valid argument is zero.
  ;;
  (case-lambda
   ((len)
    (base64-decode-final-length len #f))
   ((len padding?)
    (if padding?
	(if (zero? len) 0 #f)
      (case len
	((0)	0)
	((2)	1)
	((3)	2)
	(else	#f))))))

(define base64-decode-length
  ;;Return the  minimum number  of bytes required  in the  output vector
  ;;when  BASE64-DECODE-FINAL!  is  applied to  LEN ASCII  characters of
  ;;input.    This    is   also   the   number    required   when   both
  ;;BASE64-DECODE-UPDATE! and BASE64-DECODE-FINAL! are used.
  ;;
  (case-lambda
   ((len)
    (base64-decode-length len #f))
   ((len padding?)
    (let ((final-len (base64-decode-final-length (mod len base64-decode-block-length) padding?)))
      (if final-len
	  (+ final-len (base64-decode-update-length len))
	#f)))))


(define (base64-encode-update! ctx dst-bv dst-start src-bv src-start src-past)
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
  ;;BASE64-ENCODE-BLOCK-LENGTH  bytes; ASCII  characters are  written to
  ;;DST-BV in chunks of BASE64-DECODE-BLOCK-LENGTH bytes.
  ;;

  (define table
    (<base64-encode-ctx>-table ctx))

  (let loop ((i		dst-start)
	     (j		src-start)
	     (src-len	(- src-past src-start))
	     (dst-len	(- (bytevector-length dst-bv) dst-start)))

    (define-inline (*src)
      (begin0
	  (bytevector-u8-ref src-bv j)
	(incr! j)))

    (define-inline (*dst ?dummy ?expr)
      (bytevector-u8-set! dst-bv i (vector-ref table (lower6 ?expr)))
      (incr! i))

    (if (or (< src-len base64-encode-block-length)
	    (< dst-len base64-decode-block-length))
	(values i j)
      ;;    src0     src1     src2
      ;;
      ;; |76543210|76543210|76543210|
      ;; |--------+--------+--------|
      ;; |543210  |        |        | dst0
      ;; |      54|3210    |        | dst1
      ;; |        |    5432|10      | dst2
      ;; |        |        |  543210| dst3
      (let ((src0 (*src))
	    (src1 (*src))
	    (src2 (*src)))
	(*dst 0                          (>> src0 2))
	(*dst 1 (bitwise-ior (<< src0 4) (>> src1 4)))
	(*dst 2 (bitwise-ior (<< src1 2) (>> src2 6)))
	(*dst 3 (bitwise-ior (<< src1 6)     src2))
	(loop i j
	      (- src-len base64-encode-block-length)
	      (- dst-len base64-decode-block-length))))))


(define (base64-encode-final! ctx dst-bv dst-start src-bv src-start src-past)
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
  ;;BASE64-ENCODE-BLOCK-LENGTH  bytes; ASCII  characters are  written to
  ;;DST-BV in  chunks of BASE64-DECODE-BLOCK-LENGTH  bytes.  After that,
  ;;trailing  bytes from  SRC-BV are  encoded and  optionally  padded as
  ;;specified in CTX.
  ;;

  (define table
    (<base64-encode-ctx>-table ctx))
  (define generate-padding?
    (<base64-encode-ctx>-generate-padding? ctx))
  (define pad-char
    (<base64-encode-ctx>-pad-char ctx))

  (let loop ((i		dst-start)
	     (j		src-start)
	     (src-len	(- src-past src-start))
	     (dst-len	(- (bytevector-length dst-bv) dst-start)))

    (define-inline (*src)
      (begin0
	  (bytevector-u8-ref src-bv j)
	(incr! j)))

    (define-inline (*dst ?dummy ?expr)
      (bytevector-u8-set! dst-bv i (vector-ref table (lower6 ?expr)))
      (incr! i))

    (define-inline (*pad ?dummy)
      (bytevector-u8-set! dst-bv i pad-char)
      (incr! i))

    (cond ((zero? src-len)
	   (values #t i j))

	  ((<= base64-encode-block-length src-len)
	   ;;    src0     src1     src2
	   ;;
	   ;; |76543210|76543210|76543210|
	   ;; |--------+--------+--------|
	   ;; |543210  |        |        | dst0
	   ;; |      54|3210    |        | dst1
	   ;; |        |    5432|10      | dst2
	   ;; |        |        |  543210| dst3
	   (if (< dst-len base64-decode-block-length)
	       (values #f i j)
	     (let ((src0 (*src))
		   (src1 (*src))
		   (src2 (*src)))
	       (*dst 0              (>> src0 2))
	       (*dst 1 (bitwise-ior (<< src0 4) (>> src1 4)))
	       (*dst 2 (bitwise-ior (<< src1 2) (>> src2 6)))
	       (*dst 3 (bitwise-ior (<< src1 6)     src2))
	       (loop i j
		     (- src-len base64-encode-block-length)
		     (- dst-len base64-decode-block-length)))))

	  ((= 2 src-len)
	   ;;    src0     src1
	   ;;
	   ;; |76543210|76543210|
	   ;; |--------+--------|
	   ;; |543210  |        |   dst0
	   ;; |      54|3210    |   dst1
	   ;; |        |    5432|10 dst2
	   (if (< dst-len (if generate-padding? base64-decode-block-length 3))
	       (values #f i j)
	     (let ((src0 (*src))
		   (src1 (*src)))
	       (*dst 0              (>> src0 2))
	       (*dst 1 (bitwise-ior (<< src0 4) (>> src1 4)))
	       (*dst 2 (bitwise-ior (<< src1 2)))
	       (when generate-padding?
		 (*pad 3))
	       (values #t i j))))

	  ((= 1 src-len)
	   ;;    src0
	   ;;
	   ;; |76543210|
	   ;; |--------|
	   ;; |543210  |     dst0
	   ;; |      54|3210 dst1
	   (if (< dst-len (if generate-padding? base64-decode-block-length 2))
	       (values #f i j)
	     (let ((src0 (*src)))
	       (*dst 0              (>> src0 2))
	       (*dst 1 (bitwise-ior (<< src0 4)))
	       (when generate-padding?
		 (*pad 2)
		 (*pad 3))
	       (values #t i j)))))))


(define (base64-decode-update! ctx dst-bv dst-start src-bv src-start src-past)
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
  ;;BASE64-DECODE-BLOCK-LENGTH bytes;  binary data bytes  are written to
  ;;DST-BV in chunks of BASE64-ENCODE-BLOCK-LENGTH bytes.
  ;;

  (define table
    (<base64-decode-ctx>-table ctx))
  (define pad-char
    (<base64-decode-ctx>-pad-char ctx))
  (define expect-padding?
    (<base64-decode-ctx>-expect-padding? ctx))

  (define (%error-invalid-input-byte byte)
    (raise
     (condition (make-armor-invalid-input-byte-condition)
		(make-who-condition 'base64-decode-update!)
		(make-message-condition "invalid input byte while decoding base64 bytevector")
		(make-irritants-condition byte))))

  (define (%error-invalid-pad-area . chars)
    (raise
     (condition (make-armor-invalid-padding-condition)
		(make-who-condition 'base64-decode-update!)
		(make-message-condition "invalid padded block while decoding base64 bytevector")
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

    (cond ((or (< src-len base64-decode-block-length)
	       (< dst-len base64-encode-block-length))
	   (values #f i j))

	  (expect-padding?
	   ;;When  padding   is  on,   there  are  only   the  following
	   ;;possibilities for a 4-bytes block:
	   ;;
	   ;;	XX==
	   ;;	XXX=
	   ;;	XXXX
	   ;;
	   ;;    src0     src1     src2     src3
	   ;;
	   ;; |76543210|76543210|76543210|76543210|
	   ;; |--------+--------+--------+--------|
	   ;; |  765432|  10    |        |        | dst0
	   ;; |        |    7654|  3210  |        | dst1
	   ;; |        |        |      76|  543210| dst2
	   ;;
	   (let* ((char0 (*src))
		  (char1 (*src))
		  (char2 (*src))
		  (char3 (*src))
		  (src0  (%decode char0))
		  (src1  (%decode char1)))
	     (*dst 0 (bitwise-ior (<< src0 2) (>> src1 4)))
	     (if (= char2 pad-char)
		 (if (= char3 pad-char)
		     (values #t i j)
		   (%error-invalid-pad-area char0 char1 char2 char3))
	       (let ((src2 (%decode char2)))
		 (*dst 1 (bitwise-ior (<< src1 4) (>> src2 2)))
		 (if (= char3 pad-char)
		     (values #t i j)
		   (let ((src3 (%decode char3)))
		     (*dst 2 (bitwise-ior (<< src2 6) src3))
		     (loop i j
			   (- src-len base64-decode-block-length)
			   (- dst-len base64-encode-block-length))))))))

	  (else
	   ;;    src0     src1     src2     src3
	   ;;
	   ;; |76543210|76543210|76543210|76543210|
	   ;; |--------+--------+--------+--------|
	   ;; |  765432|  10    |        |        | dst0
	   ;; |        |    7654|  3210  |        | dst1
	   ;; |        |        |      76|  543210| dst2
	   (let ((src0 (%decode (*src)))
		 (src1 (%decode (*src)))
		 (src2 (%decode (*src)))
		 (src3 (%decode (*src))))
	     (*dst 0 (bitwise-ior (<< src0 2) (>> src1 4)))
	     (*dst 1 (bitwise-ior (<< src1 4) (>> src2 2)))
	     (*dst 2 (bitwise-ior (<< src2 6)     src3))
	     (loop i j
		   (- src-len base64-decode-block-length)
		   (- dst-len base64-encode-block-length)))))))


(define (base64-decode-final! ctx dst-bv dst-start src-bv src-start src-past)
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
  ;;BASE64-DECODE-BLOCK-LENGTH bytes;  binary data bytes  are written to
  ;;DST-BV in  chunks of BASE64-ENCODE-BLOCK-LENGTH bytes.   If the last
  ;;characters are  in a chunk of  less than BASE64-DECODE-BLOCK-LENGTH,
  ;;they are interpreted as unpadded characters as specified in CTX.
  ;;

  (define who 'base64-decode-final!)

  (define (%error-invalid-input-length)
    (raise
     (condition (make-armor-invalid-input-length-condition)
		(make-who-condition who)
		(make-message-condition "invalid input length while decoding base64 bytevector"))))

  (if (<base64-decode-ctx>-expect-padding? ctx)

      (let ((src-len (- src-past src-start)))
	(cond ((zero? src-len)
	       (values #t dst-start src-start))
	      ((zero? (mod src-len base64-decode-block-length))
	       (receive (finished? src-next dst-next)
		   (base64-decode-update! ctx dst-bv dst-start src-bv src-start src-past)
		 (cond (finished?		;;a padded block was successfully processed
			(values finished? src-next dst-next))
		       ((= src-next src-past)	;;all the input was successfully processed
			(values #t src-next dst-next))
		       (else			;;output bytevector full
			(values finished? src-next dst-next)))))
	      (else
	       (%error-invalid-input-length))))

    (let ((table (<base64-decode-ctx>-table ctx)))

      (define (%error-invalid-input-byte byte)
	(raise
	 (condition (make-armor-invalid-input-byte-condition)
		    (make-who-condition who)
		    (make-message-condition "invalid input byte while decoding base64 bytevector")
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

	      ((<= base64-decode-block-length src-len)
	       ;; |76543210|76543210|76543210|76543210|
	       ;; |--------+--------+--------+--------|
	       ;; |  765432|  10    |        |        | dst0
	       ;; |        |    7654|  3210  |        | dst1
	       ;; |        |        |      76|  543210| dst2
	       (let ((src0 (*src))
		     (src1 (*src))
		     (src2 (*src))
		     (src3 (*src)))
		 (*dst 0 (bitwise-ior (<< src0 2) (>> src1 4)))
		 (*dst 1 (bitwise-ior (<< src1 4) (>> src2 2)))
		 (*dst 2 (bitwise-ior (<< src2 6)     src3))
		 (loop i j
		       (- src-len base64-decode-block-length)
		       (- dst-len base64-encode-block-length))))

	      ((= 3 src-len)
	       ;; |76543210|76543210|76543210|
	       ;; |--------+--------+--------|
	       ;; |  765432|  10    |        | dst0
	       ;; |        |    7654|  3210  | dst1
	       (let ((src0 (*src))
		     (src1 (*src))
		     (src2 (*src)))
		 (*dst 0 (bitwise-ior (<< src0 2) (>> src1 4)))
		 (*dst 1 (bitwise-ior (<< src1 4) (>> src2 2)))
		 (values #t i j)))

	      ((= 2 src-len)
	       ;; |76543210|76543210|
	       ;; |--------+--------|
	       ;; |  765432|  10    |      dst0
	       (let ((src0 (*src))
		     (src1 (*src)))
		 (*dst 0 (bitwise-ior (<< src0 2) (>> src1 4)))
		 (values #t i j)))

	      (else
	       (%error-invalid-input-length)))))))


;;;; done

)

;;; end of file
