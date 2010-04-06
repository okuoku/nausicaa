;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: quoted-printable encoding/deconding
;;;Date: Fri Mar 12, 2010
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


(library (armor quoted-printable)
  (export
    <qprint-encode-ctx>			<qprint-decode-ctx>
    make-<qprint-encode-ctx>		make-<qprint-decode-ctx>
    <qprint-encode-ctx>?		<qprint-decode-ctx>?
    <qprint-encode-ctx>-encoding

    qprint-encode-update!		qprint-decode-update!
    qprint-encode-final!		qprint-decode-final!

					qprint-decode-block-length

    <qprint-encode-ctx>-with-record-fields-of
    <qprint-decode-ctx>-with-record-fields-of

    armored-byte-of-qprint?		armored-byte-of-qprint?/strong)
  (import (rnrs)
    (language-extensions)
    (classes)
    (armor conditions))


(define-class <qprint-encode-ctx>
  (nongenerative nausicaa:armor:quoted-printable:<qprint-encode-ctx>)
  (fields (immutable encoding))
  (protocol
   (lambda (make-<top>)
     (lambda (encoding)
       (let ((encoding (case encoding
			 ((default)		'default)
			 ((strong)		'strong	)
			 (else
			  (assertion-violation 'make-<qprint-encode-ctx>
			    "invalid encoding selection for qprint encoder" encoding)))))
	 ((make-<top>) encoding))))))

(define-class <qprint-decode-ctx>
  (nongenerative nausicaa:armor:quoted-printable:<qprint-decode-ctx>))


;;;; encoding tables

;;The following  is the  encoding table for  qprint as specified  by RFC
;;2045, upper case.

(define encode-alphabet-qprint
  '#(#\0 #\1 #\2   #\3 #\4 #\5   #\6 #\7 #\8   #\9    ;;  0 -  9
     #\A #\B #\C   #\D #\E #\F))                      ;; 10 - 15

(define encode-table-qprint
  (vector-map char->integer encode-alphabet-qprint))


;;;; decoding tables

(define (alphabet->table! alphabet table)
  (dotimes (i (vector-length alphabet) table)
    (vector-set! table (char->integer (vector-ref alphabet i)) i)))

;;Decoding tables for base 16:  disallowed input characters have a value
;;of #f; only 128 chars, as everything above 127 (#x80) is #f.

(define decode-table-qprint
  (alphabet->table! encode-alphabet-qprint (make-vector 128 #f)))

(define (armored-byte-of-qprint? byte)
  (and (integer? byte)
       (exact? byte)
       (or (<= bang-char byte less-char)
	   (<= greater-char byte tilde-char))))

(define (armored-byte-of-qprint?/strong byte)
  (and (integer? byte)
       (exact? byte)
       (or (<= zero-char byte nine-char)
	   (<= alower-char byte zlower-char)
	   (<= aupper-char byte zupper-char))))


;;;; helpers

(define equal-char	(char->integer #\=))
(define bang-char	(char->integer #\!))
(define less-char	(char->integer #\<))
(define greater-char	(char->integer #\>))
(define tilde-char	(char->integer #\~))
(define zero-char	(char->integer #\0))
(define nine-char	(char->integer #\9))
(define alower-char	(char->integer #\a))
(define zlower-char	(char->integer #\z))
(define aupper-char	(char->integer #\A))
(define zupper-char	(char->integer #\Z))

(define-inline (lower4 ?number)
  (bitwise-and #b00001111 ?number))

(define-inline (lower8 ?number)
  (bitwise-and #xFF ?number))

(define << bitwise-arithmetic-shift-left)
(define >> bitwise-arithmetic-shift-right)

(define qprint-decode-block-length 3)


(define (qprint-encode-update! ctx dst-bv dst-start src-bv src-start src-past)
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
  ;;characters are written  to DST-BV as single bytes  or as triplets of
  ;;bytes.
  ;;

  (define default-encoding?
    (eq? 'default (<qprint-encode-ctx>-encoding ctx)))

  (let loop ((i		dst-start)
	     (j		src-start)
	     (src-len	(- src-past src-start))
	     (dst-len	(- (bytevector-length dst-bv) dst-start)))

    (define-inline (*src)
      (begin0
	  (bytevector-u8-ref src-bv j)
	(incr! j)))

    (define-inline (*dst ?dummy ?expr)
      (bytevector-u8-set! dst-bv i ?expr)
      (incr! i))

    (define-inline (*dst= ?dummy)
      (bytevector-u8-set! dst-bv i equal-char)
      (incr! i))

    (define-inline (%encode ?expr)
      (vector-ref encode-table-qprint (lower4 ?expr)))

    (if (or (zero? src-len)
	    (< dst-len qprint-decode-block-length))
	(values i j)
      ;;    src0
      ;;
      ;; |76543210|
      ;; |--------|
      ;; |3210    | dst0
      ;; |    3210| dst1
      (let ((src0 (*src)))
	(if (if default-encoding?
		(or (<= bang-char src0 less-char)
		    (<= greater-char src0 tilde-char))
	      (or (<= zero-char src0 nine-char)
		  (<= alower-char src0 zlower-char)
		  (<= aupper-char src0 zupper-char)))
	    (begin
	      (*dst 0 src0)
	      (loop i j (- src-len 1) (- dst-len 1)))
	  (begin
	    (*dst= 0)
	    (*dst 1 (%encode (>> src0 4)))
	    (*dst 2 (%encode     src0))
	    (loop i j
		  (- src-len 1)
		  (- dst-len qprint-decode-block-length))))))))


(define (qprint-encode-final! ctx dst-bv dst-start src-bv src-start src-past)
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
  ;;characters are written  to DST-BV as single bytes  or as triplets of
  ;;bytes.
  ;;

  (define default-encoding?
    (eq? 'default (<qprint-encode-ctx>-encoding ctx)))

  (let loop ((i		dst-start)
	     (j		src-start)
	     (src-len	(- src-past src-start))
	     (dst-len	(- (bytevector-length dst-bv) dst-start)))

    (define-inline (*src)
      (begin0
	  (bytevector-u8-ref src-bv j)
	(incr! j)))

    (define-inline (*dst ?dummy ?expr)
      (bytevector-u8-set! dst-bv i ?expr)
      (incr! i))

    (define-inline (*dst= ?dummy)
      (bytevector-u8-set! dst-bv i equal-char)
      (incr! i))

    (define-inline (%encode ?expr)
      (vector-ref encode-table-qprint (lower4 ?expr)))

    (if (or (zero? src-len)
	    (< dst-len qprint-decode-block-length))
	(values #t i j)
      ;;    src0
      ;;
      ;; |76543210|
      ;; |--------|
      ;; |3210    | dst0
      ;; |    3210| dst1
      (let ((src0 (*src)))
	(if (if default-encoding?
		(or (<= bang-char src0 less-char)
		    (<= greater-char src0 tilde-char))
	      (or (<= zero-char src0 nine-char)
		  (<= alower-char src0 zlower-char)
		  (<= aupper-char src0 zupper-char)))
	    (begin
	      (*dst 0 src0)
	      (loop i j (- src-len 1) (- dst-len 1)))
	  (begin
	    (*dst= 0)
	    (*dst 1 (%encode (>> src0 4)))
	    (*dst 2 (%encode     src0))
	    (loop i j
		  (- src-len 1)
		  (- dst-len qprint-decode-block-length))))))))


(define (qprint-decode-update! ctx dst-bv dst-start src-bv src-start src-past)
  ;;Decode ASCII characters from the bytevector SRC-BV starting at index
  ;;SRC-START (included)  up to index SRC-PAST  (excluded); store binary
  ;;data  bytes   in  the   bytevector  DST-BV  starting   at  DST-START
  ;;(included); decoding is performed  according to the specification in
  ;;the context CTX.
  ;;
  ;;Return three values:
  ;;
  ;;(1)  A boolean always  false.  This  value exists  to make  this API
  ;;equal to the one of base64.
  ;;
  ;;(2) The index  of the next non-written byte in  DST-BV; if DST-BV is
  ;;filled to the end, this value is the length of DST-BV.
  ;;
  ;;(3)  The index  of the  next  non-read byte  in SRC-BV;  if all  the
  ;;characters from SRC-BV are consumed, this value is SRC-PAST.
  ;;
  ;;ASCII characters are read from  SRC-BV in 3-byte blocks; binary data
  ;;bytes are written to DST-BV in single-byte blocks.
  ;;

  (define (%error-invalid-input-byte byte)
    (raise
     (condition (make-armor-invalid-input-byte-condition)
		(make-who-condition 'qprint-decode-update!)
		(make-message-condition "invalid input byte while decoding qprint bytevector")
		(make-irritants-condition byte))))

  (define (%decode char)
    (if (zero? (bitwise-and #x80 char))
	(let ((byte (vector-ref decode-table-qprint char)))
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

    (if (or (< src-len qprint-decode-block-length)
	    (zero? dst-len))
	(values #f i j)
      (let ((src0 (*src)))
	(if (= equal-char src0)
	    (let* ((src1 (%decode (*src))) ;we need LET* to enforce the order
		   (src2 (%decode (*src))))
	      ;;    src0     src1
	      ;;
	      ;; |76543210|76543210|
	      ;; |--------+--------|
	      ;; |    7654|        | dst0
	      ;; |        |    3210| dst1
	      (*dst 0 (bitwise-ior (<< src1 4) src2))
	      (loop i j
		    (- src-len qprint-decode-block-length)
		    (- dst-len 1)))
	  (begin
	    (*dst 0 src0)
	    (loop i j (- src-len 1) (- dst-len 1))))))))


(define (qprint-decode-final! ctx dst-bv dst-start src-bv src-start src-past)
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
  ;;ASCII characters  are read from  SRC-BV in single-byte blocks  or in
  ;;chunks  of 3  bytes;  binary data  bytes  are written  to DST-BV  in
  ;;single-byte blocks.
  ;;

  (define who 'qprint-decode-final!)

  (define (%error-invalid-input-length)
    (raise
     (condition (make-armor-invalid-input-length-condition)
		(make-who-condition who)
		(make-message-condition "invalid input length while decoding qprint bytevector"))))

  (define (%error-invalid-input-byte byte)
    (raise
     (condition (make-armor-invalid-input-byte-condition)
		(make-who-condition who)
		(make-message-condition "invalid input byte while decoding qprint bytevector")
		(make-irritants-condition byte))))

  (define (%decode char)
    (if (zero? (bitwise-and #x80 char))
	(let ((byte (vector-ref decode-table-qprint char)))
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

    (cond ((zero? src-len)
	   (values #t i j))

	  ((zero? dst-len)
	   (values #f i j))

	  ((< src-len qprint-decode-block-length)
	   (*dst 0 (*src))
	   (loop i j (- src-len 1) (- dst-len 1)))

	  (else
	   (let ((src0 (*src)))
	     (if (= equal-char src0)
		 (let* ((src1 (%decode (*src))) ;we need LET* to enforce the order
			(src2 (%decode (*src))))
		   ;;    src0     src1
		   ;;
		   ;; |76543210|76543210|
		   ;; |--------+--------|
		   ;; |    7654|        | dst0
		   ;; |        |    3210| dst1
		   (*dst 0 (bitwise-ior (<< src1 4) src2))
		   (loop i j
			 (- src-len qprint-decode-block-length)
			 (- dst-len 1)))
	       (begin
		 (*dst 0 src0)
		 (loop i j (- src-len 1) (- dst-len 1)))))))))


;;;; done

)

;;; end of file
