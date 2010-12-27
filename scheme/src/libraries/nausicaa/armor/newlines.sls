;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: filters to insert or remove newline characters
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


(library (armor newlines)
  (export
    <newlines-encode-ctx>			<newlines-decode-ctx>
    make-<newlines-encode-ctx>			make-<newlines-decode-ctx>
    <newlines-encode-ctx>?			<newlines-decode-ctx>?
    <newlines-encode-ctx>-sequence		<newlines-decode-ctx>-sequence
    <newlines-encode-ctx>-column
    <newlines-encode-ctx>-width

    newlines-encode-update!			newlines-decode-update!
    newlines-encode-final!			newlines-decode-final!)
  (import (rnrs)
    (language-extensions)
    (classes)
    (armor conditions))


(define-class <newlines-encode-ctx>
  (nongenerative nausicaa:armor:newlines:<newlines-encode-ctx>)
  (fields (immutable width)
	  (immutable sequence)
	  (mutable column)))

(define-class <newlines-decode-ctx>
  (nongenerative nausicaa:armor:newlines:<newlines-decode-ctx>)
  (fields (immutable sequence)))


(define (newlines-encode-update! ctx dst-bv dst-start src-bv src-start src-past)
  ;;Insert newline sequences in data from the bytevector SRC-BV starting
  ;;at index SRC-START (included) up to index SRC-PAST (excluded); store
  ;;the  output   in  the   bytevector  DST-BV  starting   at  DST-START
  ;;(included); insertion is performed according to the specification in
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

  (define column
    (<newlines-encode-ctx>-column ctx))
  (define width
    (<newlines-encode-ctx>-width ctx))
  (define sequence
    (<newlines-encode-ctx>-sequence ctx))
  (define sequence-len
    (bytevector-length sequence))

  (let loop ((i		dst-start)
	     (j		src-start)
	     (src-len	(- src-past src-start))
	     (dst-len	(- (bytevector-length dst-bv) dst-start)))

    (define-inline (*src)
      (begin0
	  (bytevector-u8-ref src-bv j)
	(incr! j)))

    (define-inline (*dst ?expr)
      (bytevector-u8-set! dst-bv i ?expr)
      (incr! i)
      (incr! column))

    (cond ((= column width)
	   (if (< dst-len sequence-len)
	       (begin
		 (<newlines-encode-ctx>-column-set! ctx column)
		 (values i j))
	     (do ((k 0 (+ 1 k)))
		 ((= k sequence-len)
		  (set! column 0)
		  (loop i j src-len (- dst-len sequence-len)))
	       (*dst (bytevector-u8-ref sequence k)))))

	  ((or (zero? src-len) (zero? dst-len))
	   (<newlines-encode-ctx>-column-set! ctx column)
	   (values i j))

	  (else
	   (*dst (*src))
	   (loop i j (- src-len 1) (- dst-len 1))))))


(define (newlines-encode-final! ctx dst-bv dst-start src-bv src-start src-past)
  ;;Insert newline sequences in data from the bytevector SRC-BV starting
  ;;at index SRC-START (included) up to index SRC-PAST (excluded); store
  ;;the  output   in  the   bytevector  DST-BV  starting   at  DST-START
  ;;(included); insertion is performed according to the specification in
  ;;the context CTX.
  ;;
  ;;Return three values:
  ;;
  ;;(1) A boolean: true if all  the data from SRC-BV has been processed,
  ;;false otherwise.
  ;;
  ;;(2) The index  of the next non-written byte in  DST-BV; if DST-BV is
  ;;left full, this value is the length of DST-BV.
  ;;
  ;;(3) The index of the next  non-read byte in SRC-BV; if all the bytes
  ;;from SRC-BV are consumed, this value is SRC-PAST.
  ;;

  (define column
    (<newlines-encode-ctx>-column ctx))
  (define width
    (<newlines-encode-ctx>-width ctx))
  (define sequence
    (<newlines-encode-ctx>-sequence ctx))
  (define sequence-len
    (bytevector-length sequence))

  (let loop ((i		dst-start)
	     (j		src-start)
	     (src-len	(- src-past src-start))
	     (dst-len	(- (bytevector-length dst-bv) dst-start)))

    (define-inline (*src)
      (begin0
	  (bytevector-u8-ref src-bv j)
	(incr! j)))

    (define-inline (*dst ?expr)
      (bytevector-u8-set! dst-bv i ?expr)
      (incr! i)
      (incr! column))

    (cond ((= column width)
	   (if (< dst-len sequence-len)
	       (begin
		 (<newlines-encode-ctx>-column-set! ctx column)
		 (values #f i j))
	     (do ((k 0 (+ 1 k)))
		 ((= k sequence-len)
		  (set! column 0)
		  (loop i j src-len (- dst-len sequence-len)))
	       (*dst (bytevector-u8-ref sequence k)))))

	  ((zero? src-len)
	   (<newlines-encode-ctx>-column-set! ctx column)
	   (values #t i j))

	  ((zero? dst-len)
	   (<newlines-encode-ctx>-column-set! ctx column)
	   (values #f i j))

	  (else
	   (*dst (*src))
	   (loop i j (- src-len 1) (- dst-len 1))))))


(define (newlines-decode-update! ctx dst-bv dst-start src-bv src-start src-past)
  ;;Remove newline sequences in data from the bytevector SRC-BV starting
  ;;at index SRC-START (included) up to index SRC-PAST (excluded); store
  ;;the  output   in  the   bytevector  DST-BV  starting   at  DST-START
  ;;(included); removal  is performed according to  the specification in
  ;;the context CTX.
  ;;
  ;;Return two values:
  ;;
  ;;(1) A  boolean always false.   This value is  here to make  this API
  ;;equal to the one of the base64 decoder.
  ;;
  ;;(2) The index  of the next non-written byte in  DST-BV; if DST-BV is
  ;;left full, this value is the length of DST-BV.
  ;;
  ;;(3) The index of the next  non-read byte in SRC-BV; if all the bytes
  ;;from SRC-BV are consumed, this value is SRC-PAST.
  ;;
  ;;Bytes are read  from SRC-BV until less than  the sequence length are
  ;;present.
  ;;

  (define sequence
    (<newlines-decode-ctx>-sequence ctx))
  (define sequence-len
    (bytevector-length sequence))

  (let loop ((i		dst-start)
	     (j		src-start)
	     (src-len	(- src-past src-start))
	     (dst-len	(- (bytevector-length dst-bv) dst-start)))

    (define-inline (*src)
      (begin0
	  (bytevector-u8-ref src-bv j)
	(incr! j)))

    (define-inline (*dst ?expr)
      (bytevector-u8-set! dst-bv i ?expr)
      (incr! i))

    (define (%compare k)
      (cond ((= k sequence-len)
	     #f)
	    ((= (bytevector-u8-ref src-bv  (+ j k))
		(bytevector-u8-ref sequence k))
	     #t)
	    (else
	     (%compare (+ 1 k)))))

    (cond ((< src-len sequence-len)
	   (values #f i j))

	  ((zero? dst-len)
	   (if (and (<= sequence-len src-len) (%compare 0))
	       (values #f i (+ j sequence-len))
	     (values #f i j)))

	  (else
	   (if (and (<= sequence-len src-len) (%compare 0))
	       (loop i (+ j sequence-len) (- src-len sequence-len) dst-len)
	     (begin
	       (*dst (*src))
	       (loop i j (- src-len 1) (- dst-len 1))))))))


(define (newlines-decode-final! ctx dst-bv dst-start src-bv src-start src-past)
  ;;Remove newline sequences in data from the bytevector SRC-BV starting
  ;;at index SRC-START (included) up to index SRC-PAST (excluded); store
  ;;the  output   in  the   bytevector  DST-BV  starting   at  DST-START
  ;;(included); removal  is performed according to  the specification in
  ;;the context CTX.
  ;;
  ;;Return two values:
  ;;
  ;;(1) A boolean: true if all  the input data has been processed, false
  ;;if the output bytevector was filled before flushing all the bytes.
  ;;
  ;;(2) The index  of the next non-written byte in  DST-BV; if DST-BV is
  ;;left full, this value is the length of DST-BV.
  ;;
  ;;(3) The index of the next  non-read byte in SRC-BV; if all the bytes
  ;;from SRC-BV are consumed, this value is SRC-PAST.
  ;;

  (define sequence
    (<newlines-decode-ctx>-sequence ctx))
  (define sequence-len
    (bytevector-length sequence))

  (let loop ((i		dst-start)
	     (j		src-start)
	     (src-len	(- src-past src-start))
	     (dst-len	(- (bytevector-length dst-bv) dst-start)))

    (define-inline (*src)
      (begin0
	  (bytevector-u8-ref src-bv j)
	(incr! j)))

    (define-inline (*dst ?expr)
      (bytevector-u8-set! dst-bv i ?expr)
      (incr! i))

    (define (%compare k)
      (cond ((= k sequence-len)
	     #f)
	    ((= (bytevector-u8-ref src-bv  (+ j k))
		(bytevector-u8-ref sequence k))
	     #t)
	    (else
	     (%compare (+ 1 k)))))

    (cond ((zero? src-len)
	   (values #t i j))

	  ((zero? dst-len)
	   (if (and (<= sequence-len src-len) (%compare 0))
	       (values (= sequence-len src-len) i (+ j sequence-len))
	     (values #f i j)))

	  ((<= sequence-len src-len)
	   (if (and (<= sequence-len src-len) (%compare 0))
	       (loop i (+ j sequence-len) (- src-len sequence-len) dst-len)
	     (begin
	       (*dst (*src))
	       (loop i j (- src-len 1) (- dst-len 1)))))

	  (else
	   (assert (< src-len sequence-len))
	   (do ((k 0 (+ 1 k)))
	       ((= k src-len)
		(values #t i j))
	     (*dst (*src)))))))


;;;; done

)

;;; end of file
