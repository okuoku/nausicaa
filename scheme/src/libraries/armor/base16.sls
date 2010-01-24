;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: base16 encoding/deconding
;;;Date: Sun Jan 24, 2010
;;;
;;;Abstract
;;;
;;;	This library  is derived from the  code in Nettle,  a C language
;;;	library of cryptographic building blocks.
;;;
;;;Copyright (c) 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (C) 2002 Niels Möller
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
    <base16-encode-ctx>
    make-<base16-encode-ctx>		<base16-encode-ctx>?

    <base16-decode-ctx>
    make-<base16-decode-ctx>		<base16-decode-ctx>?

    base16-encode-length		base16-decode-length
    base16-encode-update!		base16-decode-update!
    base16-encode-finished?		base16-decode-finished?)
  (import (rnrs)
    (armor base))


(define-record-type <base16-encode-ctx>
  (parent <armor-ctx>)
  (fields (immutable upper-case?))
  (protocol (lambda (maker)
	      (lambda (upper-case?)
		(let ((p (maker base16-encode-update! base16-encode-finished?)))
		  (p upper-case?))))))

(define-record-type <base16-decode-ctx>
  (parent <armor-ctx>)
  (fields (immutable allow-blanks?)
	  (mutable bits)) ;leftover bits
  (protocol (lambda (maker)
	      (lambda (allow-blanks?)
		(let ((p (maker base16-decode-update! base16-decode-finished?)))
		  (p allow-blanks? #f))))))


(define HEX-DIGITS-UPPER
  (list->vector (map char->integer '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
				     #\A #\B #\C #\D #\E #\F))))

(define HEX-DIGITS-LOWER
  (list->vector (map char->integer '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
				     #\a #\b #\c #\d #\e #\f))))

(define (base16-encode-length src-len)
  (* 2 src-len))

(define (base16-encode-update! ctx dst-bv dst-idx src-bv src-idx src-len)
  (let ((len	(+ src-idx src-len))
	(table	(if (<base16-encode-ctx>-upper-case? ctx)
		    HEX-DIGITS-UPPER
		  HEX-DIGITS-LOWER)))
    (let loop ((i src-idx) (j dst-idx))
      (if (= i len)
	  src-len
	(let ((byte (bytevector-u8-ref src-bv i))
	      (j1   (+ 1 j)))
	  (bytevector-u8-set! dst-bv j (vector-ref table (div byte #x10)))
	  (bytevector-u8-set! dst-bv j1 (vector-ref table (bitwise-and byte #xf)))
	  (loop (+ 1 i) (+ 1 j1)))))))

(define (base16-encode-finished? ctx)
  #t)


(define HEX-DECODE-TABLE
  ;; -1 = invalid
  ;; -2 = space
  '#(-1 -1 -1 -1 -1 -1 -1 -1 -1 -2 -2 -1 -1 -2 -1 -1
	-1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
	-2 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
	0   1  2  3  4  5  6  7  8  9 -1 -1 -1 -1 -1 -1
	-1 10 11 12 13 14 15 -1 -1 -1 -1 -1 -1 -1 -1 -1
	-1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
	-1 10 11 12 13 14 15 -1 -1 -1 -1 -1 -1 -1 -1 -1
	-1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1))

(define (base16-decode-length src-len)
  (div (+ 1 src-len) 2))

(define (base16-decode-update! ctx dst-bv dst-idx src-bv src-idx src-len)
  (let loop ((i 0) (j 0))
    (if (= j src-len)
	i
      (let ((byte (bytevector-u8-ref src-bv (+ j src-idx))))
	(if (>= byte #x80)
	    #f
	  (let ((digit (vector-ref HEX-DECODE-TABLE byte)))
	    (case digit
	      ((-1)
	       (error 'base16-decode-update!
		 "invalid digit base16 in input bytevector" digit))
	      ((-2) ;when accepting white spaces in the input, just skip them
	       (if (<base16-decode-ctx>-allow-blanks? ctx)
		   (loop i (+ 1 j))
		 (error 'base16-decode-update!
		   "invalid digit base16 in input bytevector, blanks not allowed" digit)))
	      (else
	       ;; (assert (>= digit 0))
	       ;; (assert (<  digit #x10))
	       (if (<base16-decode-ctx>-bits ctx)
		   (begin
		     (bytevector-u8-set! dst-bv (+ i dst-idx)
					 (bitwise-ior digit (bitwise-arithmetic-shift-left
							     (<base16-decode-ctx>-bits ctx) 4)))
		     (<base16-decode-ctx>-bits-set! ctx #f)
		     (loop (+ 1 i) (+ 1 j)))
		 (begin
		   (<base16-decode-ctx>-bits-set! ctx digit)
		   (loop i (+ 1 j))))))))))))

(define (base16-decode-finished? ctx)
  (<base16-decode-ctx>-bits ctx))


;;;; done

)

;;; end of file
