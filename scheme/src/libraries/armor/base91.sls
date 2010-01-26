;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: ASCII armor base91
;;;Date: Tue Jan 26, 2010
;;;
;;;Abstract
;;;
;;;	This is a modified version of the code in basE91.
;;;
;;;Copyright (c) 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;Original C language code is:
;;;Copyright (c) 2000-2005 Joachim Henke <j-o@users.sourceforge.net>
;;;Original code modified in 2006, 2008 by Marco Maggi
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
;;;
;;;Original C language license terms
;;;---------------------------------
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

    <base91-encode-ctx>			<base91-decode-ctx>
    make-<base91-encode-ctx>		make-<base91-decode-ctx>
    <base91-encode-ctx>?		<base91-decode-ctx>?

    base91-encode-update!		base91-decode-update!
    base91-encode-final!		base91-decode-final!

    base91-encode-length		base91-decode-length

    base91-encode-finished?		base91-decode-finished?)
  (import (rnrs)
    (receive))


(define-record-type <base91-encode-ctx>
  (fields (mutable queue)
	  (mutable nbits))
  (protocol (lambda (maker)
	      (lambda ()
		(maker 0 0)))))

(define-record-type <base91-decode-ctx>
  (fields (mutable queue)
	  (mutable nbits)
	  (mutable val))
  (protocol (lambda (maker)
	      (lambda ()
		(maker 0 0 -1)))))


(define << bitwise-arithmetic-shift-left)
(define >> bitwise-arithmetic-shift-right)

(define-syntax incr!
  (syntax-rules ()
    ((_ ?id)
     (set! ?id (+ ?id 1)))
    ((_ ?id ?delta)
     (set! ?id (+ ?id ?delta)))))

(define-syntax decr!
  (syntax-rules ()
    ((_ ?id)
     (set! ?id (- ?id 1)))
    ((_ ?id ?delta)
     (set! ?id (- ?id ?delta)))))

(define-syntax define-encode-accessor
  (lambda (stx)
    (define (%field->accessor field-stx)
      (string->symbol (string-append "<base91-encode-ctx>-"
				     (symbol->string (syntax->datum field-stx)))))
    (define (%field->mutator field-stx)
      (string->symbol (string-append "<base91-encode-ctx>-"
				     (symbol->string (syntax->datum field-stx))
				     "-set!")))
    (syntax-case stx ()
      ((_ ?record ?field)
       (with-syntax ((ACCESSOR	(datum->syntax #'field (%field->accessor #'?field)))
		     (MUTATOR	(datum->syntax #'field (%field->mutator  #'?field))))
       #'(define-syntax ?field
	   (identifier-syntax
	    (?id
	     (ACCESSOR ?record))
	    ((set! ?id ?e)
	     (MUTATOR ?record ?e)))))))))

(define-syntax define-decode-accessor
  (lambda (stx)
    (define (%field->accessor field-stx)
      (string->symbol (string-append "<base91-decode-ctx>-"
				     (symbol->string (syntax->datum field-stx)))))
    (define (%field->mutator field-stx)
      (string->symbol (string-append "<base91-decode-ctx>-"
				     (symbol->string (syntax->datum field-stx))
				     "-set!")))
    (syntax-case stx ()
      ((_ ?record ?field)
       (with-syntax ((ACCESSOR	(datum->syntax #'field (%field->accessor #'?field)))
		     (MUTATOR	(datum->syntax #'field (%field->mutator  #'?field))))
       #'(define-syntax ?field
	   (identifier-syntax
	    (?id
	     (ACCESSOR ?record))
	    ((set! ?id ?e)
	     (MUTATOR ?record ?e)))))))))


(define encode-table
  (list->vector
   (map char->integer
     '(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
       #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z
       #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
       #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z
       #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\! #\# #\$
       #\% #\& #\( #\) #\* #\+ #\, #\. #\/ #\: #\; #\< #\=
       #\> #\? #\@ #\[ #\] #\^ #\_ #\` #\{ #\| #\} #\~ #\"))))

(define-syntax %encode
  (syntax-rules ()
    ((_ ?idx)
     (vector-ref encode-table ?idx))))

(define decode-table
  '#(#xff #xff #xff #xff #xff #xff #xff #xff #xff #xff
	  #xff #xff #xff #xff #xff #xff #xff #xff #xff #xff
	  #xff #xff #xff #xff #xff #xff #xff #xff #xff #xff
	  #xff #xff #xff #x3e #x5a #x3f #x40 #x41 #x42 #xff
	  #x43 #x44 #x45 #x46 #x47 #xff #x48 #x49 #x34 #x35
	  #x36 #x37 #x38 #x39 #x3a #x3b #x3c #x3d #x4a #x4b
	  #x4c #x4d #x4e #x4f #x50 #x00 #x01 #x02 #x03 #x04
	  #x05 #x06 #x07 #x08 #x09 #x0a #x0b #x0c #x0d #x0e
	  #x0f #x10 #x11 #x12 #x13 #x14 #x15 #x16 #x17 #x18
	  #x19 #x51 #xff #x52 #x53 #x54 #x55 #x1a #x1b #x1c
	  #x1d #x1e #x1f #x20 #x21 #x22 #x23 #x24 #x25 #x26
	  #x27 #x28 #x29 #x2a #x2b #x2c #x2d #x2e #x2f #x30
	  #x31 #x32 #x33 #x56 #x57 #x58 #x59 #xff #xff #xff
	  #xff #xff #xff #xff #xff #xff #xff #xff #xff #xff
	  #xff #xff #xff #xff #xff #xff #xff #xff #xff #xff
	  #xff #xff #xff #xff #xff #xff #xff #xff #xff #xff
	  #xff #xff #xff #xff #xff #xff #xff #xff #xff #xff
	  #xff #xff #xff #xff #xff #xff #xff #xff #xff #xff
	  #xff #xff #xff #xff #xff #xff #xff #xff #xff #xff
	  #xff #xff #xff #xff #xff #xff #xff #xff #xff #xff
	  #xff #xff #xff #xff #xff #xff #xff #xff #xff #xff
	  #xff #xff #xff #xff #xff #xff #xff #xff #xff #xff
	  #xff #xff #xff #xff #xff #xff #xff #xff #xff #xff
	  #xff #xff #xff #xff #xff #xff #xff #xff #xff #xff
	  #xff #xff #xff #xff #xff #xff #xff #xff #xff #xff
	  #xff #xff #xff #xff #xff #xff))

(define-syntax %decode
  (syntax-rules ()
    ((_ ?idx)
     (vector-ref decode-table ?idx))))

(define blanks
  (map char->integer '(#\newline #\return #\tab #\vtab #\space #\page)))


;;;These  are not  efficient  because they  overestimate  the amount  of
;;;memory,  but  the original  package  does  not  include functions  to
;;;compute a prediction of the maximum size.

(define (base91-encode-length len)
  (+ 2 (* 2 len)))

(define (base91-decode-length len)
  (+ 1 len))


(define (base91-encode-update! ctx dst-bv dst-start src-bv src-start src-len)
  (define-encode-accessor ctx queue)
  (define-encode-accessor ctx nbits)
  (let loop ((i 0) (j 0))
    (if (= j src-len)
	i
      (let ((byte (bytevector-u8-ref src-bv (+ j src-start))))
	(set! queue (bitwise-ior queue (<< byte nbits)))
	(incr! nbits 8)
	(if (< 13 nbits)
	    (let ((v (bitwise-and queue #x1FFF)))
	      (if (< 88 v)
		  (begin
		    (set! queue (>> queue 13))
		    (decr! nbits 13))
		(begin
		  (set! v (bitwise-and queue #x3FFF))
		  (set! queue (>> queue 14))
		  (decr! nbits 14)))
	      (bytevector-u8-set! dst-bv (+ i dst-start) (%encode (mod v 91)))
	      (incr! i)
	      (bytevector-u8-set! dst-bv (+ i dst-start) (%encode (div v 91)))
	      (loop (+ 1 i) (+ 1 j)))
	  (loop i (+ 1 j)))))))

(define (base91-encode-final! ctx dst-bv dst-start)
  (let ((n (<base91-encode-ctx>-nbits ctx)))
    (if (= 0 n)
	0
      (let ((q (<base91-encode-ctx>-queue ctx)))
	(<base91-encode-ctx>-nbits-set! ctx 0)
	(bytevector-u8-set! dst-bv dst-start (%encode (mod q 91)))
	(if (or (< 7 n) (< 90 q))
	    (begin
	      (bytevector-u8-set! dst-bv (+ 1 dst-start) (%encode (div q 91)))
	      2)
	  1)))))

(define (base91-encode-finished? ctx)
  (= 0 (<base91-encode-ctx>-nbits ctx)))


(define (base91-decode-update! ctx dst-bv dst-start src-bv src-start src-len)
  (define-decode-accessor ctx queue)
  (define-decode-accessor ctx nbits)
  (define-decode-accessor ctx val)
  (let loop ((i 0) (j 0))
    (if (= j src-len)
	i
      (let ((byte (%decode (bytevector-u8-ref src-bv (+ j src-start)))))
	(cond ((= #xFF byte)
	       (loop i (+ 1 j)))

	      ((= -1 val) ;start next value
	       (set! val byte)
	       (loop i (+ 1 j)))

	      (else
	       (incr! val (* 91 byte))
	       (set! queue (bitwise-and #xFFFFFF (bitwise-ior queue (<< val nbits))))
	       (incr! nbits (if (< 88 (bitwise-and val #x1FFF)) 13 14))
	       (let inner ()
		 (when (< 7 nbits)
		   (bytevector-u8-set! dst-bv (+ i dst-start) (bitwise-and #xFF queue))
		   (incr! i)
		   (set! queue (>> queue 8))
		   (decr! nbits 8)
		   (inner)))
	       (set! val -1)
	       (loop i (+ 1 j))))))))

(define (base91-decode-final! ctx dst-bv dst-start)
  ;;Process remaining bits; write at most 1 byte.
  ;;
  (let ((val (<base91-decode-ctx>-val ctx)))
    (if (= -1 val)
	0
      (begin
	(bytevector-u8-set! dst-bv dst-start
			    (bitwise-ior (<base91-decode-ctx>-queue ctx)
					 (<< val (<base91-decode-ctx>-nbits ctx))))
	(<base91-decode-ctx>-val-set! ctx -1)
	1))))

(define (base91-decode-finished? ctx)
  (= -1 (<base91-decode-ctx>-val ctx)))


;;;; done

)

;;; end of file
