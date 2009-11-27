;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Iconv
;;;Contents: primitives for Iconv binding
;;;Date: Fri Nov 27, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(library (foreign i18n iconv primitives)
  (export iconv-open iconv! iconv-close iconv-context?)
  (import (rnrs)
    (compensations)
    (receive)
    (only (foreign cstrings) string->cstring/c)
    (only (foreign memory) malloc-small/c)
    (foreign memory memblocks)
    (foreign errno)
    (only (foreign ffi pointers) pointer->integer)
    (only (foreign ffi peekers-and-pokers)
	  pointer-set-c-pointer!	pointer-set-c-size_t!
	  pointer-ref-c-pointer		pointer-ref-c-size_t
	  pointer-set-c-unsigned-int!	pointer-ref-c-unsigned-int)
    (rename (foreign i18n iconv record-types)
	    (<iconv-context>?	iconv-context?))
    (foreign i18n iconv enumerations)
    (foreign i18n iconv platform)
    (foreign i18n iconv sizeof))


;;;; helpers

(define (%enum-set->string set procname)
  (let ((ell	(enum-set->list set))
	(spec	""))
    (when (memq 'TRANSLIT ell)
      (set! spec "//TRANSLIT")
      (set! ell (remq 'TRANSLIT ell)))
    (when (memq 'IGNORE ell)
      (set! spec (string-append spec "//IGNORE"))
      (set! ell (remq 'IGNORE ell)))
    (if (= 1 (length ell))
	(string-append (symbol->string (car ell)) spec)
      (assertion-violation procname "invalid Iconv enumeration set" set))))


(define (iconv-open from to)
  (assert (enum-set-subset? from iconv-encoding-universe))
  (assert (enum-set-subset? to   iconv-encoding-universe))
  (with-compensations
    (receive (context* errno)
	(iconv_open (string->cstring/c (%enum-set->string from 'iconv-open))
		    (string->cstring/c (%enum-set->string to   'iconv-open)))
      (if (= -1 (pointer->integer context*))
	  (raise-errno-error 'iconv-open errno from to)
	(make-<iconv-context> context* from to)))))

(define (iconv-close context)
  (assert (iconv-context? context))
  (receive (code errno)
      (iconv (<iconv-context>-pointer context))
    (if (= -1 code)
	(raise-errno-error 'iconv-close errno context)
      code)))

(define (iconv! context inbuf oubuf)
  (assert (iconv-context? context))
  (assert (<memblock>? inbuf))
  (assert (<memblock>? oubuf))
  (with-compensations
    (let ((inbuf** (malloc-small/c)) (oubuf** (malloc-small/c))
	  (inlen*  (malloc-small/c)) (oulen*  (malloc-small/c)))
      (pointer-set-c-pointer! inbuf** 0 (<memblock>-pointer inbuf))
      (pointer-set-c-pointer! oubuf** 0 (<memblock>-pointer oubuf))
      (pointer-set-c-size_t!  inlen*  0 (<memblock>-size    inbuf))
      (pointer-set-c-size_t!  oulen*  0 (<memblock>-size    oubuf))
      (write (pointer-ref-c-size_t  oulen*  0))(newline)
      (receive (code errno)
	  (iconv (<iconv-context>-pointer context) inbuf** inlen* oubuf** oulen*)
	(write (pointer-ref-c-size_t  oulen*  0))(newline)
	(write (list 'errno code errno))(newline)
	(<memblock>-pointer-set! inbuf (pointer-ref-c-pointer inbuf** 0))
	(<memblock>-size-set!    inbuf (pointer-ref-c-size_t  inlen*  0))
	(<memblock>-pointer-set! oubuf (pointer-ref-c-pointer oubuf** 0))
	(<memblock>-size-set!    oubuf (pointer-ref-c-size_t  oulen*  0))
	(if (= 0 code)
	    (raise-errno-error 'iconv errno context)
	  code)))))


;;;; done

)

;;; end of file
