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
  (export iconv-open iconv! iconv-close iconv-context?
	  iconv-membuffer!)
  (import (rnrs)
    (compensations)
    (receive)
    (only (foreign cstrings) string->cstring/c)
    (foreign memory)
    (foreign memory membuffers)
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


(define (iconv-open to from)
  (assert (enum-set-subset? from iconv-encoding-universe))
  (assert (enum-set-subset? to   iconv-encoding-universe))
  (with-compensations
    (receive (context* errno)
	(iconv_open (string->cstring/c (%enum-set->string to   'iconv-open))
		    (string->cstring/c (%enum-set->string from 'iconv-open)))
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

(define iconv!
  (case-lambda
   ((context to-block)
    (iconv! context to-block (memblock-null)))
   ((context to-block from-block)
    (assert (iconv-context? context))
    (assert (<memblock>? from-block))
    (assert (<memblock>? to-block))
    (with-compensations
      (let ((from.ptr* (malloc-small/c))
	    (from.len* (malloc-small/c))
	    (to.ptr*   (malloc-small/c))
	    (to.len*   (malloc-small/c)))
	(pointer-set-c-pointer! from.ptr* 0 (<memblock>-pointer from-block))
	(pointer-set-c-size_t!  from.len* 0 (<memblock>-size    from-block))
	(pointer-set-c-pointer! to.ptr*   0 (<memblock>-pointer to-block))
	(pointer-set-c-size_t!  to.len*   0 (<memblock>-size    to-block))
	(receive (code errno)
	    (iconv (<iconv-context>-pointer context) from.ptr* from.len* to.ptr* to.len*)
	  (<memblock>-pointer-set! from-block (pointer-ref-c-pointer from.ptr* 0))
	  (<memblock>-size-set!    from-block (pointer-ref-c-size_t  from.len* 0))
	  (<memblock>-pointer-set! to-block   (pointer-ref-c-pointer to.ptr*   0))
	  (<memblock>-size-set!    to-block   (pointer-ref-c-size_t  to.len*   0))
	  (if (= -1 code)
	      (raise-errno-error 'iconv errno context)
	    code)))))))

(define iconv-membuffer!
  (case-lambda
   ((ctx buffer)
    (iconv-membuffer! ctx buffer (memblock-null)))
   ((ctx buffer in-block)
    (with-compensations
      (let* ((in-tail	(memblock-shallow-clone in-block))
	     (ou	(malloc-memblock/c 1024))
	     (ou-tail	(memblock-shallow-clone ou)))
	(iconv! ctx ou-tail in-tail)
	(let ((ou-head	(memblock&tail-head ou ou-tail)))
	  (membuffer-push-memblock! buffer ou-head)))))))


;;;; done

)

;;; end of file
