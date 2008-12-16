;;;
;;;Part of: Nausicaa/Uriel
;;;Contents: functions for cstrings handling
;;;Date: Tue Dec 16, 2008
;;;Time-stamp: <2008-12-16 16:33:30 marco>
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
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



;;;; setup

(library (uriel cstring)
  (export

    ;;inspection
    strlen strerror

    ;;conversion
    cstring->string		cstring->string/len
    string->cstring		string->cstring/compensated
    string-or-symbol->cstring	string-or-symbol->cstring/compensated
    )
  (import (r6rs)
    (uriel ffi)
    (uriel memory))


;;;; inspection

(define-c-function strlen
  (size_t strlen (pointer)))




;;;; conversion functions

(define (string->cstring s)
  (let* ((bv		(string->utf8 s))
	 (len		(bytevector-length bv))
	 (pointer	(malloc (+ 1 len))))
    (do ((i 0 (+ 1 i)))
	((= i len)
	 (pointer-set-c-char! pointer i 0)
	 pointer)
      (pointer-set-c-char! pointer i (bytevector-s8-ref bv i)))))

(define (string->cstring/compensated s)
  (let* ((bv		(string->utf8 s))
	 (len		(bytevector-length bv))
	 (pointer	(malloc-block/compensated (+ 1 len))))
    (do ((i 0 (+ 1 i)))
	((= i len)
	 (pointer-set-c-char! pointer i 0)
	 pointer)
      (pointer-set-c-char! pointer i (bytevector-s8-ref bv i)))))

(define (string-or-symbol->cstring/compensated s)
  (string->cstring/compensated (if (symbol? s)
				   (symbol->string s)
				 s)))

(define (string-or-symbol->cstring s)
  (string->cstring (if (symbol? s)
		       (symbol->string s)
		     s)))

;;; --------------------------------------------------------------------

(define (cstring->string/len pointer len)
  (let* ((bv	(make-bytevector len)))
    (do ((i 0 (+ 1 i)))
	((= i len)
	 (utf8->string bv))
      (bytevector-s8-set! bv i (pointer-ref-c-signed-char pointer i)))))

(define (cstring->string pointer)
  (cstring->string/len pointer (strlen pointer)))



;;;; miscellaneous functions

(define-c-function primitive-strerror
  (pointer strerror (int)))

(define (strerror errno-value)
  (cstring->string (primitive-strerror errno-value)))



;;;; done

)

;;; end of file
