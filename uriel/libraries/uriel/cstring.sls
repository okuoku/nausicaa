;;;
;;;Part of: Nausicaa/Uriel
;;;Contents: functions for cstrings handling
;;;Date: Tue Dec 16, 2008
;;;Time-stamp: <2008-12-17 21:24:59 marco>
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
    strlen
    strcmp			strncmp
    memchr			memrchr
    strchr			strrchr
    strstr			memmem

    ;;operations
    strcpy			strncpy
    strdup			strndup

    ;;conversion
    cstring->string		cstring->string/len
    string->cstring/c		string->cstring)
  (import (r6rs)
    (uriel lang)
    (uriel ffi)
    (uriel memory))


;;;; inspection

(define-c-function strlen
  (size_t strlen (char*)))

(define-c-function strcmp
  (int strcmp (char* char*)))

(define-c-function strncmp
  (int strncmp (char* char* size_t)))

(define-c-function memchr
  (char* memchr (char* int size_t)))

(define-c-function memrchr
  (char* memrchr (char* int size_t)))

(define-c-function strchr
  (char* strchr (char* int)))

(define-c-function strrchr
  (char* strchr (char* int)))

(define-c-function strstr
  (char* strstr (char* int)))

(define-c-function memmem
  (void* memmem (void* size_t void* size_t)))



;;;; operations

(define-c-function strcpy
  (char* strcpy (char* char*)))

(define-c-function strncpy
  (char* strcpy (char* char* size_t)))

(define strdup
  (case-lambda
   ((cstring)
    (strdup cstring malloc))
   ((cstring malloc)
    (strndup cstring (strlen cstring) malloc))))

(define strndup
  (case-lambda
   ((cstring size)
    (strndup cstring size malloc))
   ((cstring size malloc)
    (let* ((p	(malloc (+ 1 size))))
      (strncpy p cstring size)
      p))))



;;;; conversion functions

(define string->cstring
  (case-lambda
   ((s malloc)
    (let* ((bv		(string->utf8 (symbol->string/maybe s)))
	   (len		(bytevector-length bv))
	   (pointer	(malloc (+ 1 len))))
      (do ((i 0 (+ 1 i)))
	  ((= i len)
	   (pointer-set-c-char! pointer i 0)
	   pointer)
	(pointer-set-c-char! pointer i (bytevector-s8-ref bv i)))))
   ((s)
    (string->cstring s malloc))))

(define (string->cstring/c s)
  (string->cstring s malloc-block/c))

(define (cstring->string/len pointer len)
  (let* ((bv	(make-bytevector len)))
    (do ((i 0 (+ 1 i)))
	((= i len)
	 (utf8->string bv))
      (bytevector-s8-set! bv i (pointer-ref-c-signed-char pointer i)))))

(define (cstring->string pointer)
  (cstring->string/len pointer (strlen pointer)))


;;;; done

)

;;; end of file
