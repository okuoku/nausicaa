;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: functions for cstrings handling
;;;Date: Tue Dec 16, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008, 2009 Marco Maggi <marcomaggi@gna.org>
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


(library (foreign cstrings)
  (export

    empty-string

    ;;inspection
    strlen
    strcmp			strncmp

    ;;operations
    strcpy			strncpy
    strdup			strndup

    ;;conversion
    string->cstring		cstring->string
    string->cstring/c

    string->memblock		memblock->string
    string->memblock/c

    ;; null-terminated arrays of cstrings
    strings->argv		argv->strings
    argv-length)
  (import (nausicaa)
    (foreign ffi)
    (foreign ffi sizeof)
    (foreign memory)
    (compensations))


;;;; inspection and operations

(define-c-functions libc-shared-object
  (strlen	(size_t strlen (char*)))
  (strcmp	(int strcmp (char* char*)))
  (strncmp	(int strncmp (char* char* size_t)))
  (strcpy	(char* strcpy (char* char*)))
  (strncpy	(char* strncpy (char* char* size_t))))

(define strdup
  (case-lambda
   ((cstring)
    (strdup cstring malloc))
   ((cstring malloc-funk)
    (strndup cstring (strlen cstring) malloc-funk))))

(define strndup
  (case-lambda
   ((cstring size)
    (strndup cstring size malloc))
   ((cstring size malloc)
    (let* ((p	(malloc (+ 1 size))))
      (memcpy p cstring size)
      (pointer-set-c-signed-char! p size 0)
      p))))


;;;; conversion functions: scheme strings

(define string->cstring
  (case-lambda
   ((s malloc)
    (let* ((bv		(string->utf8 (if (symbol? s)
					  (symbol->string s)
					s)))
	   (len		(bytevector-length bv))
	   (pointer	(malloc (+ 1 len))))
      (do ((i 0 (+ 1 i)))
	  ((= i len)
	   (pointer-set-c-signed-char! pointer i 0)
	   pointer)
	(pointer-set-c-signed-char! pointer i (bytevector-s8-ref bv i)))))
   ((s)
    (string->cstring s malloc))))

(define (string->cstring/c s)
  (string->cstring s malloc-block/c))

(define cstring->string
  (case-lambda
   ((pointer)
    (cstring->string pointer (strlen pointer)))
   ((pointer len)
    (let* ((bv	(make-bytevector len)))
      (do ((i 0 (+ 1 i)))
	  ((= i len)
	   (utf8->string bv))
	(bytevector-s8-set! bv i (pointer-ref-c-signed-char pointer i)))))))

(define empty-string
  (string->cstring ""))


;;;; conversion functions: <memblock> records

(define string->memblock
  (case-lambda
   ((s malloc)
    (let* ((bv		(string->utf8 (if (symbol? s)
					  (symbol->string s)
					s)))
	   (len		(bytevector-length bv))
	   (pointer	(malloc len)))
      (do ((i 0 (+ 1 i)))
	  ((= i len)
	   (make-<memblock> pointer len len))
	(pointer-set-c-signed-char! pointer i (bytevector-s8-ref bv i)))))
   ((s)
    (string->cstring s malloc))))

(define (string->memblock/c s)
  (string->memblock s malloc-block/c))

(define memblock->string
  (case-lambda
   ((mb)
    (memblock->string mb (<memblock>-size mb)))
   ((mb len)
    (let* ((pointer	(<memblock>-pointer mb))
	   (bv		(make-bytevector len)))
      (do ((i 0 (+ 1 i)))
	  ((= i len)
	   (utf8->string bv))
	(bytevector-s8-set! bv i (pointer-ref-c-signed-char pointer i)))))))


;;;; null-terminated arrays of strings

(define strings->argv
  (case-lambda
   ((args)
    (strings->argv args malloc))
   ((args malloc-func)
    (let* ((args	(map
			    (lambda (s)
			      (string->cstring s malloc-func))
			  args))
	   (argc	(length args))
	   (argv	(malloc-func (sizeof-pointer-array (+ 1 argc)))))
      (do ((i 0 (+ 1 i))
	   (c args (cdr c)))
	  ((= i argc)
	   (array-set-c-pointer! argv argc pointer-null)
	   argv)
	(array-set-c-pointer! argv i (car c)))))))

(define (argv-length argv)
  (do ((i 0 (+ 1 i)))
      ((pointer-null? (array-ref-c-pointer argv i))
       i)
    #f))

(define argv->strings
  (case-lambda
   ((argv argc)
    (let ((args	'()))
      (do ((i 0 (+ 1 i)))
	  ((= i argc)
	   (reverse args))
	(set! args (cons (cstring->string (array-ref-c-pointer argv i)) args)))))
   ((argv)
    (let ((args	'()))
      (do ((i 0 (+ 1 i)))
	  ((pointer-null? (array-ref-c-pointer argv i))
	   (reverse args))
	(set! args (cons (cstring->string (array-ref-c-pointer argv i)) args)))))))


;;;; done

)

;;; end of file
