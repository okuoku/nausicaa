;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: marshaling interface for streams functions
;;;Date: Fri Nov  6, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009, 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(library (glibc streams primitives)
  (export
    pointer->FILE*	FILE*->pointer	FILE*?

    ferror
    fopen		fclose		fcloseall
    fwrite		fputc		fputs		fflush
    fread		fgetc		fgets
    ungetc
    feof		fseek		fseeko
    ftell		ftello		rewind
    fdopen		fileno

    ferror_unlocked
    fwrite_unlocked	fputc_unlocked	fputs_unlocked	fflush_unlocked
    fread_unlocked	fgetc_unlocked	fgets_unlocked
    feof_unlocked	fileno_unlocked

    popen		pclose

    freadable		fwritable
    freading		fwriting

    getline		getdelim

    (rename (platform:clearerr		clearerr)
	    (platform:clearerr_unlocked	clearerr_unlocked)
	    (platform:fwide		fwide)
	    (platform:fpurge		fpurge)))
  (import (rnrs)
    (receive)
    (begin0)
    (compensations)
    (only (foreign ffi pointers) pointer-null?)
    (only (foreign ffi peekers-and-pokers) pointer-ref-c-pointer)
    (only (foreign memory) malloc-small/c primitive-free)
    (only (foreign cstrings) string->cstring/c cstring->string)
    (only (foreign errno) raise-errno-error)
    (only (posix sizeof) EOF)
    (posix typedefs)
    (prefix (glibc streams platform) platform:))


(define (ferror stream)
  (if (= 0 (platform:ferror (FILE*->pointer stream))) #f #t))

;;; --------------------------------------------------------------------

(define (ferror_unlocked stream)
  (if (= 0 (platform:ferror_unlocked (FILE*->pointer stream))) #f #t))


(define (fopen pathname mode)
  (receive (result errno)
      (with-compensations
	(let ((pathname-cstr	(string->cstring/c pathname))
	      (mode-cstr	(string->cstring/c mode)))
	  (platform:fopen pathname-cstr mode-cstr)))
    (if (pointer-null? result)
	(raise-errno-error 'fopen errno (list pathname mode))
      (pointer->FILE* result))))

(define (fclose stream)
  (receive (result errno)
      (platform:fclose (FILE*->pointer stream))
    (unless (= 0 result)
      (raise-errno-error 'fclose errno stream))
    result))

(define (fcloseall)
  (receive (result errno)
      (platform:fcloseall)
    (unless (= 0 result)
      (raise-errno-error 'fcloseall errno #f))
    result))


(define (fwrite pointer size count stream)
  (receive (result errno)
      (platform:fwrite pointer size count (FILE*->pointer stream))
    (if (= count result)
	result
      (raise-errno-error 'fwrite errno (list pointer size count stream)))))

(define (fputc char stream)
  (let ((c-char (char->integer char)))
    (when (< 255 c-char)
      (assertion-violation 'fputc
	"expected character with scalar value in range [0, 255]" char))
    (receive (result errno)
	(platform:fputc char (FILE*->pointer stream))
      (unless (= char result)
	(raise-errno-error 'fputc errno (list char stream)))
      result)))

(define (fputs string stream)
  (receive (result errno)
      (with-compensations
	(let ((cstring (string->cstring/c string)))
	  (platform:fputs cstring (FILE*->pointer stream))))
    (when (= EOF result)
      (raise-errno-error 'fputs errno (list string stream)))
    result))

(define (fflush stream)
  (receive (result errno)
      (platform:fflush (FILE*->pointer stream))
    (when (= EOF result)
      (raise-errno-error 'fflush errno stream))
    result))

;;; --------------------------------------------------------------------

(define (fwrite_unlocked pointer size count stream)
  (receive (result errno)
      (platform:fwrite_unlocked pointer size count (FILE*->pointer stream))
    (unless (= count result)
      (raise-errno-error 'fwrite_unlocked errno (list pointer size count stream)))
    result))

(define (fputc_unlocked char stream)
  (let ((c-char (char->integer char)))
    (when (< 255 c-char)
      (assertion-violation 'fputc_unlocked
	"expected character with scalar value in range [0, 255]" char))
    (receive (result errno)
	(platform:fputc_unlocked char (FILE*->pointer stream))
      (unless (= c-char result)
	(raise-errno-error 'fputc_unlocked errno (list char stream)))
      result)))

(define (fputs_unlocked string stream)
  (receive (result errno)
      (with-compensations
	(let ((cstring (string->cstring/c string)))
	  (platform:fputs_unlocked cstring (FILE*->pointer stream))))
    (when (= EOF result)
      (raise-errno-error 'fputs_unlocked errno (list string stream)))
    result))

(define (fflush_unlocked stream)
  (receive (result errno)
      (platform:fflush_unlocked (FILE*->pointer stream))
    (when (= EOF result)
      (raise-errno-error 'fflush_unlocked errno stream))
    result))


(define (fread pointer size count stream)
  (receive (result errno)
      (platform:fread pointer size count (FILE*->pointer stream))
    (when (ferror stream)
      (raise-errno-error 'fread errno (list pointer size count stream)))
    result))

(define (fgetc char stream)
  (receive (result errno)
      (platform:fgetc (FILE*->pointer stream))
    (when (= EOF result)
      (raise-errno-error 'fgetc errno stream))
    (integer->char result)))

(define (fgets pointer count stream)
  (receive (result errno)
      (platform:fgets pointer count (FILE*->pointer stream))
    (when (ferror stream)
      (raise-errno-error 'fgets errno (list pointer count stream)))
    result))

;;; --------------------------------------------------------------------

(define (fread_unlocked pointer size count stream)
  (receive (result errno)
      (platform:fread_unlocked pointer size count (FILE*->pointer stream))
    (when (ferror stream)
      (raise-errno-error 'fread_unlocked errno (list pointer size count stream)))
    result))

(define (fgetc_unlocked char stream)
  (receive (result errno)
      (platform:fgetc_unlocked (FILE*->pointer stream))
    (when (= EOF result)
      (raise-errno-error 'fgetc_unlocked errno stream))
    (integer->char result)))

(define (fgets_unlocked pointer count stream)
  (receive (result errno)
      (platform:fgets_unlocked pointer count (FILE*->pointer stream))
    (when (ferror stream)
      (raise-errno-error 'fgets_unlocked errno (list pointer count stream)))
    result))


(define (ungetc char stream)
  (receive (result errno)
      (let ((c-char (char->integer char)))
	(when (< 255 c-char)
	  (assertion-violation 'ungetc
	    "expected character with scalar value in range [0, 255]" char))
	(platform:ungetc char (FILE*->pointer stream)))
    (when (ferror stream)
      (raise-errno-error 'ungetc errno (list char stream)))
    result))


(define (feof stream)
  (receive (result errno)
      (platform:feof (FILE*->pointer stream))
    (when (ferror stream)
      (raise-errno-error 'feof errno stream))
    (not (= 0 result))))

(define (fseek stream offset whence)
  (receive (result errno)
      (platform:fseek (FILE*->pointer stream) offset whence)
    (when (ferror stream)
      (raise-errno-error 'fseek errno stream))
    result))

(define (fseeko stream offset whence)
  (receive (result errno)
      (platform:fseeko (FILE*->pointer stream) offset whence)
    (when (ferror stream)
      (raise-errno-error 'fseeko errno stream))
    result))

(define (ftell stream)
  (receive (result errno)
      (platform:ftell (FILE*->pointer stream))
    (when (ferror stream)
      (raise-errno-error 'ftell errno stream))
    result))

(define (ftello stream)
  (receive (result errno)
      (platform:ftello (FILE*->pointer stream))
    (when (ferror stream)
      (raise-errno-error 'ftello errno stream))
    result))

(define (rewind stream)
  (receive (result errno)
      (platform:rewind (FILE*->pointer stream))
    (when (ferror stream)
      (raise-errno-error 'rewind errno stream))
    result))

;;; --------------------------------------------------------------------

(define (feof_unlocked stream)
  (receive (result errno)
      (platform:feof_unlocked (FILE*->pointer stream))
    (when (ferror stream)
      (raise-errno-error 'feof_unlocked errno stream))
    (not (= 0 result))))


(define (fdopen fd open-mode)
  (receive (result errno)
      (with-compensations
	(let ((mode	(string->cstring/c open-mode)))
	  (platform:fdopen (<fd>->integer fd) mode)))
    (if (pointer-null? result)
	(raise-errno-error 'fdopen errno (list fd open-mode))
      (pointer->FILE* result))))

(define (fileno stream)
  (receive (result errno)
      (platform:fileno (FILE*->pointer stream))
    (when (pointer-null? result)
      (raise-errno-error 'fileno errno stream))
    (integer-><fd> result)))

;;; --------------------------------------------------------------------

(define (fileno_unlocked stream)
  (receive (result errno)
      (platform:fileno_unlocked (FILE*->pointer stream))
    (when (pointer-null? result)
      (raise-errno-error 'fileno_unlocked errno stream))
    result))


(define (popen command mode)
  (with-compensations
    (let ((c-command	(string->cstring/c command))
	  (c-mode	(string->cstring/c mode)))
      (receive (result errno)
	  (platform:popen c-command c-mode)
	(if (pointer-null? result)
	    (raise-errno-error 'popen errno (list command mode))
	  (pointer->FILE* result))))))

(define (pclose stream)
  (receive (result errno)
      (platform:pclose (FILE*->pointer stream))
    (unless (= 0 result)
      (raise-errno-error 'pclose errno stream))
    result))


(define (freadable stream)
  (not (= 0 (platform:freadable (FILE*->pointer stream)))))

(define (fwritable stream)
  (not (= 0 (platform:fwritable (FILE*->pointer stream)))))

(define (freading stream)
  (not (= 0 (platform:freading (FILE*->pointer stream)))))

(define (fwriting stream)
  (not (= 0 (platform:fwriting (FILE*->pointer stream)))))


(define (getline stream)
  (with-compensations
    (let* ((*pointer	(malloc-small/c))
	   (*count	(malloc-small/c))
	   (getp	(lambda ()
			  (pointer-ref-c-pointer *pointer 0)))
	   (free	(lambda ()
			  (let ((p (getp)))
			    (unless (pointer-null? p)
			      (primitive-free p))))))
      (receive (result errno)
	  (platform:getline *pointer *count (FILE*->pointer stream))
	(cond ((ferror stream)
	       (free)
	       (raise-errno-error 'getline errno stream))
	      ((= -1 result)
	       (free)
	       "")
	      (else
	       (let ((p (getp)))
		 (begin0
		     (cstring->string p result)
		   (primitive-free p)))))))))

(define (getdelim stream delimiter)
  (let ((delimiter (char->integer delimiter)))
    (when (< 255 delimiter)
      (assertion-violation 'getdelim
	"expected delimiter with scalar value in range [0, 255]"
	delimiter))
    (with-compensations
      (let* ((*pointer	(malloc-small/c))
	     (*count	(malloc-small/c))
	     (getp	(lambda ()
			  (pointer-ref-c-pointer *pointer 0)))
	     (free	(lambda ()
			  (let ((p (getp)))
			    (unless (pointer-null? p)
			      (primitive-free p))))))
	(receive (result errno)
	    (platform:getdelim *pointer *count delimiter (FILE*->pointer stream))
	  (cond ((ferror stream)
		 (free)
		 (raise-errno-error 'getline errno stream))
		((= -1 result)
		 (free)
		 "")
		(else
		 (let ((p (getp)))
		   (begin0
		       (cstring->string p result)
		     (primitive-free p))))))))))


;;;; done

)

;;; end of file
