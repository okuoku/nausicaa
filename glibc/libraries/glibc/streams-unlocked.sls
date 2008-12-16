;;;
;;;Part of: Glibc libraries for R6RS Scheme
;;;Contents: interface to unlocked stream functions
;;;Date: Thu Dec  4, 2008
;;;Time-stamp: <2008-12-16 10:01:04 marco>
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

(library (glibc streams-unlocked)
  (export
    ferror_unlocked		primitive-ferror_unlocked
    clearerr_unlocked

    fwrite_unlocked		primitive-fwrite_unlocked
    fputc_unlocked		primitive-fputc_unlocked
    fputs_unlocked		primitive-fputs_unlocked
    fflush_unlocked		primitive-fflush_unlocked

    fread_unlocked		primitive-fread_unlocked
    fgetc_unlocked		primitive-fgetc_unlocked
    fgets_unlocked		primitive-fgets_unlocked

    feof_unlocked		primitive-feof_unlocked

    fileno_unlocked		primitive-fileno_unlocked)
  (import (r6rs)
    (srfi receive)
    (uriel lang)
    (rename (uriel ffi)
	    (string-or-symbol->cstring/compensated s->c))
    (glibc sizeof))

(define libc
  (begin
    (shared-object self-shared-object)
    self-shared-object))



;;;; errors

(define-c-function primitive-ferror_unlocked
  (int ferror_unlocked (FILE*)))

(define-c-function clearerr_unlocked
  (int clearerr_unlocked (FILE*)))

(define (ferror_unlocked stream)
  (if (= 0 (primitive-ferror_unlocked stream)) #f #t))


;;;; writing

(define-c-function/with-errno primitive-fwrite_unlocked
  (size_t fwrite_unlocked (void* size_t size_t FILE*)))

(define-c-function/with-errno primitive-fputc_unlocked
  (int fputc_unlocked (int FILE*)))

(define-c-function/with-errno primitive-fputs_unlocked
  (int fputs (char* FILE*)))

(define-c-function/with-errno primitive-fflush_unlocked
  (int fflush (FILE*)))

(define (fwrite_unlocked pointer size count stream)
  (receive (result errno)
      (primitive-fwrite_unlocked pointer size count stream)
    (unless (= count result)
      (raise-errno-error 'fwrite_unlocked errno (list pointer size count stream)))
    result))

(define (fputc_unlocked char stream)
  (let ((c-char (char->integer char)))
    (when (< 255 c-char)
      (assertion-violation 'fputc_unlocked
	"expected character with scalar value in range [0, 255]" char))
    (receive (result errno)
	(primitive-fputc_unlocked char stream)
      (unless (= c-char result)
	(raise-errno-error 'fputc_unlocked errno (list char stream)))
      result)))

(define (fputs_unlocked string stream)
  (receive (result errno)
      (with-compensations
	(let ((cstring (s->c string)))
	  (primitive-fwrite_unlocked cstring stream)))
    (when (= valueof-eof result)
      (raise-errno-error 'fputs_unlocked errno (list string stream)))
    result))

(define (fflush_unlocked stream)
  (receive (result errno)
      (primitive-fflush_unlocked stream)
    (when (= valueof-eof result)
      (raise-errno-error 'fputc_unlocked errno stream))
    result))



;;;; reading

(define-c-function/with-errno primitive-fread_unlocked
  (size_t fwrite_unlocked (void* size_t size_t FILE*)))

(define-c-function/with-errno primitive-fgetc_unlocked
  (int fgetc (FILE*)))

(define-c-function/with-errno primitive-fgets_unlocked
  (char* fgets (char* int FILE*)))

(define (fread_unlocked pointer size count stream)
  (receive (result errno)
      (primitive-fread_unlocked pointer size count stream)
    (when (ferror_unlocked stream)
      (raise-errno-error 'fread_unlocked errno (list pointer size count stream)))
    result))

(define (fgetc_unlocked char stream)
  (receive (result errno)
      (primitive-fgetc_unlocked stream)
    (when (= valueof-eof result)
      (raise-errno-error 'fgetc_unlocked errno stream))
    (integer->char result)))

(define (fgets_unlocked pointer count stream)
  (receive (result errno)
      (primitive-fread_unlocked pointer count stream)
    (when (ferror_unlocked stream)
      (raise-errno-error 'fgets_unlocked errno (list pointer count stream)))
    result))



;;;; seeking

(define-c-function/with-errno primitive-feof_unlocked
  (int feof_unlocked (FILE*)))

(define (feof_unlocked stream)
  (receive (result errno)
      (primitive-feof_unlocked stream)
    (when (ferror_unlocked stream)
      (raise-errno-error 'feof_unlocked errno stream))
    result))



;;;; streams and file descriptors

(define-c-function/with-errno primitive-fileno_unlocked
  (int fileno_unlocked (FILE*)))

(define (fileno_unlocked stream)
  (receive (result errno)
      (primitive-fileno_unlocked stream)
    (when (pointer-null? result)
      (raise-errno-error 'fileno_unlocked errno stream))
    result))



;;;; done

)

;;; end of file
