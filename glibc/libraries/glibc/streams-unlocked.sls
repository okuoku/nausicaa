;;;
;;;Part of: Glibc libraries for R6RS Scheme
;;;Contents: interface to unlocked stream functions
;;;Date: Thu Dec  4, 2008
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


;;; --------------------------------------------------------------------
;;; Setup.
;;; --------------------------------------------------------------------

(library (glibc streams-unlocked)
  (export
    ferror_unlocked primitive-ferror_unlocked-function primitive-ferror_unlocked platform-ferror_unlocked
    clearerr_unlocked

    fwrite_unlocked primitive-fwrite_unlocked-function primitive-fwrite_unlocked platform-fwrite_unlocked
    fputc_unlocked primitive-fputc_unlocked-function primitive-fputc_unlocked platform-fputc_unlocked
    fputs_unlocked primitive-fputs_unlocked-function primitive-fputs_unlocked platform-fputs_unlocked
    fflush_unlocked primitive-fflush_unlocked-function primitive-fflush_unlocked platform-fflush_unlocked

    fread_unlocked primitive-fread_unlocked-function primitive-fread_unlocked platform-fread_unlocked
    fgetc_unlocked primitive-fgetc_unlocked-function primitive-fgetc_unlocked platform-fgetc_unlocked
    fgets_unlocked primitive-fgets_unlocked-function primitive-fgets_unlocked platform-fgets_unlocked

    feof_unlocked primitive-feof_unlocked-function primitive-feof_unlocked platform-feof_unlocked

    fileno_unlocked primitive-fileno_unlocked-function primitive-fileno_unlocked platform-fileno_unlocked)
  (import (r6rs)
    (uriel lang)
    (uriel foreign)
    (glibc sizeof))

(define libc
  (begin
    (shared-object self-shared-object)
    self-shared-object))



;;;; errors

(define-c-function platform-ferror_unlocked
  (int ferror_unlocked (FILE*)))

(define-c-function clearerr_unlocked
  (int clearerr_unlocked (FILE*)))

(define (primitive-ferror_unlocked stream)
  (if (= 0 (platform-ferror_unlocked stream)) #f #t))

(define primitive-ferror_unlocked-function
  (make-parameter primitive-ferror_unlocked
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'platform-ferror_unlocked-function
	  "expected procedure as value for the PLATFORM-FERROR_UNLOCKED-FUNCTION parameter"
	  func))
      func)))

(define (ferror_unlocked stream)
  ((primitive-ferror_unlocked-function) stream))



;;;; writing

(define-c-function/with-errno platform-fwrite_unlocked
  (size_t fwrite_unlocked (void* size_t size_t FILE*)))

(define-c-function/with-errno platform-fputc_unlocked
  (int fputc_unlocked (int FILE*)))

(define-c-function/with-errno platform-fputs_unlocked
  (int fputs (char* FILE*)))

(define-c-function/with-errno platform-fflush_unlocked
  (int fflush (FILE*)))

;;; --------------------------------------------------------------------

(define (primitive-fwrite_unlocked pointer size count stream)
  (receive (result errno)
      (platform-fwrite_unlocked pointer size count stream)
    (unless (= count result)
      (raise-errno-error 'primitive-fwrite_unlocked errno (list pointer size count stream)))
    result))

(define (primitive-fputc_unlocked char stream)
  (let ((c-char (char->integer char)))
    (when (< 255 c-char)
      (assertion-violation 'primitive-fputc_unlocked
	"expected character with scalar value in range [0, 255]" char))
    (receive (result errno)
	(platform-fputc_unlocked char stream)
      (unless (= c-char result)
	(raise-errno-error 'primitive-fputc_unlocked errno (list char stream)))
      result)))

(define (primitive-fputs_unlocked string stream)
  (receive (result errno)
      (with-compensations
	(let ((cstring (string->cstring/c string)))
	  (platform-fwrite_unlocked cstring stream)))
    (when (= EOF result)
      (raise-errno-error 'primitive-fputs_unlocked errno (list string stream)))
    result))

(define (primitive-fflush_unlocked stream)
  (receive (result errno)
      (platform-fflush_unlocked stream)
    (when (= EOF result)
      (raise-errno-error 'primitive-fputc_unlocked errno stream))
    result))

;;; --------------------------------------------------------------------

(define primitive-fwrite_unlocked-function
  (make-parameter primitive-fwrite_unlocked
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'platform-fwrite_unlocked-function
	  "expected procedure as value for the PLATFORM-FWRITE_UNLOCKED-FUNCTION parameter"
	  func))
      func)))

(define primitive-fputc_unlocked-function
  (make-parameter primitive-fputc_unlocked
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'platform-fputc_unlocked-function
	  "expected procedure as value for the PLATFORM-FPUTC_UNLOCKED-FUNCTION parameter"
	  func))
      func)))

(define primitive-fputs_unlocked-function
  (make-parameter primitive-fputs_unlocked
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'platform-fputs_unlocked-function
	  "expected procedure as value for the PLATFORM-FPUTS_UNLOCKED-FUNCTION parameter"
	  func))
      func)))

(define primitive-fflush_unlocked-function
  (make-parameter primitive-fflush_unlocked
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'platform-fflush_unlocked-function
	  "expected procedure as value for the PLATFORM-FFLUSH_UNLOCKED-FUNCTION parameter"
	  func))
      func)))

;;; --------------------------------------------------------------------

(define (fwrite_unlocked pointer size count stream)
  ((primitive-fwrite_unlocked-function) pointer size count stream))

(define (fputc_unlocked char stream)
  ((primitive-fputc_unlocked) char stream))

(define (fputs_unlocked string stream)
  ((primitive-fputs_unlocked-function) string stream))

(define (fflush_unlocked stream)
  ((primitive-fflush_unlocked) stream))



;;;; reading

(define-c-function/with-errno platform-fread_unlocked
  (size_t fwrite_unlocked (void* size_t size_t FILE*)))

(define-c-function/with-errno platform-fgetc_unlocked
  (int fgetc (FILE*)))

(define-c-function/with-errno platform-fgets_unlocked
  (char* fgets (char* int FILE*)))

;;; --------------------------------------------------------------------

(define (primitive-fread_unlocked pointer size count stream)
  (receive (result errno)
      (platform-fread_unlocked pointer size count stream)
    (when (ferror_unlocked stream)
      (raise-errno-error 'primitive-fread_unlocked errno (list pointer size count stream)))
    result))

(define (primitive-fgetc_unlocked char stream)
  (receive (result errno)
      (platform-fgetc_unlocked stream)
    (when (= EOF result)
      (raise-errno-error 'primitive-fgetc_unlocked errno stream))
    (integer->char result)))

(define (primitive-fgets_unlocked pointer count stream)
  (receive (result errno)
      (platform-fread_unlocked pointer count stream)
    (when (ferror_unlocked stream)
      (raise-errno-error 'primitive-fgets_unlocked errno (list pointer count stream)))
    result))

;;; --------------------------------------------------------------------

(define primitive-fread_unlocked-function
  (make-parameter primitive-fread_unlocked
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'platform-fread_unlocked-function
	  "expected procedure as value for the PLATFORM-FREAD_UNLOCKED-FUNCTION parameter"
	  func))
      func)))

(define primitive-fgetc_unlocked-function
  (make-parameter primitive-fgetc_unlocked
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'platform-fgetc_unlocked-function
	  "expected procedure as value for the PLATFORM-FGETC_UNLOCKED-FUNCTION parameter"
	  func))
      func)))

(define primitive-fgets_unlocked-function
  (make-parameter primitive-fgets_unlocked
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'platform-fgets_unlocked-function
	  "expected procedure as value for the PLATFORM-FGETS_UNLOCKED-FUNCTION parameter"
	  func))
      func)))

;;; --------------------------------------------------------------------

(define (fread_unlocked pointer size count stream)
  ((primitive-fread_unlocked-function) pointer size count stream))

(define (fgetc_unlocked char stream)
  ((primitive-fgetc_unlocked-function) char stream))

(define (fgets_unlocked pointer count stream)
  ((primitive-fgets_unlocked-function) pointer count stream))



;;;; end of file

(define-c-function/with-errno platform-feof_unlocked
  (int feof_unlocked (FILE*)))

(define (primitive-feof_unlocked stream)
  (receive (result errno)
      (platform-feof_unlocked stream)
    (when (ferror_unlocked stream)
      (raise-errno-error 'primitive-feof_unlocked errno stream))
    result))

(define primitive-feof_unlocked-function
  (make-parameter primitive-feof_unlocked
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'platform-feof_unlocked-function
	  "expected procedure as value for the PLATFORM-FEOF_UNLOCKED-FUNCTION parameter"
	  func))
      func)))

(define (feof_unlocked stream)
  ((primitive-feof_unlocked) stream))



;;;; streams and file descriptors

(define-c-function/with-errno platform-fileno_unlocked
  (int fileno_unlocked (FILE*)))

(define (primitive-fileno_unlocked stream)
  (receive (result errno)
      (platform-fileno_unlocked stream)
    (when (pointer-null? result)
      (raise-errno-error 'primitivefileno_unlocked errno stream))
    result))

(define primitive-fileno_unlocked-function
  (make-parameter primitive-fileno_unlocked
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'primitive-fileno_unlocked-function
	  "expected procedure as value for the PRIMITIVE-FILENO_UNLOCKED-FUNCTION parameter"
	  func))
      func)))

(define (fileno_unlocked stream)
  ((primitive-fileno_unlocked-function) stream))



;;;; done

)

;;; end of file
