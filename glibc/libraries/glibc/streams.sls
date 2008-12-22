;;;
;;;Part of: Glibc libraries for R6RS Scheme
;;;Contents: stream functions
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

(library (glibc streams)
  (export
    ferror primitive-ferror primitive-ferror-function platform-ferror
    clearerr

    fopen primitive-fopen primitive-fopen-function platform-fopen
    fclose primitive-fclose primitive-fclose-function platform-fclose
    fcloseall primitive-fcloseall primitive-fcloseall-function platform-fcloseall

    fwrite primitive-fwrite primitive-fwrite-function platform-fwrite
    fputc primitive-fputc primitive-fputc-function platform-fputc
    fputs primitive-fputs primitive-fputs-function platform-fputs
    fflush primitive-fflush primitive-fflush-function platform-fflush

    fread primitive-fread primitive-fread-function platform-fread
    fgetc primitive-fgetc primitive-fgetc-function platform-fgetc
    fgets primitive-fgets primitive-fgets-function platform-fgets

    feof primitive-feof primitive-feof-function platform-feof
    fseek primitive-fseek primitive-fseek-function platform-fseek
    ftell primitive-ftell primitive-ftell-function platform-ftell
    rewind primitive-rewind primitive-rewind-function platform-rewind

    fdopen primitive-fdopen primitive-fdopen-function platform-fdopen
    fileno primitive-fileno primitive-fileno-function platform-fileno

    valueof-eof		valueof-seek-set
    valueof-seek-cur	valueof-seek-end)
  (import (r6rs)
    (uriel lang)
    (uriel foreign)
    (glibc sizeof))

  (define libc
    (begin
      (shared-object self-shared-object)
      self-shared-object))


;;;; errors

(define-c-function platform-ferror
  (int ferror (FILE*)))

(define-c-function clearerr
  (int clearerr (FILE*)))

(define (primitive-ferror stream)
  (if (= 0 (platform-ferror stream)) #f #t))

(define primitive-ferror-function
  (make-parameter primitive-ferror
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'primitive-ferror-function
	  "expected procedure as value for the PRIMITIVE-FERROR-FUNCTION parameter"
	  func))
      func)))

(define (ferror stream)
  ((primitive-ferror-function) stream))



;;;; opening and closing

(define-c-function/with-errno platform-fopen
  (FILE* fopen (char* char*)))

(define-c-function/with-errno platform-fclose
  (int fclose (FILE*)))

(define-c-function/with-errno platform-fcloseall
  (int fcloseall (void)))

;;; --------------------------------------------------------------------

(define (primitive-fopen pathname mode)
  (receive (result errno)
      (with-compensations
	(let ((pathname	(string->cstring/c pathname))
	      (mode	(string->cstring/c mode)))
	  (platform-fopen pathname mode)))
    (when (pointer-null? result)
      (raise-errno-error 'primitive-fopen errno (list pathname mode)))
    result))

(define (primitive-fclose stream)
  (receive (result errno)
      (platform-fclose stream)
    (unless (= 0 result)
      (raise-errno-error 'primitive-fclose errno stream))
    result))

(define (primitive-fcloseall)
  (receive (result errno)
      (platform-fcloseall)
    (unless (= 0 result)
      (raise-errno-error 'primitive-fcloseall errno #f))
    result))

;;; --------------------------------------------------------------------

(define primitive-fopen-function
  (make-parameter primitive-fopen
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'primitive-fopen-function
	  "expected procedure as value for the PRIMITIVE-FOPEN-FUNCTION parameter"
	  func))
      func)))

(define primitive-fclose-function
  (make-parameter primitive-fclose
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'primitive-fclose-function
	  "expected procedure as value for the PRIMITIVE-FCLOSE-FUNCTION parameter"
	  func))
      func)))

(define primitive-fcloseall-function
  (make-parameter primitive-fcloseall
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'primitive-fcloseall-function
	  "expected procedure as value for the PRIMITIVE-FCLOSEALL-FUNCTION parameter"
	  func))
      func)))

;;; --------------------------------------------------------------------

(define (fopen pathname mode)
  ((primitive-fopen-function) pathname mode))

(define (fclose stream)
  ((primitive-fclose-function) stream))

(define (fcloseall)
  ((primitive-fcloseall-function)))



;;;; writing

(define-c-function/with-errno platform-fwrite
  (size_t fwrite (void* size_t size_t FILE*)))

(define-c-function/with-errno platform-fputc
  (int fputc (int FILE*)))

(define-c-function/with-errno platform-fputs
  (int fputs (char* FILE*)))

(define-c-function/with-errno platform-fflush
  (int fflush (FILE*)))

;;; --------------------------------------------------------------------

(define (primitive-fwrite pointer size count stream)
  (receive (result errno)
      (platform-fwrite pointer size count stream)
    (unless (= count result)
      (raise-errno-error 'primitive-fwrite errno (list pointer size count stream)))
    result))

(define (primitive-fputc char stream)
  (let ((c-char (char->integer char)))
    (when (< 255 c-char)
      (assertion-violation 'primitive-fputc
	"expected character with scalar value in range [0, 255]" char))
    (receive (result errno)
	(platform-fputc char stream)
      (unless (= char result)
	(raise-errno-error 'primitive-fputc errno (list char stream)))
      result)))

(define (primitive-fputs string stream)
  (receive (result errno)
      (with-compensations
	(let ((cstring (string->cstring/c string)))
	  (platform-fwrite cstring stream)))
    (when (= valueof-eof result)
      (raise-errno-error 'primitive-fputs errno (list string stream)))
    result))

(define (primitive-fflush stream)
  (receive (result errno)
      (platform-fflush stream)
    (when (= valueof-eof result)
      (raise-errno-error 'primitive-fputc errno stream))
    result))

;;; --------------------------------------------------------------------

(define primitive-fwrite-function
  (make-parameter primitive-fwrite
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'primitive-fwrite-function
	  "expected procedure as value for the PRIMITIVE-FWRITE-FUNCTION parameter"
	  func))
      func)))

(define primitive-fputs-function
  (make-parameter primitive-fputs
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'primitive-fputs-function
	  "expected procedure as value for the PRIMITIVE-FPUTS-FUNCTION parameter"
	  func))
      func)))

(define primitive-fputc-function
  (make-parameter primitive-fputc
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'primitive-fputc-function
	  "expected procedure as value for the PRIMITIVE-FPUTC-FUNCTION parameter"
	  func))
      func)))

(define primitive-fflush-function
  (make-parameter primitive-fflush
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'primitive-fflush-function
	  "expected procedure as value for the PRIMITIVE-FFLUSH-FUNCTION parameter"
	  func))
      func)))

;;; --------------------------------------------------------------------

(define (fwrite pointer size count stream)
  ((primitive-fwrite-function) pointer size count stream))

(define (fputc char stream)
  ((primitive-fputc-function) char stream))

(define (fputs string stream)
  ((primitive-fputs-function) string stream))

(define (fflush stream)
  ((primitive-fflush-function) stream))



;;;; reading

(define-c-function/with-errno platform-fread
  (size_t fwrite (void* size_t size_t FILE*)))

(define-c-function/with-errno platform-fgetc
  (int fgetc (FILE*)))

(define-c-function/with-errno platform-fgets
  (char* fgets (char* int FILE*)))

;;; --------------------------------------------------------------------

(define (primitive-fread pointer size count stream)
  (receive (result errno)
      (platform-fread pointer size count stream)
    (when (ferror stream)
      (raise-errno-error 'primitive-fread errno (list pointer size count stream)))
    result))

(define (primitive-fgetc char stream)
  (receive (result errno)
      (platform-fgetc stream)
    (when (= valueof-eof result)
      (raise-errno-error 'primitive-fgetc errno stream))
    (integer->char result)))

(define (primitive-fgets pointer count stream)
  (receive (result errno)
      (platform-fread pointer count stream)
    (when (ferror stream)
      (raise-errno-error 'primitive-fgets errno (list pointer count stream)))
    result))

;;; --------------------------------------------------------------------

(define primitive-fread-function
  (make-parameter primitive-fread
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'primitive-fread-function
	  "expected procedure as value for the PRIMITIVE-FREAD-FUNCTION parameter"
	  func))
      func)))

(define primitive-fgetc-function
  (make-parameter primitive-fgetc
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'primitive-fgetc-function
	  "expected procedure as value for the PRIMITIVE-FGETC-FUNCTION parameter"
	  func))
      func)))

(define primitive-fgets-function
  (make-parameter primitive-fgets
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'primitive-fgets-function
	  "expected procedure as value for the PRIMITIVE-FGETS-FUNCTION parameter"
	  func))
      func)))

;;; --------------------------------------------------------------------

(define (fread pointer size count stream)
  ((primitive-fread-function) pointer size count stream))

(define (fgetc char stream)
  ((primitive-fgetc-function) char stream))

(define (fgets pointer count stream)
  ((primitive-fgets-function) pointer count stream))



;;;; unreading

(define-c-function/with-errno platform-ungetc
  (int ungetc (int FILE*)))

(define (primitive-ungetc char stream)
  (receive (result errno)
      (let ((c-char (char->integer char)))
	(when (< 255 c-char)
	  (assertion-violation 'primitive-ungetc
	    "expected character with scalar value in range [0, 255]" char))
	(platform-ungetc char stream))
    (when (ferror stream)
      (raise-errno-error 'primitive-ungetc errno (list char stream)))
    result))

(define primitive-ungetc-function
  (make-parameter primitive-ungetc
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'primitive-ungetc-function
	  "expected procedure as value for the PRIMITIVE-UNGETC-FUNCTION parameter"
	  func))
      func)))

(define (ungetc char stream)
  ((primitive-ungetc-function) char stream))



;;;; seeking

(define-c-function/with-errno platform-feof
  (int feof (FILE*)))

(define-c-function/with-errno platform-fseek
  (int fseek (FILE* long int)))

(define-c-function/with-errno platform-ftell
  (long ftell (FILE*)))

(define-c-function/with-errno platform-rewind
  (void rewind (FILE*)))

;;; --------------------------------------------------------------------

(define (primitive-feof stream)
  (receive (result errno)
      (platform-feof stream)
    (when (ferror stream)
      (raise-errno-error 'primitive-feof errno stream))
    (not (= 0 result))))

(define (primitive-fseek stream offset whence)
  (receive (result errno)
      (platform-fseek stream offset whence)
    (when (ferror stream)
      (raise-errno-error 'primitive-fseek errno stream))
    result))

(define (primitive-ftell stream)
  (receive (result errno)
      (platform-ftell stream)
    (when (ferror stream)
      (raise-errno-error 'primitive-ftell errno stream))
    result))

(define (primitive-rewind stream)
  (receive (result errno)
      (platform-rewind stream)
    (when (ferror stream)
      (raise-errno-error 'primitive-rewind errno stream))
    result))

;;; --------------------------------------------------------------------

(define primitive-feof-function
  (make-parameter primitive-feof
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'primitive-feof-function
	  "expected procedure as value for the PRIMITIVE-FEOF-FUNCTION parameter"
	  func))
      func)))

(define primitive-fseek-function
  (make-parameter primitive-fseek
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'primitive-fseek-function
	  "expected procedure as value for the PRIMITIVE-FSEEK-FUNCTION parameter"
	  func))
      func)))

(define primitive-ftell-function
  (make-parameter primitive-ftell
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'primitive-ftell-function
	  "expected procedure as value for the PRIMITIVE-FTELL-FUNCTION parameter"
	  func))
      func)))

(define primitive-rewind-function
  (make-parameter primitive-rewind
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'primitive-rewind-function
	  "expected procedure as value for the PRIMITIVE-REWIND-FUNCTION parameter"
	  func))
      func)))

;;; --------------------------------------------------------------------

(define (feof stream)
  ((primitive-feof-function) stream))

(define (fseek stream offset whence)
  ((primitive-fseek-function) stream offset whence))


(define (ftell stream)
  ((primitive-ftell-function) stream))

(define (rewind stream)
  ((primitive-rewind) stream))



;;;; streams and file descriptors

(define-c-function/with-errno platform-fdopen
  (FILE* fdopen (int char*)))

(define-c-function/with-errno platform-fileno
  (int fileno (FILE*)))

;;; --------------------------------------------------------------------

(define (primitive-fdopen fd open-mode)
  (receive (result errno)
      (with-compensations
	(let ((mode	(string->cstring/c open-mode)))
	  (platform-fdopen fd mode)))
    (when (pointer-null? result)
      (raise-errno-error 'primitive-fdopen errno (list fd open-mode)))
    result))

(define (primitive-fileno stream)
  (receive (result errno)
      (platform-fileno stream)
    (when (pointer-null? result)
      (raise-errno-error 'primitive-fileno errno stream))
    result))

;;; --------------------------------------------------------------------

(define primitive-fdopen-function
  (make-parameter primitive-fdopen
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'primitive-fdopen-function
	  "expected procedure as value for the PRIMITIVE-FDOPEN-FUNCTION parameter"
	  func))
      func)))

(define primitive-fileno-function
  (make-parameter primitive-fileno
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'primitive-fileno-function
	  "expected procedure as value for the PRIMITIVE-FILENO-FUNCTION parameter"
	  func))
      func)))

;;; --------------------------------------------------------------------

(define (fdopen fd open-mode)
  ((primitive-fdopen-function) fd open-mode))

(define (fileno stream)
  ((primitive-fileno) stream))



;;;; done

)

;;; end of file
