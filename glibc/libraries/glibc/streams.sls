;;;
;;;Part of: Glibc libraries for R6RS Scheme
;;;Contents: stream functions
;;;Date: Thu Dec  4, 2008
;;;Time-stamp: <2008-12-16 10:00:58 marco>
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

(library (glibc streams)
  (export
    ferror		primitive-ferror
    clearerr

    fopen		primitive-fopen
    fclose		primitive-fclose
    fcloseall		primitive-fcloseall

    fwrite		primitive-fwrite
    fputc		primitive-fputc
    fputs		primitive-fputs
    fflush		primitive-fflush

    fread		primitive-fread
    fgetc		primitive-fgetc
    fgets		primitive-fgets

    feof		primitive-feof
    fseek		primitive-fseek
    ftell		primitive-ftell
    rewind		primitive-rewind

    fdopen		primitive-fdopen
    fileno		primitive-fileno

    valueof-eof		valueof-seek-set
    valueof-seek-cur	valueof-seek-end)
  (import (r6rs)
    (srfi receive)
    (rename (uriel ffi)
	    (string-or-symbol->cstring/compensated s->c))
    (uriel lang)
    (glibc sizeof))

(define libc
  (begin
    (shared-object self-shared-object)
    self-shared-object))


;;;; errors

(define-c-function primitive-ferror
  (int ferror (FILE*)))

(define-c-function clearerr
  (int clearerr (FILE*)))

(define (ferror stream)
  (if (= 0 (primitive-ferror stream)) #f #t))



;;;; opening and closing

(define-c-function/with-errno primitive-fopen
  (FILE* fopen (char* char*)))

(define-c-function/with-errno primitive-fclose
  (int fclose (FILE*)))

(define-c-function/with-errno primitive-fcloseall
  (int fcloseall (void)))

(define (fopen pathname mode)
  (receive (result errno)
      (with-compensations
	(let ((pathname	(s->c pathname))
	      (mode	(s->c mode)))
	  (primitive-fopen pathname mode)))
    (when (pointer-null? result)
      (raise-errno-error 'fopen errno (list pathname mode)))
    result))

(define (fclose stream)
  (receive (result errno)
      (primitive-fclose stream)
    (unless (= 0 result)
      (raise-errno-error 'fclose errno stream))
    result))

(define (fcloseall)
  (receive (result errno)
      (primitive-fcloseall)
    (unless (= 0 result)
      (raise-errno-error 'fcloseall errno #f))
    result))



;;;; writing

(define-c-function/with-errno primitive-fwrite
  (size_t fwrite (void* size_t size_t FILE*)))

(define-c-function/with-errno primitive-fputc
  (int fputc (int FILE*)))

(define-c-function/with-errno primitive-fputs
  (int fputs (char* FILE*)))

(define-c-function/with-errno primitive-fflush
  (int fflush (FILE*)))

(define (fwrite pointer size count stream)
  (receive (result errno)
      (primitive-fwrite pointer size count stream)
    (unless (= count result)
      (raise-errno-error 'fwrite errno (list pointer size count stream)))
    result))

(define (fputc char stream)
  (let ((c-char (char->integer char)))
    (when (< 255 c-char)
      (assertion-violation 'fputc
	"expected character with scalar value in range [0, 255]" char))
    (receive (result errno)
	(primitive-fputc char stream)
      (unless (= char result)
	(raise-errno-error 'fputc errno (list char stream)))
      result)))

(define (fputs string stream)
  (receive (result errno)
      (with-compensations
	(let ((cstring (s->c string)))
	  (primitive-fwrite cstring stream)))
    (when (= valueof-eof result)
      (raise-errno-error 'fputs errno (list string stream)))
    result))

(define (fflush stream)
  (receive (result errno)
      (primitive-fflush stream)
    (when (= valueof-eof result)
      (raise-errno-error 'fputc errno stream))
    result))



;;;; reading

(define-c-function/with-errno primitive-fread
  (size_t fwrite (void* size_t size_t FILE*)))

(define-c-function/with-errno primitive-fgetc
  (int fgetc (FILE*)))

(define-c-function/with-errno primitive-fgets
  (char* fgets (char* int FILE*)))

(define (fread pointer size count stream)
  (receive (result errno)
      (primitive-fread pointer size count stream)
    (when (ferror stream)
      (raise-errno-error 'fread errno (list pointer size count stream)))
    result))

(define (fgetc char stream)
  (receive (result errno)
      (primitive-fgetc stream)
    (when (= valueof-eof result)
      (raise-errno-error 'fgetc errno stream))
    (integer->char result)))

(define (fgets pointer count stream)
  (receive (result errno)
      (primitive-fread pointer count stream)
    (when (ferror stream)
      (raise-errno-error 'fgets errno (list pointer count stream)))
    result))



;;;; unreading

(define-c-function/with-errno primitive-ungetc
  (int ungetc (int FILE*)))

(define (ungetc char stream)
  (receive (result errno)
      (let ((c-char (char->integer char)))
	(when (< 255 c-char)
	  (assertion-violation 'ungetc
	    "expected character with scalar value in range [0, 255]" char))
	(primitive-ungetc char stream))
    (when (ferror stream)
      (raise-errno-error 'ungetc errno (list char stream)))
    result))



;;;; seeking

(define-c-function/with-errno primitive-feof
  (int feof (FILE*)))

(define-c-function/with-errno primitive-fseek
  (int fseek (FILE* long int)))

(define-c-function/with-errno primitive-ftell
  (long ftell (FILE*)))

(define-c-function/with-errno primitive-rewind
  (void rewind (FILE*)))

(define (feof stream)
  (receive (result errno)
      (primitive-feof stream)
    (when (ferror stream)
      (raise-errno-error 'feof errno stream))
    (not (= 0 result))))

(define (fseek stream offset whence)
  (receive (result errno)
      (primitive-fseek stream offset whence)
    (when (ferror stream)
      (raise-errno-error 'fseek errno stream))
    result))

(define (ftell stream)
  (receive (result errno)
      (primitive-ftell stream)
    (when (ferror stream)
      (raise-errno-error 'ftell errno stream))
    result))

(define (rewind stream)
  (receive (result errno)
      (primitive-rewind stream)
    (when (ferror stream)
      (raise-errno-error 'rewind errno stream))
    result))



;;;; streams and file descriptors

(define-c-function/with-errno primitive-fdopen
  (FILE* fdopen (int char*)))

(define-c-function/with-errno primitive-fileno
  (int fileno (FILE*)))


(define (fdopen fd open-mode)
  (receive (result errno)
      (with-compensations
	(let ((mode	(s->c open-mode)))
	  (primitive-fdopen fd mode)))
    (when (pointer-null? result)
      (raise-errno-error 'fdopen errno (list fd open-mode)))
    result))

(define (fileno stream)
  (receive (result errno)
      (primitive-fileno stream)
    (when (pointer-null? result)
      (raise-errno-error 'fileno errno stream))
    result))



;;;; done

)

;;; end of file
