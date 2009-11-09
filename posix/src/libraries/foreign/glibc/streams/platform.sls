;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: direct interface to stream functions
;;;Date: Fri Nov  6, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009 Marco Maggi <marcomaggi@gna.org>
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


(library (foreign glibc streams platform)
  (export
    ferror		clearerr
    fopen		fclose		fcloseall
    fwrite		fputc		fputs		fflush
    fread		fgetc		fgets
    ungetc
    feof		fseek		ftell		rewind
    fdopen		fileno

    ferror_unlocked	clearerr_unlocked
    fwrite_unlocked	fputc_unlocked	fputs_unlocked	fflush_unlocked
    fread_unlocked	fgetc_unlocked	fgets_unlocked
    feof_unlocked	fileno_unlocked

    popen		pclose

    freadable		fwritable
    freading		fwriting
    fwide		fpurge

    getline		getdelim)
  (import (rnrs)
    (foreign posix shared-object)
    (foreign ffi)
    (foreign ffi sizeof)
    (foreign posix sizeof))

  (define dummy
    (shared-object standard-c-library))


(define-c-function ferror
  (int ferror (FILE*)))

(define-c-function clearerr
  (int clearerr (FILE*)))

;;; --------------------------------------------------------------------

(define-c-function/with-errno fopen
  (FILE* fopen (char* char*)))

(define-c-function/with-errno fclose
  (int fclose (FILE*)))

(define-c-function/with-errno fcloseall
  (int fcloseall (void)))

;;; --------------------------------------------------------------------

(define-c-function/with-errno fwrite
  (size_t fwrite (void* size_t size_t FILE*)))

(define-c-function/with-errno fputc
  (int fputc (int FILE*)))

(define-c-function/with-errno fputs
  (int fputs (char* FILE*)))

(define-c-function/with-errno fflush
  (int fflush (FILE*)))

;;; --------------------------------------------------------------------

(define-c-function/with-errno fread
  (size_t fwrite (void* size_t size_t FILE*)))

(define-c-function/with-errno fgetc
  (int fgetc (FILE*)))

(define-c-function/with-errno fgets
  (char* fgets (char* int FILE*)))

;;; --------------------------------------------------------------------

(define-c-function/with-errno ungetc
  (int ungetc (int FILE*)))

;;; --------------------------------------------------------------------

(define-c-function/with-errno feof
  (int feof (FILE*)))

(define-c-function/with-errno fseek
  (int fseek (FILE* long int)))

(define-c-function/with-errno ftell
  (long ftell (FILE*)))

(define-c-function/with-errno rewind
  (void rewind (FILE*)))

;;; --------------------------------------------------------------------

(define-c-function/with-errno fdopen
  (FILE* fdopen (int char*)))

(define-c-function/with-errno fileno
  (int fileno (FILE*)))


(define-c-function ferror_unlocked
  (int ferror_unlocked (FILE*)))

(define-c-function clearerr_unlocked
  (int clearerr_unlocked (FILE*)))

;;; --------------------------------------------------------------------

(define-c-function/with-errno fwrite_unlocked
  (size_t fwrite_unlocked (void* size_t size_t FILE*)))

(define-c-function/with-errno fputc_unlocked
  (int fputc_unlocked (int FILE*)))

(define-c-function/with-errno fputs_unlocked
  (int fputs (char* FILE*)))

(define-c-function/with-errno fflush_unlocked
  (int fflush (FILE*)))

;;; --------------------------------------------------------------------

(define-c-function/with-errno fread_unlocked
  (size_t fwrite_unlocked (void* size_t size_t FILE*)))

(define-c-function/with-errno fgetc_unlocked
  (int fgetc (FILE*)))

(define-c-function/with-errno fgets_unlocked
  (char* fgets (char* int FILE*)))

;;; --------------------------------------------------------------------

(define-c-function/with-errno feof_unlocked
  (int feof_unlocked (FILE*)))

;;; --------------------------------------------------------------------

(define-c-function/with-errno fileno_unlocked
  (int fileno_unlocked (FILE*)))

;;; --------------------------------------------------------------------

(define-c-function/with-errno popen
  (FILE* popen (char* char*)))

(define-c-function/with-errno pclose
  (int pclose (FILE*)))

;;; --------------------------------------------------------------------

(define-c-function freadable
  (int __freadable (FILE*)))

(define-c-function fwritable
  (int __fwritable (FILE*)))

(define-c-function freading
  (int __freading (FILE*)))

(define-c-function fwriting
  (int __fwriting (FILE*)))

(define-c-function fwide
  (int fwide (FILE* int)))

(define-c-function fpurge
  (void __fpurge (FILE*)))

;;; --------------------------------------------------------------------

(define-c-function/with-errno getline
  (ssize_t getline (void* void* FILE*)))

(define-c-function/with-errno getdelim
  (ssize_t getdelim (void* void* int FILE*)))


;;;; done

)

;;; end of file
