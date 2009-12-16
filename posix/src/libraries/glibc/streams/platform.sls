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


(library (glibc streams platform)
  (export
    ferror		clearerr
    fopen		fclose		fcloseall
    fwrite		fputc		fputs		fflush
    fread		fgetc		fgets
    ungetc
    feof		rewind
    fseeko		fseek
    ftell		ftello
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
    (foreign ffi)
    (foreign ffi sizeof)
    (posix sizeof)
    (posix shared-object))


(define-c-functions libc-shared-object
  (ferror		(int ferror (FILE*)))
  (clearerr		(int clearerr (FILE*))))

(define-c-functions/with-errno libc-shared-object
  (fopen		(FILE* fopen (char* char*)))
  (fdopen		(FILE* fdopen (int char*)))
  (fileno		(int fileno (FILE*)))
  (fclose		(int fclose (FILE*)))
  (fcloseall		(int fcloseall (void)))

  (fwrite		(size_t fwrite (void* size_t size_t FILE*)))
  (fputc		(int fputc (int FILE*)))
  (fputs		(int fputs (char* FILE*)))
  (fflush		(int fflush (FILE*)))

  (fread		(size_t fwrite (void* size_t size_t FILE*)))
  (fgetc		(int fgetc (FILE*)))
  (fgets		(char* fgets (char* int FILE*)))

  (ungetc		(int ungetc (int FILE*)))

  (feof			(int feof (FILE*)))

  (ftell		(long ftell (FILE*)))
  (rewind		(void rewind (FILE*))))

(define-c-functions/with-errno libnausicaa-posix
  (fseek		(int nausicaa_posix_fseek (FILE* long int)))
  (fseeko		(int nausicaa_posix_fseeko (FILE* off_t int)))
  (ftello		(off_t nausicaa_posix_ftello (FILE*))))


(define-c-functions libc-shared-object
  (ferror_unlocked		(int ferror_unlocked (FILE*)))
  (clearerr_unlocked		(int clearerr_unlocked (FILE*))))

(define-c-functions/with-errno libc-shared-object
  (fwrite_unlocked		(size_t fwrite_unlocked (void* size_t size_t FILE*)))
  (fputc_unlocked		(int fputc_unlocked (int FILE*)))
  (fputs_unlocked		(int fputs (char* FILE*)))
  (fflush_unlocked		(int fflush (FILE*)))

  (fread_unlocked		(size_t fwrite_unlocked (void* size_t size_t FILE*)))
  (fgetc_unlocked		(int fgetc (FILE*)))
  (fgets_unlocked		(char* fgets (char* int FILE*)))

  (feof_unlocked		(int feof_unlocked (FILE*)))

  (fileno_unlocked		(int fileno_unlocked (FILE*)))

  (popen			(FILE* popen (char* char*)))
  (pclose			(int pclose (FILE*)))

  (getline			(ssize_t getline (void* void* FILE*)))
  (getdelim			(ssize_t getdelim (void* void* int FILE*))))

(define-c-functions libc-shared-object
  (freadable			(int __freadable (FILE*)))
  (fwritable			(int __fwritable (FILE*)))
  (freading			(int __freading (FILE*)))
  (fwriting			(int __fwriting (FILE*)))
  (fwide			(int fwide (FILE* int)))
  (fpurge			(void __fpurge (FILE*))))


;;;; done

)

;;; end of file
