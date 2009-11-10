;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: high-level interface for streams
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


(library (foreign glibc streams)
  (export
    ferror		ferror-function
    clearerr		clearerr-function
    fopen		fopen-function
    fclose		fclose-function
    fcloseall		fcloseall-function
    fwrite		fwrite-function
    fputc		fputc-function
    fputs		fputs-function
    fflush		fflush-function
    fread		fread-function
    fgetc		fgetc-function
    fgets		fgets-function
    feof		feof-function
    fseek		fseek-function
    fseeko		fseeko-function
    ftell		ftell-function
    ftello		ftello-function
    rewind		rewind-function
    fdopen		fdopen-function
    fileno		fileno-function

    ferror_unlocked	ferror_unlocked-function
    clearerr_unlocked	clearerr_unlocked-function
    fwrite_unlocked	fwrite_unlocked-function
    fputc_unlocked	fputc_unlocked-function
    fputs_unlocked	fputs_unlocked-function
    fflush_unlocked	fflush_unlocked-function
    fread_unlocked	fread_unlocked-function
    fgetc_unlocked	fgetc_unlocked-function
    fgets_unlocked	fgets_unlocked-function
    feof_unlocked	feof_unlocked-function
    fileno_unlocked	fileno_unlocked-function

    popen		popen-function
    pclose		pclose-function

    freadable		freadable-function
    fwritable		fwritable-function
    freading		freading-function
    fwriting		fwriting-function
    fwide		fwide-function
    fpurge		fpurge-function

    getline		getline-function
    getdelim		getdelim-function)
  (import (rnrs)
    (foreign posix helpers)
    (prefix (foreign glibc streams primitives) primitive:))


(define-parametrised ferror stream)
(define-parametrised clearerr stream)

(define-parametrised ferror_unlocked stream)
(define-parametrised clearerr_unlocked stream)

;;; --------------------------------------------------------------------

(define-parametrised fopen pathname mode)
(define-parametrised fclose stream)
(define-parametrised fcloseall)

;;; --------------------------------------------------------------------

(define-parametrised fwrite pointer size count stream)
(define-parametrised fputc char stream)
(define-parametrised fputs string stream)
(define-parametrised fflush stream)

(define-parametrised fwrite_unlocked pointer size count stream)
(define-parametrised fputc_unlocked char stream)
(define-parametrised fputs_unlocked string stream)
(define-parametrised fflush_unlocked stream)

;;; --------------------------------------------------------------------

(define-parametrised fread pointer size count stream)
(define-parametrised fgetc char stream)
(define-parametrised fgets pointer count stream)

(define-parametrised fread_unlocked pointer size count stream)
(define-parametrised fgetc_unlocked char stream)
(define-parametrised fgets_unlocked pointer count stream)

;;; --------------------------------------------------------------------

(define-parametrised ungetc char stream)

;;; --------------------------------------------------------------------

(define-parametrised feof stream)
(define-parametrised fseek stream offset whence)
(define-parametrised fseeko stream offset whence)
(define-parametrised ftell stream)
(define-parametrised ftello stream)
(define-parametrised rewind stream)

(define-parametrised feof_unlocked stream)

;;; --------------------------------------------------------------------

(define-parametrised fdopen fd open-mode)
(define-parametrised fileno stream)

(define-parametrised fileno_unlocked stream)

;;; --------------------------------------------------------------------

(define-parametrised pclose stream)
(define-parametrised popen command mode)

;;; --------------------------------------------------------------------

(define-parametrised freadable stream)
(define-parametrised fwritable stream)
(define-parametrised freading stream)
(define-parametrised fwriting stream)
(define-parametrised fwide strea mode)
(define-parametrised fpurge stream)

;;; --------------------------------------------------------------------

(define-parametrised getline stream)
(define-parametrised getdelim stream delimiter)


;;;; done

)

;;; end of file
