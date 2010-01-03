;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Gcrypt
;;;Contents: compensated constructors
;;;Date: Sat Dec 26, 2009
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


(library (foreign crypto gcrypt compensated)
  (export
    gcry-cipher-open/c		gcry-md-open/c		gcry-md-copy/c
    gcry-mpi-new/c		gcry-mpi-snew/c		gcry-mpi-copy/c
    string->gcry-sexp/c		list->gcry-sexp/c)
  (import (rnrs)
    (compensations)
    (foreign crypto gcrypt))


(define (gcry-cipher-open/c . args)
  (letrec ((hd (compensate
		   (apply gcry-cipher-open args)
		 (with
		  (gcry-cipher-close hd)))))
    hd))

;;; --------------------------------------------------------------------

(define (gcry-md-open/c . args)
  (letrec ((hd (compensate
		   (apply gcry-md-open args)
		 (with
		  (gcry-md-close hd)))))
    hd))

(define (gcry-md-copy/c . args)
  (letrec ((hd (compensate
		   (apply gcry-md-copy args)
		 (with
		  (gcry-md-close hd)))))
    hd))

;;; --------------------------------------------------------------------

(define (gcry-mpi-new/c . args)
  (letrec ((n (compensate
		  (apply gcry-mpi-new args)
		(with
		 (gcry-mpi-release n)))))
    n))

(define (gcry-mpi-snew/c . args)
  (letrec ((n (compensate
		  (apply gcry-mpi-snew args)
		(with
		 (gcry-mpi-release n)))))
    n))

(define (gcry-mpi-copy/c . args)
  (letrec ((n (compensate
		  (apply gcry-mpi-copy args)
		(with
		 (gcry-mpi-release n)))))
    n))

;;; --------------------------------------------------------------------

(define (string->gcry-sexp/c . args)
  (letrec ((sexp (compensate
		     (apply string->gcry-sexp args)
		   (with
		    (gcry-sexp-release sexp)))))
    sexp))

(define (list->gcry-sexp/c . args)
  (letrec ((sexp (compensate
		     (apply list->gcry-sexp args)
		   (with
		    (gcry-sexp-release sexp)))))
    sexp))


;;;; done

)

;;; end of file
