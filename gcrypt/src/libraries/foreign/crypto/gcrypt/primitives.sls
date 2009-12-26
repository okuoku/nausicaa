;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Gcrypt
;;;Contents: primitive functions
;;;Date: Sat Dec 26, 2009
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


(library (foreign crypto gcrypt primitives)
  (export)
  (import (rnrs)
    (compensations)
    (foreign ffi)
    (foreign memory)
    (foreign cstrings)
    (foreign crypto gcrypt platform)
    (foreign crypto gcrypt sizeof))


;;;; callback makers

(define (make-gcry-prime-check-func-t-callback scheme-function)
  (make-c-callback* int
		    scheme-function
		    (void* int gcry_mpi_t)))

(define (make-gcry_handler_progress_t-callback scheme-function)
  (make-c-callback* void
		    scheme-function
		    (void* char* int int int)))

(define (make-gcry_handler_alloc_t-callback scheme-function)
  (make-c-callback* void*
		    scheme-function
		    (size_t)))

(define (make-gcry_handler_secure_check_t-callback scheme-function)
  (make-c-callback* int
		    scheme-function
		    (void*)))

(define (make-gcry_handler_realloc_t-callback scheme-function)
  (make-c-callback* void*
		    scheme-function
		    (void* size_t)))

(define (make-gcry_handler_free_t-callback scheme-function)
  (make-c-callback* void
		    scheme-function
		    (void*)))

(define (make-gcry_handler_no_mem_t-callback scheme-function)
  (make-c-callback* int
		    scheme-function
		    (void* size_t unsigned-int)))

(define (make-gcry_handler_error_t-callback scheme-function)
  (make-c-callback* void
		    scheme-function
		    (void* int char*)))

;;Variadic!!!
;;
;; (define (make-gcry_handler_log_t-callback scheme-function)
;;   (make-c-callback* void
;; 		    gcry_handler_log_t
;; 		    (void* int char* va_list)))



;;;; done

)

;;; end of file
