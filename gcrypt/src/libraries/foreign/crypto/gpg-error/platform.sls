;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Gcrypt
;;;Contents: bindings to foreign functions for libgpg-error
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


(library (foreign crypto gpg-error platform)
  (export
    gpg_err_init
    gpg_strerror
    gpg_strerror_r
    gpg_strsource
    gpg_err_code_from_errno
    gpg_err_code_to_errno
    gpg_err_code_from_syserror
    gpg_err_make
    gpg_error
    gpg_err_code
    gpg_err_source
    gpg_err_make_from_errno
    gpg_error_from_errno
    gpg_error_from_syserror)
  (import (rnrs)
    (foreign ffi)
    (foreign ffi sizeof)
    (foreign crypto gpg-error shared-object)
    (foreign crypto gpg-error sizeof))


(define-c-functions gpg-error-shared-object
  (gpg_err_init			(gpg_error_t gpg_err_init (void)))
  (gpg_strerror			(char* gpg_strerror (gpg_error_t)))
  (gpg_strerror_r		(int gpg_strerror_r (gpg_error_t char* size_t)))
  (gpg_strsource		(char* gpg_strsource (gpg_error_t)))
  (gpg_err_code_from_errno	(gpg_err_code_t gpg_err_code_from_errno (int)))
  (gpg_err_code_to_errno	(int gpg_err_code_to_errno (gpg_err_code_t)))
  (gpg_err_code_from_syserror	(gpg_err_code_t gpg_err_code_from_syserror (void))))

;; gpg_error_t
;; gpg_err_make (gpg_err_source_t source, gpg_err_code_t code)
;; {
;;   return code == GPG_ERR_NO_ERROR ? GPG_ERR_NO_ERROR
;;     : (((source & GPG_ERR_SOURCE_MASK) << GPG_ERR_SOURCE_SHIFT)
;;        | (code & GPG_ERR_CODE_MASK));
;; }
(define (gpg_err_make source code)
  (if (= code GPG_ERR_NO_ERROR)
      GPG_ERR_NO_ERROR
    (bitwise-ior (bitwise-arithmetic-shift-left (bitwise-and source GPG_ERR_SOURCE_MASK)
						GPG_ERR_SOURCE_SHIFT)
		 (bitwise-and code GPG_ERR_CODE_MASK))))

;; gpg_error_t
;; gpg_error (gpg_err_code_t code)
;; {
;;   return gpg_err_make (GPG_ERR_SOURCE_DEFAULT, code);
;; }
(define (gpg_error code)
  (gpg_err_make GPG_ERR_SOURCE_DEFAULT code))

;; gpg_err_code_t
;; gpg_err_code (gpg_error_t err)
;; {
;;   return (gpg_err_code_t) (err & GPG_ERR_CODE_MASK);
;; }
(define (gpg_err_code err)
  (bitwise-and err GPG_ERR_CODE_MASK))

;; gpg_err_source_t
;; gpg_err_source (gpg_error_t err)
;; {
;;   return (gpg_err_source_t) ((err >> GPG_ERR_SOURCE_SHIFT)
;; 			     & GPG_ERR_SOURCE_MASK);
;; }
(define (gpg_err_source err)
  (bitwise-and (bitwise-arithmetic-shift-right err GPG_ERR_SOURCE_SHIFT)
	       GPG_ERR_SOURCE_MASK))

;; gpg_error_t
;; gpg_err_make_from_errno (gpg_err_source_t source, int err)
;; {
;;   return gpg_err_make (source, gpg_err_code_from_errno (err));
;; }
(define (gpg_err_make_from_errno source err)
  (gpg_err_make source (gpg_err_code_from_errno err)))

;; gpg_error_t
;; gpg_error_from_errno (int err)
;; {
;;   return gpg_error (gpg_err_code_from_errno (err));
;; }
(define (gpg_error_from_errno err)
  (gpg_error (gpg_err_code_from_errno err)))

;; gpg_error_t
;; gpg_error_from_syserror (void)
;; {
;;   return gpg_error (gpg_err_code_from_syserror ());
;; }
(define (gpg_error_from_syserror)
  (gpg_error (gpg_err_code_from_syserror)))


;;;; done

)

;;; end of file
