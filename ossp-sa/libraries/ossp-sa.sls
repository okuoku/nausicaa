;;;
;;;Part of: Nausicaa/OSSP/sa
;;;Contents: high level interface to OSSP/sa for R6RS Scheme
;;;Date: Sat Dec 13, 2008
;;;Time-stamp: <2008-12-15 11:33:55 marco>
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

(library (ossp-sa)
  (export

    ;; errors
    sa-return-value->symbol raise-sa-error

    ;; address
    make-sa-address make-sa-address/compensated destroy-sa-address
    sa-address-set! sa-address-ref

    )
  (import (r6rs)
    (uriel lang)
    (uriel ffi)
    (uriel ffi sizeof)
    (ossp-sa foreign)
    (ossp-sa sizeof))


;;; --------------------------------------------------------------------
;;; Address: creation and destruction.
;;; --------------------------------------------------------------------

(define make-sa-address
  (case-lambda
   (()
    (with-compensations
      (let* ((address*	(compensate-malloc/small))
	     (e		(sa_addr_create address*)))
	(unless (= SA_OK e)
	  (raise-sa-error 'make-sa-address e))
	(pointer-ref-c-pointer address* 0))))
   ((uri)
    (with-compensations
      (let ((address	(make-sa-address)))
	(guard (exc (else (raise exc)))
	  (sa-address-set! address uri))
	address)))))

(define (destroy-sa-address address)
  (let ((e (sa_addr_destroy address)))
    (unless (= SA_OK e)
      (raise-sa-error 'sa_addr_create e))))

(define make-sa-address/compensated
  (case-lambda
   (()
    (letrec ((address (compensate (make-sa-address)
			(with (destroy-sa-address address)))))
      address))
   ((uri)
    (letrec ((address (compensate (make-sa-address uri)
			(with (destroy-sa-address address)))))
      address))))



;;; --------------------------------------------------------------------
;;; Address: operations.
;;; --------------------------------------------------------------------

(define (sa-address-set! address uri)
  (with-compensations
    (let* ((spec	(string-or-symbol->cstring/compensated uri))
	   (e		(sa_addr_u2a address spec)))
      (unless (= SA_OK e)
	(destroy-sa-address address)
	(raise-sa-error 'sa-address-set! e)))))

(define (sa-address-ref address)
  (with-compensations
    (let* ((spec*	(compensate-malloc/small))
	   (e		(compensate
			    (sa_addr_a2u address spec*)
			  (with
			   (primitive-free (pointer-ref-c-pointer spec* 0))))))
      (cstring->string (pointer-ref-c-pointer spec* 0)))))



;;; --------------------------------------------------------------------
;;; Conditions.
;;; --------------------------------------------------------------------

(define-condition-type &sa-error &error
  make-sa-condition sa-condition?
  (code sa-error-code))

(define (raise-sa-error who code)
  (if (= SA_ERR_MEM code)
      (raise-out-of-memory who #f)
    (raise (condition (make-who-condition who)
		      (make-message-condition (cstring->string (sa_error code)))
		      (make-sa-condition (sa-return-value->symbol code))))))

(define sa-return-values-alist
  `((,SA_OK . SA_OK)
    (,SA_ERR_ARG . SA_ERR_ARG)
    (,SA_ERR_USE . SA_ERR_USE)
    (,SA_ERR_MEM . SA_ERR_MEM)
    (,SA_ERR_MTC . SA_ERR_MTC)
    (,SA_ERR_EOF . SA_ERR_EOF)
    (,SA_ERR_TMT . SA_ERR_TMT)
    (,SA_ERR_SYS . SA_ERR_SYS)
    (,SA_ERR_IMP . SA_ERR_IMP)
    (,SA_ERR_INT . SA_ERR_INT)))

(define (sa-return-value->symbol retval)
  (let ((pair (assq retval sa-return-values-alist)))
    (if pair
	(cdr pair)
      (assertion-violation 'sa-return-value->symbol
	"unknown SA return value" retval))))


;;; --------------------------------------------------------------------
;;; Done.
;;; --------------------------------------------------------------------

)

;;; end of file
