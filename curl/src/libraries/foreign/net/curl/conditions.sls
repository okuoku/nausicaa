;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/cURL
;;;Contents: condition types
;;;Date: Sat Nov 21, 2009
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


(library (foreign net curl conditions)
  (export

    &curl-handle
    make-curl-handle-condition
    curl-handle-condition?
    condition-curl-handle

    &curl-error-code
    make-curl-error-code-condition
    curl-error-code-condition?
    condition-curl-error-numeric-code

;;; --------------------------------------------------------------------

    &curl-error
    make-curl-error-condition
    curl-error-condition?
    raise-curl-easy-error
    raise-curl-multi-error

    &curl-init-error
    make-curl-init-error-condition
    curl-init-error-condition?
    raise-curl-init-error

    )
  (import (rnrs)
    (foreign cstrings)
    (foreign net curl enumerations)
    (foreign net curl platform))


(define-condition-type &curl-handle &condition
  make-curl-handle-condition curl-handle-condition?
  (handle		condition-curl-handle))

;;; --------------------------------------------------------------------

(define-condition-type &curl-error-code &condition
  %make-curl-error-code-condition curl-error-code-condition?
  (numeric-code		condition-curl-error-numeric-code)
  (symbolic-code	condition-curl-error-symbolic-code))

(define (make-curl-error-code-condition numeric-code)
  (%make-curl-error-code-condition numeric-code (curl-numeric-code->symbolic-code numeric-code)))


(define-condition-type &curl-error &error
  make-curl-error-condition curl-error-condition?)

(define (raise-curl-easy-error who code handle)
  (raise (condition
	  (make-curl-error-condition)
	  (make-curl-error-code-condition code)
	  (make-curl-handle-condition handle)
	  (make-who-condition who)
	  (make-message-condition (cstring->string (curl_easy_strerror code))))))

(define (raise-curl-multi-error who code handle)
  (raise (condition
	  (make-curl-error-condition)
	  (make-curl-error-code-condition code)
	  (make-curl-handle-condition handle)
	  (make-who-condition who)
	  (make-message-condition (cstring->string (curl_multi_strerror code))))))

;;; --------------------------------------------------------------------

(define-condition-type &curl-init-error &curl-error
  make-curl-init-error-condition curl-init-error-condition?)

(define-syntax raise-curl-init-error
  (syntax-rules ()
    ((_ ?who ?message)
     (raise (condition (make-curl-init-error-condition)
		       (make-who-condition ?who)
		       (make-message-condition ?message))))))


;;;; done

)

;;; end of file
