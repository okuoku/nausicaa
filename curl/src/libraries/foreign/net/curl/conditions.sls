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

    &curl-easy-handle
    make-curl-easy-handle-condition
    curl-easy-handle-condition?
    condition-curl-easy-handle

    &curl-multi-handle
    make-curl-multi-handle-condition
    curl-multi-handle-condition?
    condition-curl-multi-handle

    &curl-sockfd
    make-curl-sockfd-condition
    curl-sockfd-condition?
    condition-curl-sockfd

    &curl-shared-object
    make-curl-shared-object-condition
    curl-shared-object-condition?
    condition-curl-shared-object

    &curl-error-code
    make-curl-error-code-condition
    curl-error-code-condition?
    condition-curl-error-numeric-code
    condition-curl-error-symbolic-code

    &curl-easy-error-code
    make-curl-easy-error-code-condition
    curl-easy-error-code-condition?

    &curl-multi-error-code
    make-curl-multi-error-code-condition
    curl-multi-error-code-condition?

    &curl-share-error-code
    make-curl-share-error-code-condition
    curl-share-error-code-condition?

    make-curl-easy-message-condition
    make-curl-multi-message-condition
    make-curl-share-message-condition

;;; --------------------------------------------------------------------

    &curl-error
    make-curl-error-condition
    curl-error-condition?

    &curl-init-error
    make-curl-init-error-condition
    curl-init-error-condition?

    &curl-action-error
    make-curl-action-error-condition
    curl-action-error-condition?

    &curl-easy-action-error
    make-curl-easy-action-error-condition
    curl-easy-action-error-condition?

    &curl-multi-action-error
    make-curl-multi-action-error-condition
    curl-multi-action-error-condition?

    )
  (import (rnrs)
    (only (foreign cstrings) cstring->string)
    (foreign net curl enumerations)
    (foreign net curl platform))


(define-condition-type &curl-easy-handle &condition
  make-curl-easy-handle-condition curl-easy-handle-condition?
  (handle		condition-curl-easy-handle))

(define-condition-type &curl-multi-handle &condition
  make-curl-multi-handle-condition curl-multi-handle-condition?
  (handle		condition-curl-multi-handle))

(define-condition-type &curl-shared-object &condition
  make-curl-shared-object-condition curl-shared-object-condition?
  (object		condition-curl-shared-object))

(define-condition-type &curl-sockfd &condition
  make-curl-sockfd-condition curl-sockfd-condition?
  (file-descriptor	condition-curl-sockfd))

(define-condition-type &curl-error-code &condition
  make-curl-error-code-condition curl-error-code-condition?
  (numeric-code		condition-curl-error-numeric-code)
  (symbolic-code	condition-curl-error-symbolic-code))

(define-condition-type &curl-easy-error-code &condition
  %make-curl-easy-error-code-condition curl-easy-error-code-condition?)

(define-condition-type &curl-multi-error-code &condition
  %make-curl-multi-error-code-condition curl-multi-error-code-condition?)

(define-condition-type &curl-share-error-code &condition
  %make-curl-share-error-code-condition curl-share-error-code-condition?)

(define (make-curl-easy-error-code-condition numeric-code)
  (%make-curl-easy-error-code-condition numeric-code
					(curl-easy-numeric-code->symbolic-code numeric-code)))

(define (make-curl-multi-error-code-condition numeric-code)
  (%make-curl-multi-error-code-condition numeric-code
					(curl-multi-numeric-code->symbolic-code numeric-code)))

(define (make-curl-share-error-code-condition numeric-code)
  (%make-curl-share-error-code-condition numeric-code
					 (curl-share-numeric-code->symbolic-code numeric-code)))

(define (make-curl-easy-message-condition numeric-code)
  (make-message-condition (cstring->string (curl_easy_strerror numeric-code))))

(define (make-curl-multi-message-condition numeric-code)
  (make-message-condition (cstring->string (curl_multi_strerror numeric-code))))

(define (make-curl-share-message-condition numeric-code)
  (make-message-condition (cstring->string (curl_share_strerror numeric-code))))


(define-condition-type &curl-error &error
  make-curl-error-condition curl-error-condition?)

(define-condition-type &curl-init-error &curl-error
  make-curl-init-error-condition curl-init-error-condition?)

(define-condition-type &curl-action-error &curl-error
  make-curl-action-error-condition curl-action-error-condition?)

(define-condition-type &curl-easy-action-error &curl-action-error
  make-curl-easy-action-error-condition curl-easy-action-error-condition?)

(define-condition-type &curl-multi-action-error &curl-action-error
  make-curl-multi-action-error-condition curl-multi-action-error-condition?)


;;;; done

)

;;; end of file
