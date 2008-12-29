;;;
;;;Part of: Nausicaa/OSSP/sa
;;;Contents: low level interface to OSSP/sa for R6RS Scheme
;;;Date: Sat Dec 13, 2008
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

(library (ossp-sa stub)
  (export
    sa_addr_create
    sa_addr_destroy
    sa_addr_u2a
    sa_addr_s2a
    sa_addr_a2u
    sa_addr_a2s
    sa_addr_match
    sa_create
    sa_destroy
    sa_type
    sa_timeout
    sa_buffer
    sa_option
    sa_syscall
    sa_bind
    sa_connect
    sa_listen
    sa_accept
    sa_getremote
    sa_getlocal
    sa_getfd
    sa_shutdown
    sa_read
    sa_readln
    sa_write
;;; sa_writef
    sa_flush
    sa_recv
    sa_send
;;; sa_sendf
    sa_error)
  (import (r6rs)
    (uriel ffi)
    (uriel ffi sizeof)
    (ossp-sa sizeof))

(define sa-lib
  (let ((o (open-shared-object 'libsa.so)))
    (shared-object o)
    o))

;;; address allocation
(define-c-function sa_addr_create
  (sa_rc_t sa_addr_create (sa_addr_t**)))

(define-c-function sa_addr_destroy
  (sa_rc_t sa_addr_destroy (sa_addr_t*)))

;;; address operations
(define-c-function sa_addr_u2a
  (sa_rc_t sa_addr_u2a (sa_addr_t* char*)))
(define-c-function sa_addr_s2a
  (sa_rc_t sa_addr_s2a (sa_addr_t* pointer int)))
(define-c-function sa_addr_a2u
  (sa_rc_t sa_addr_a2u (sa_addr_t* pointer)))
(define-c-function sa_addr_a2s
  (sa_rc_t sa_addr_a2s (sa_addr_t* pointer pointer)))
(define-c-function sa_addr_match
  (sa_rc_t sa_addr_match (sa_addr_t* sa_addr_t* int)))

;;; socket object operations
(define-c-function sa_create
  (sa_rc_t sa_create (sa_t**)))
(define-c-function sa_destroy
  (sa_rc_t sa_destroy (sa_t*)))

;;; socket parameter operations
(define-c-function sa_type
  (sa_rc_t sa_type (sa_t* sa_type_t)))
(define-c-function sa_timeout
  (sa_rc_t sa_timeout (sa_t* sa_timeout_t long long)))
(define-c-function sa_buffer
  (sa_rc_t sa_buffer (sa_t* sa_buffer_t size_t)))
(define-c-function sa_option
  (sa_rc_t sa_option (sa_t* sa_option_t int)))
(define-c-function sa_syscall
  (sa_rc_t sa_syscall (sa_t* sa_syscall_t callback)))

;;; socket connection operations
(define-c-function sa_bind
  (sa_rc_t sa_bind (sa_t* sa_addr_t*)))
(define-c-function sa_connect
  (sa_rc_t sa_connect (sa_t* sa_addr_t*)))
(define-c-function sa_listen
  (sa_rc_t sa_listen (sa_t* int)))
(define-c-function sa_accept
  (sa_rc_t sa_accept (sa_t* sa_addr_t** sa_t**)))
(define-c-function sa_getremote
  (sa_rc_t sa_getremote (sa_t* sa_addr_t**)))
(define-c-function sa_getlocal
  (sa_rc_t sa_getlocal (sa_t* sa_addr_t**)))
(define-c-function sa_getfd
  (sa_rc_t sa_getfd (sa_t* pointer)))
(define-c-function sa_shutdown
  (sa_rc_t sa_shutdown (sa_t* char*)))

;;; socket input/output operations (stream communication)
(define-c-function sa_read
  (sa_rc_t sa_read (sa_t* char* size_t pointer)))
(define-c-function sa_readln
  (sa_rc_t sa_readln (sa_t* char* size_t pointer)))
(define-c-function sa_write
  (sa_rc_t sa_write (sa_t* char* size_t pointer)))
;; (define-c-function sa_writef
;;   (sa_rc_t sa_writef (sa_t* char* ...)))
(define-c-function sa_flush
  (sa_rc_t sa_flush (sa_t*)))

;;; socket input/output operations (datagram communication)
(define-c-function sa_recv
  (sa_rc_t sa_recv (sa_t* sa_addr_t** char* size_t pointer)))
(define-c-function sa_send
  (sa_rc_t sa_send (sa_t* sa_addr_t* char* size_t pointer)))
;; (define-c-function sa_sendf
;;   (sa_rc_t sa_sendf (sa_t* sa_addr_t* char* ...)))

;;; error handling operations
(define-c-function sa_error
  (char* sa_error (sa_rc_t))))

;;; end of file
