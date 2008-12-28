;;;
;;;Part of: Nausicaa/OSSP/sa
;;;Contents: high level interface to OSSP/sa for R6RS Scheme
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


;;; --------------------------------------------------------------------
;;; Setup.
;;; --------------------------------------------------------------------

(library (ossp-sa)
  (export

    ;; errors
    raise-sa-error sa-error

    ;; address
    make-sa-address			make-sa-address/compensated
    destroy-sa-address
    sa-address-set!			sa-address-ref

    (rename (make-sa-address/compensated make-sa-address/c))

    ;; socket
    make-sa-socket			make-sa-socket/compensated
    destroy-sa-socket
    sa-type sa-timeout sa-buffer sa-option
    sa-bind sa-connect sa-listen
    sa-accept				sa-accept/compensated
    sa-getremote			sa-getremote/compensated
    sa-getlocal				sa-getlocal/compensated
    sa-shutdown				sa-getfd
    sa-read				sa-read-string
    sa-write				sa-write-string
    sa-flush				sa-readln
    sa-recv				sa-send

    (rename (make-sa-socket/compensated	make-sa-socket/c))
    (rename (sa-accept/compensated	sa-accept/c))
    (rename (sa-getlocal/compensated	sa-getlocal/c)))
  (import (r6rs)
    (uriel lang)
    (uriel memory)
    (uriel ffi)
    (uriel ffi sizeof)
    (uriel cstring)
    (ossp-sa foreign)
    (ossp-sa sizeof)
    (srfi receive))


;;; --------------------------------------------------------------------
;;; Address: creation and destruction.
;;; --------------------------------------------------------------------

(define make-sa-address
  (case-lambda
   (()
    (with-compensations
      (let* ((address*	(malloc-small/c))
	     (e		(sa_addr_create address*)))
	(unless (= SA_OK e)
	  (raise-sa-error 'make-sa-address e #f))
	(pointer-ref-c-pointer address* 0))))
   ((uri)
    (with-compensations
      (let ((address	(make-sa-address)))
	(guard (exc (else
		     (destroy-sa-address)
		     (raise exc)))
	  (sa-address-set! address uri))
	address)))))

(define (destroy-sa-address address)
  (let ((e (sa_addr_destroy address)))
    (unless (= SA_OK e)
      (raise-sa-error 'destroy-sa-address e address))))

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
    (let* ((spec	(string->cstring/c uri))
	   (e		(sa_addr_u2a address spec)))
      (unless (= SA_OK e)
	(destroy-sa-address address)
	(raise-sa-error 'sa-address-set! e (list address uri))))))

(define (sa-address-ref address)
  (with-compensations
    (let* ((spec*	(malloc-small/c)))
      (compensate
	  (let ((e (sa_addr_a2u address spec*)))
	    (unless (= SA_OK e)
	      (raise-sa-error 'make-sa-address e #f)))
	(with
	 (primitive-free (pointer-ref-c-pointer spec* 0))))
      (cstring->string (pointer-ref-c-pointer spec* 0)))))



;;; --------------------------------------------------------------------
;;; Socket: constructor and destructor.
;;; --------------------------------------------------------------------

(define (make-sa-socket)
  (with-compensations
    (let* ((socket*	(malloc-small/c))
	   (e		(sa_create socket*)))
      (unless (= SA_OK e)
	(raise-sa-error 'make-sa-socket e))
      (pointer-ref-c-pointer socket* 0))))

(define (destroy-sa-socket socket)
  (let ((e (sa_destroy socket)))
    (unless (= SA_OK e)
      (raise-sa-error 'destroy-sa-socket e))))

(define (make-sa-socket/compensated)
  (letrec ((socket (compensate (make-sa-socket)
		     (with (destroy-sa-socket socket)))))
    socket))


;;; --------------------------------------------------------------------
;;; Socket: configuration.
;;; --------------------------------------------------------------------

(define (sa-type socket type)
  (let ((e (sa_type socket type)))
    (unless (= SA_OK e)
      (raise-sa-error 'sa-type e (list socket type)))))

(define (sa-timeout socket timeout sec usec)
  (let ((e (sa_timeout socket timeout sec usec)))
    (unless (= SA_OK e)
      (raise-sa-error 'sa-timeout e
		      (list socket (sa-timeout-option->symbol timeout)
			    sec usec)))))

(define (sa-buffer socket buffer size)
  (let ((e (sa_buffer socket buffer size)))
    (unless (= SA_OK e)
      (raise-sa-error 'sa-buffer e
		      (list socket (sa-buffer-option->symbol buffer) size)))))

(define (sa-option socket option value)
  (let ((e (sa_option socket option value)))
    (unless (= SA_OK e)
      (raise-sa-error 'sa-option e
		      (list socket (sa-socket-option->symbol option) value)))))



;;; --------------------------------------------------------------------
;;; Socket: operations.
;;; --------------------------------------------------------------------

(define (sa-bind socket address)
  (let ((e (sa_bind socket address)))
    (unless (= SA_OK e)
      (raise-sa-error 'sa-bind e (list socket address)))))

(define (sa-connect socket address)
  (let ((e (sa_connect socket address)))
    (unless (= SA_OK e)
      (raise-sa-error 'sa-connect e (list socket address)))))

(define (sa-listen server-socket backlog)
  (let ((e (sa_listen server-socket backlog)))
    (unless (= SA_OK e)
      (raise-sa-error 'sa-listen e (list server-socket backlog)))))

(define (sa-accept server-socket)
  (with-compensations
    (let* ((client-address*	(malloc-small/c))
	   (client-socket*	(malloc-small/c))
	   (e			(sa_accept server-socket
					   client-address*
					   client-socket*)))
      (unless (= SA_OK e)
	(raise-sa-error 'sa-accept e server-socket))
      (values (pointer-ref-c-pointer client-address* 0)
	      (pointer-ref-c-pointer client-socket*  0)))))

(define (sa-accept/compensated server-socket)
  (let ((client-addr #f)
	(client-sock #f))
    (compensate
	(receive (addr sock)
	    (sa-accept server-socket)
	  (set! client-addr addr)
	  (set! client-sock sock))
      (with
       (destroy-sa-address client-addr)
       (destroy-sa-socket  client-sock)))
    (values client-addr client-sock)))

(define (sa-getremote socket)
  (with-compensations
    (let* ((address*	(malloc-small/c))
	   (e		(sa_getremote socket address*)))
      (unless (= SA_OK e)
	(raise-sa-error 'sa-getremote e socket))
      (pointer-ref-c-pointer address* 0))))

(define (sa-getremote/compensated socket)
  (letrec ((address (compensate
			(sa-getremote socket)
		      (with
		       (destroy-sa-address address)))))
    address))

(define (sa-getlocal socket)
  (with-compensations
    (let* ((address*	(malloc-small/c))
	   (e		(sa_getlocal socket address*)))
      (unless (= SA_OK e)
	(raise-sa-error 'sa-getlocal e socket))
      (pointer-ref-c-pointer address* 0))))

(define (sa-getlocal/compensated socket)
  (letrec ((address (compensate
			(sa-getlocal socket)
		      (with
		       (destroy-sa-address address)))))
    address))

(define (sa-shutdown socket flags)
  (with-compensations
    (let* ((cflags	(string->cstring/c flags))
	   (e		(sa_shutdown socket cflags)))
      (unless (= SA_OK e)
	(raise-sa-error 'sa-shutdown e (list socket flags))))))



;;; --------------------------------------------------------------------
;;; Socket: stream input/output.
;;; --------------------------------------------------------------------

(define (sa-getfd socket)
  (with-compensations
    (let* ((fd*		(malloc-small/c))
	   (e		(sa_getfd socket fd*)))
      (unless (= SA_OK e)
	(raise-sa-error 'sa-getfd e socket))
      (pointer-ref-c-pointer fd* 0))))

(define (sa-read socket memblock)
  (with-compensations
    (let* ((bufdone*	(malloc-small/c))
	   (e		(sa_read socket
				 (memblock-pointer memblock)
				 (memblock-size memblock)
				 bufdone*)))
      (unless (= SA_OK e)
	(raise-sa-error 'sa-read e (list socket memblock)))
      (pointer-ref-c-signed-int bufdone* 0))))

(define (sa-readln socket memblock)
  (with-compensations
    (let* ((bufdone*	(malloc-small/c))
	   (e		(sa_readln socket
				   (memblock-pointer memblock)
				   (memblock-size memblock)
				   bufdone*)))
      (unless (= SA_OK e)
	(raise-sa-error 'sa-readln e (list socket memblock)))
      (pointer-ref-c-signed-int bufdone* 0))))

(define (sa-read-string socket size)
  (with-compensations
    (let* ((mb	(malloc-memblock/c size))
	   (len	(sa-readln socket mb)))
      (cstring->string/len (memblock-pointer mb) len))))

(define (sa-write socket memblock)
  (with-compensations
    (let* ((bufdone*	(malloc-small/c))
	   (e		(sa_write socket
				  (memblock-pointer memblock)
				  (memblock-size memblock)
				  bufdone*)))
      (unless (= SA_OK e)
	(raise-sa-error 'sa-write e (list socket memblock)))
      (pointer-ref-c-signed-int bufdone* 0))))

(define (sa-write-string socket string)
  (with-compensations
    (sa-write socket (make-memblock (string->cstring/c string)
				    (string-length string)))))

(define (sa-flush socket)
  (let ((e (sa_flush socket)))
    (unless (= SA_OK e)
      (raise-sa-error 'sa-flush e socket))))



;;; --------------------------------------------------------------------
;;; Socket: datagram input/output.
;;; --------------------------------------------------------------------

(define (sa-recv socket memblock)
  (with-compensations
    (let* ((bufdone*	(malloc-small/c))
	   (address*	(malloc-small/c))
	   (e		(sa_recv socket address*
				 (memblock-pointer memblock)
				 (memblock-size    memblock)
				 bufdone*)))
      (unless (= SA_OK e)
	(raise-sa-error 'sa-recv e (list socket memblock)))
      (values (pointer-ref-c-pointer    address* 0)
	      (pointer-ref-c-signed-int bufdone* 0)))))

(define (sa-send socket address memblock)
  (with-compensations
    (let* ((bufdone*	(malloc-small/c))
	   (e		(sa_send socket address
				 (memblock-pointer memblock)
				 (memblock-size    memblock)
				 bufdone*)))
      (unless (= SA_OK e)
	(raise-sa-error 'sa-send e (list socket address memblock)))
      (pointer-ref-c-signed-int bufdone* 0))))



;;; --------------------------------------------------------------------
;;; Conditions.
;;; --------------------------------------------------------------------

(define-condition-type &sa-error &error
  make-sa-condition sa-condition?
  (code sa-error-code))

(define (sa-error retval)
  (cstring->string (sa_error retval)))

(define (raise-sa-error who retval irritants)
  (if (= SA_ERR_MEM retval)
      (raise-out-of-memory who #f)
    (raise (condition (make-who-condition who)
		      (make-message-condition (sa-error retval))
		      (make-sa-condition (sa-return-value->symbol retval))
		      (make-irritants-condition irritants)))))




;;; --------------------------------------------------------------------
;;; Done.
;;; --------------------------------------------------------------------

)

;;; end of file
