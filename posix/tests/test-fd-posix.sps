;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: test for file descriptors library
;;;Date: Sun Dec  7, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008-2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(import (nausicaa)
  (nausicaa checks)
  (nausicaa strings)
  (nausicaa ffi)
  (nausicaa ffi memory)
  (nausicaa ffi cstrings)
  (nausicaa ffi errno)
  (for (prefix (nausicaa posix typedefs)   px.) expand run)
  (for (prefix (nausicaa posix extensions) px.) expand)
  (prefix (nausicaa posix fd)     px.)
  (prefix (nausicaa posix sizeof) px.))

(check-set-mode! 'report-failed)
(display "*** testing POSIX fd\n")

(define TMPDIR (get-environment-variable "TMPDIR"))

(define the-pathname (string-join (list TMPDIR "name.ext") "/"))

(define the-string "Le Poete est semblable au prince des nuees
Qui hante la tempete e se rit de l'archer;
Exile sul le sol au milieu des huees,
Ses ailes de geant l'empechent de marcher.")


(parameterize ((check-test-name	'basic)
	       (debugging	#t))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in basic" E))
    (lambda ()

      (check	;open, close, write, read, lseek, fdatasync
	  (with-compensations
	    (let ((pathname the-pathname))
	      (letrec ((fd (compensate
			       (px.open pathname
					(bitwise-ior px.O_CREAT px.O_RDWR)
					(bitwise-ior px.S_IRUSR px.S_IWUSR))
			     (with
			      (px.close fd)))))
		(let* ((bufptr	(string->cstring/c the-string))
		       (buflen	(strlen bufptr))
		       (buflen2	buflen)
		       (bufptr2	(malloc-block/c buflen2)))
		  (px.write fd bufptr buflen)
		  (px.fdatasync fd)
		  (px.lseek fd 0 px.SEEK_SET)
		  (px.read fd bufptr2 buflen2)
		  (cstring->string bufptr2 buflen2)))))
	=> the-string)

      (check ;with enumerations: open, close, write, read, lseek, fdatasync
	  (with-compensations
	    (let ((pathname the-pathname))
	      (letrec ((fd (compensate
			       (px.open pathname
					(px.open-mode creat rdwr)
					(px.access-permissions user-read user-write))
			     (with
			      (px.close fd)))))
		(let* ((bufptr	(string->cstring/c the-string))
		       (buflen	(strlen bufptr))
		       (buflen2	buflen)
		       (bufptr2	(malloc-block/c buflen2)))
		  (px.write fd bufptr buflen)
		  (px.fdatasync fd)
		  (px.lseek fd 0 px.SEEK_SET)
		  (px.read fd bufptr2 buflen2)
		  (cstring->string bufptr2 buflen2)))))
	=> the-string)

      (check 	;open, close, pwrite, pread, lseek, fsync
	  (with-compensations
	    (let ((pathname the-pathname))
	      (letrec ((fd (compensate
			       (px.open pathname
					(px.open-mode creat rdwr)
					(px.access-permissions user-read user-write))
			     (with
			      (px.close fd)))))
		(let* ((bufptr	(string->cstring/c the-string))
		       (buflen	(strlen bufptr))
		       (buflen2	buflen)
		       (bufptr2	(malloc-block/c buflen2)))
		  (px.pwrite fd bufptr buflen 0)
		  (px.fsync fd)
		  (px.pread fd bufptr2 buflen2 0)
		  (cstring->string bufptr2 buflen2)))))
	=> the-string)

      (check	;open, close, write, read, lseek, sync
	  (with-compensations
	    (let ((pathname the-pathname))
	      (letrec ((fd (compensate
			       (px.open pathname
					(bitwise-ior px.O_CREAT px.O_RDWR)
					(bitwise-ior px.S_IRUSR px.S_IWUSR))
			     (with
			      (px.close fd)))))
		(let* ((bufptr	(string->cstring/c the-string))
		       (buflen	(strlen bufptr))
		       (buflen2	buflen)
		       (bufptr2	(malloc-block/c buflen2))
		       (len		(string-length "Le Poete est semblable au prince des nuees\n")))
		  (px.write fd bufptr buflen)
		  (px.sync)
		  (px.lseek fd len px.SEEK_SET)
		  (px.read fd bufptr2 buflen2)
		  (cstring->string bufptr2 (- buflen2 len))))))
	=> "Qui hante la tempete e se rit de l'archer;
Exile sul le sol au milieu des huees,
Ses ailes de geant l'empechent de marcher.")

      #f)))


(parametrise ((check-test-name	'dup)
	      (debugging	#t))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in dup" E))
    (lambda ()

      (check
	  (with-compensations
	    (let ((pathname the-pathname))
	      (letrec ((fd (compensate
			       (px.open pathname
					(bitwise-ior px.O_CREAT px.O_RDWR)
					(bitwise-ior px.S_IRUSR px.S_IWUSR))
			     (with
			      (px.close fd)))))
		(let* ((bufptr	(string->cstring/c the-string))
		       (buflen	(strlen bufptr))
		       (buflen2	buflen)
		       (bufptr2	(malloc-block/c buflen2)))
		  (px.write fd bufptr buflen)
		  (letrec ((fd2 (compensate
				    (px.dup fd)
				  (with
				   (px.close fd2)))))
		    (px.lseek fd2 0 px.SEEK_SET)
		    (px.read fd2 bufptr2 buflen2))
		  (cstring->string bufptr2 buflen2)))))
	=> the-string)

      (check
	  (with-compensations
	    (let ((pathname the-pathname))
	      (letrec ((fd (compensate
			       (px.open pathname
					(bitwise-ior px.O_CREAT px.O_RDWR)
					(bitwise-ior px.S_IRUSR px.S_IWUSR))
			     (with
			      (px.close fd)))))
		(let* ((bufptr	(string->cstring/c the-string))
		       (buflen	(strlen bufptr))
		       (buflen2	buflen)
		       (bufptr2	(malloc-block/c buflen2)))
		  (px.write fd bufptr buflen)
		  (letrec ((fd2 (compensate
				    (px.dup2 fd 123)
				  (with
				   (px.close fd2)))))
		    (px.lseek fd2 0 px.SEEK_SET)
		    (px.read fd2 bufptr2 buflen2))
		  (cstring->string bufptr2 buflen2)))))
	=> the-string)

      #f)))


(parametrise ((check-test-name	'lock)
	      (debugging	#t))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in lock" E))
    (lambda ()

      (check
	  (with-compensations
	    (let ((pathname the-pathname))
	      (letrec ((fd (compensate
			       (px.open pathname
					(bitwise-ior px.O_CREAT px.O_RDWR)
					(bitwise-ior px.S_IRUSR px.S_IWUSR))
			     (with
			      (px.close fd)))))
		(let* ((bufptr	(string->cstring/c the-string))
		       (buflen	(strlen bufptr))
		       (buflen2	buflen)
		       (bufptr2	(malloc-block/c buflen2)))
		  (px.write fd bufptr buflen)
		  (px.lseek fd 0 px.SEEK_SET)
		  (let ((lock	(px.make-struct-flock malloc-block/c)))
		    (with-fields* (((type whence start len) px.struct-flock* lock))
		      (set! lock.type   px.F_WRLCK)
		      (set! lock.whence px.SEEK_SET)
		      (set! lock.start  0)
		      (set! lock.len    10)
		      (compensate
			  (px.fcntl fd px.F_SETLK lock)
			(with
			 (px.fcntl fd px.F_UNLCK lock)))
		      (px.read fd bufptr2 buflen2)
		      (px.fcntl fd px.F_GETLK lock)
		      (cstring->string bufptr2 buflen2)))))))
	=> the-string)

      #t)))


(parametrise ((check-test-name	'scatter/gather)
	      (debugging	#f))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in scatter/gather" E))
    (lambda ()

      (check
	  (with-compensations
	    (let ((pathname the-pathname))
	      (letrec ((fd		(compensate
					    (px.open pathname
						     (bitwise-ior px.O_CREAT px.O_RDWR)
						     (bitwise-ior px.S_IRUSR px.S_IWUSR))
					  (with
					   (px.close fd)))))
		(let* ((iovec-count	3)
		       (iovec**	(malloc-block/c (px.sizeof-iovec-array iovec-count))))

		  (let loop ((i	0)
			     (ell	'("ciao" "salut" "hello")))
		    (unless (= i iovec-count)
		      (let ((iovec*	(px.array-ref-c-iovec iovec** i))
			    (cstr	(string->cstring/c (car ell))))
			(px.struct-iovec-iov_base-set! iovec* cstr)
			(px.struct-iovec-iov_len-set!  iovec* (strlen cstr)))
		      (loop (+ 1 i) (cdr ell))))

		  (px.writev fd iovec** iovec-count)
		  (px.lseek  fd 0 px.SEEK_SET)
		  (px.readv  fd iovec** iovec-count)

		  (let loop ((i	0)
			     (ell	'()))
		    (if (= i iovec-count)
			ell
		      (let ((iovec*	(px.array-ref-c-iovec iovec** i)))
			(loop (+ 1 i)
			      (cons (cstring->string (px.struct-iovec-iov_base-ref iovec*)
						     (px.struct-iovec-iov_len-ref  iovec*))
				    ell)))))))))
	=> '("hello" "salut" "ciao"))

      #t)))


(parametrise ((check-test-name	'pipe)
	      (debugging	#t))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in pipe" E))
    (lambda ()

      (check	;raw fd
	  (with-compensations
	    (let-values (((in ou) (px.pipe)))
	      (push-compensation (px.close in))
	      (push-compensation (px.close ou))
	      (let ((s (string->cstring/c "ciao\n")))
		(px.write ou s (strlen s)))
	      (let* ((p	(malloc 10))
		     (len	(px.read in p 10)))
		(cstring->string p len))))
	=> "ciao\n")

      (check	;binary port
	  (with-compensations
	    (receive (in ou)
		(px.pipe)
	      (letrec ((inp	(compensate
				    (px.fd->binary-input-port  in)
				  (with
				   (close-port inp))))
		       (oup	(compensate
				    (px.fd->binary-output-port ou)
				  (with
				   (close-port oup)))))
		(put-bytevector oup (string->bytevector "ciao\n" (native-transcoder)))
		(flush-output-port oup)
		(bytevector->string (get-bytevector-n inp 5) (native-transcoder)))))
	=> "ciao\n")

      (check	;textual port
	  (with-compensations
	    (receive (in ou)
		(px.pipe)
	      (letrec ((inp	(compensate
				    (px.fd->textual-input-port in)
				  (with
				   (close-port inp))))
		       (oup	(compensate
				    (px.fd->textual-output-port ou)
				  (with
				   (close-port oup)))))
		(put-string oup "ciao\n")
		(flush-output-port oup)
		(get-string-n inp 5))))
	=> "ciao\n")

      (check	;pipe binary ports
	  (with-compensations
	    (receive (inp oup)
		(px.pipe-binary-ports)
	      (push-compensation (close-port inp))
	      (push-compensation (close-port oup))
	      (put-bytevector oup (string->bytevector "ciao\n" (native-transcoder)))
	      (flush-output-port oup)
	      (bytevector->string (get-bytevector-n inp 5) (native-transcoder))))
	=> "ciao\n")

      (check	;textual port
	  (with-compensations
	    (receive (inp oup)
		(px.pipe-textual-ports)
	      (push-compensation (close-port oup))
	      (push-compensation (close-port inp))
	      (put-string oup "ciao\n")
	      (flush-output-port oup)
	      (get-string-n inp 5)))
	=> "ciao\n")

      #f)))


(parametrise ((check-test-name	'fifo)
	      (debugging	#t))

  (define pathname
    (begin0-let ((p (string-join (list (get-environment-variable "TMPDIR") "fifo") "/")))
      (when (file-exists? p)
	(delete-file p))))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in fifo" E))
    (lambda ()

      (check	;binary port
	  (with-compensations
	      (compensate
		  (px.mkfifo pathname (bitwise-ior px.S_IRUSR px.S_IWUSR))
		(with
		 (delete-file pathname)))
;;;(debug  "created  fifo,   now  opening~%")
	    ;;These file  descriptors will be closed  when closing the
	    ;;Scheme ports below.
	    (let* ((in (px.open pathname (bitwise-ior px.O_NONBLOCK px.O_RDONLY) 0))
		   (ou (px.open pathname px.O_WRONLY 0)))
;;;(debug "opened writing port~%")
;;;(debug "making scheme port~%")
	      (letrec ((inp	(compensate
				    (px.fd->binary-input-port  in)
				  (with
				   (close-port inp))))
		       (oup	(compensate
				    (px.fd->binary-output-port ou)
				  (with
				   (close-port oup)))))
;;;(debug "writing and reading~%")
		(put-bytevector oup (string->bytevector "ciao\n" (native-transcoder)))
		(flush-output-port oup)
		(bytevector->string (get-bytevector-n inp 5) (native-transcoder)))))
	=> "ciao\n")

      (check	;textual port
	  (with-compensations
	      (compensate
		  (px.mkfifo pathname #o600)
		(with
		 (delete-file pathname)))
	    ;;These file  descriptors will be closed  when closing the
	    ;;Scheme ports below.
	    (let* ((in (px.open pathname (bitwise-ior px.O_NONBLOCK px.O_RDONLY) 0))
		   (ou (px.open pathname px.O_WRONLY 0)))
	      (letrec ((inp	(compensate
				    (px.fd->textual-input-port  in)
				  (with
				   (close-port inp))))
		       (oup	(compensate
				    (px.fd->textual-output-port ou)
				  (with
				   (close-port oup)))))
		(put-string oup "ciao\n")
		(flush-output-port oup)
		(get-string-n inp 5))))
	=> "ciao\n")

      #f)))


(parametrise ((check-test-name	'mmap)
	      (debugging	#f))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in mmap" E))
    (lambda ()

      (check
	  (with-compensations
	    (let ((pathname the-pathname))
	      (letrec* ((fd		(compensate
					    (px.open pathname
							(bitwise-ior px.O_CREAT px.O_RDWR)
							(bitwise-ior px.S_IRUSR px.S_IWUSR))
					  (with
					   (px.close fd))))
			(map-len	10)
			(address	(compensate
					    (px.mmap pointer-null map-len
							(bitwise-ior px.PROT_READ px.PROT_WRITE)
							px.MAP_SHARED fd 0)
					  (with
					   (px.munmap address map-len)))))
		(px.write fd (string->cstring/c "0123456789") 10)
		(px.lseek fd 0 px.SEEK_SET)
		(pointer-set-c-signed-char! address 3 (char->integer #\A))
		(px.msync address 5 px.MS_SYNC) ;just to verify that no error occurs
		(list (integer->char (pointer-ref-c-signed-char address 3))
		      (integer->char (pointer-ref-c-signed-char address 5))))))
	=> '(#\A #\5))

      #t)))


(parametrise ((check-test-name	'select)
	      (debugging	#f))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in select" E))
    (lambda ()

      (check	;write to file descriptor, fd as max-fd
	  (with-compensations
	    (let ((pathname the-pathname))
	      (letrec ((fd (compensate
			       (px.open pathname
					(bitwise-ior px.O_CREAT px.O_RDWR)
					(bitwise-ior px.S_IRUSR px.S_IWUSR))
			     (with
			      (px.close fd)))))

		(px.write fd (string->cstring/c "0123456789") 10)

		(let ((rd-fdset	(px.make-fdset malloc-block/c))
		      (wr-fdset	(px.make-fdset malloc-block/c))
		      (ex-fdset	(px.make-fdset malloc-block/c))
		      (timeout	(px.make-struct-timeval malloc-block/c)))

		  (px.FD_ZERO rd-fdset)
		  (px.FD_ZERO wr-fdset)
		  (px.FD_ZERO ex-fdset)

		  (px.FD_SET fd rd-fdset)
		  (px.FD_SET fd wr-fdset)
		  (px.FD_SET fd ex-fdset)

		  (with-fields* (((sec usec) px.struct-timeval* timeout))
		    (set! timeout.sec  1)
		    (set! timeout.usec 0))

		  (px.select fd rd-fdset wr-fdset ex-fdset timeout)

		  (list (px.FD_ISSET fd rd-fdset)
			(px.FD_ISSET fd wr-fdset)
			(px.FD_ISSET fd ex-fdset))))))
	=> '(#f #f #f))

      (check	;write to pipe
	  (with-compensations
	    (let-values (((in ou) (px.pipe)))
	      (push-compensation (px.close in))
	      (push-compensation (px.close ou))

	      (let ((rd-fdset	(px.make-fdset malloc-block/c))
		    (wr-fdset	(px.make-fdset malloc-block/c))
		    (ex-fdset	(px.make-fdset malloc-block/c))
		    (timeout	(px.make-struct-timeval malloc-block/c)))

		(with-fields* (((sec usec) px.struct-timeval* timeout))
		  (set! timeout.sec  1)
		  (set! timeout.usec 0)

		  (px.FD_ZERO rd-fdset)
		  (px.FD_ZERO wr-fdset)
		  (px.FD_ZERO ex-fdset)

		  (px.FD_SET in rd-fdset)

		  (px.write ou (string->cstring/c "ciao") 4)

		  (px.select px.FD_SETSIZE rd-fdset wr-fdset ex-fdset timeout)
		  (px.FD_ISSET in rd-fdset)
		  ))))
	=> #t)

      (check	;write to pipe, #f as max-fd
	  (with-compensations
	    (let-values (((in ou) (px.pipe)))
	      (push-compensation (px.close in))
	      (push-compensation (px.close ou))

	      (let ((rd-fdset	(px.make-fdset malloc-block/c))
		    (wr-fdset	(px.make-fdset malloc-block/c))
		    (ex-fdset	(px.make-fdset malloc-block/c))
		    (timeout	(px.make-struct-timeval malloc-block/c)))

		(with-fields* (((sec usec) px.struct-timeval* timeout))
		  (set! timeout.sec  1)
		  (set! timeout.usec 0)

		  (px.FD_ZERO rd-fdset)
		  (px.FD_ZERO wr-fdset)
		  (px.FD_ZERO ex-fdset)

		  (px.FD_SET in rd-fdset)

		  (px.write ou (string->cstring/c "ciao") 4)

		  (px.select #f rd-fdset wr-fdset ex-fdset timeout)
		  (px.FD_ISSET in rd-fdset)
		  ))))
	=> #t)

      (check	;write to pipe, /interruptible
	  (with-compensations
	    (let-values (((in ou) (px.pipe)))
	      (push-compensation (px.close in))
	      (push-compensation (px.close ou))

	      (let ((rd-fdset	(px.make-fdset malloc-block/c))
		    (wr-fdset	(px.make-fdset malloc-block/c))
		    (ex-fdset	(px.make-fdset malloc-block/c))
		    (timeout	(px.make-struct-timeval malloc-block/c)))

		(with-fields* (((sec usec) px.struct-timeval* timeout))
		  (set! timeout.sec  1)
		  (set! timeout.usec 0)

		  (px.FD_ZERO rd-fdset)
		  (px.FD_ZERO wr-fdset)
		  (px.FD_ZERO ex-fdset)

		  (px.FD_SET in rd-fdset)

		  (px.write ou (string->cstring/c "ciao") 4)

		  (px.select/interruptible px.FD_SETSIZE rd-fdset wr-fdset ex-fdset timeout)
		  (px.FD_ISSET in rd-fdset)
		  ))))
	=> #t)

;;; --------------------------------------------------------------------

      (check	;write to pipe, <timeval>
	  (with-compensations
	    (let-values (((in ou) (px.pipe)))
	      (push-compensation (px.close in))
	      (push-compensation (px.close ou))

	      (let ((rd-fdset	(px.make-fdset malloc-block/c))
		    (wr-fdset	(px.make-fdset malloc-block/c))
		    (ex-fdset	(px.make-fdset malloc-block/c))
		    (timeout	(px.make-<timeval> 1 0)))

		(px.FD_ZERO rd-fdset)
		(px.FD_ZERO wr-fdset)
		(px.FD_ZERO ex-fdset)

		(px.FD_SET in rd-fdset)

		(px.write ou (string->cstring/c "ciao") 4)

		(px.select px.FD_SETSIZE rd-fdset wr-fdset ex-fdset timeout)
		(px.FD_ISSET in rd-fdset)
		)))
	=> #t)

      (check	;write to pipe, /interruptible, <timeval>
	  (with-compensations
	    (let-values (((in ou) (px.pipe)))
	      (push-compensation (px.close in))
	      (push-compensation (px.close ou))

	      (let ((rd-fdset	(px.make-fdset malloc-block/c))
		    (wr-fdset	(px.make-fdset malloc-block/c))
		    (ex-fdset	(px.make-fdset malloc-block/c))
		    (timeout	(px.make-<timeval> 1 0)))

		(px.FD_ZERO rd-fdset)
		(px.FD_ZERO wr-fdset)
		(px.FD_ZERO ex-fdset)

		(px.FD_SET in rd-fdset)

		(px.write ou (string->cstring/c "ciao") 4)

		(px.select/interruptible px.FD_SETSIZE rd-fdset wr-fdset ex-fdset timeout)
		(px.FD_ISSET in rd-fdset)
		)))
	=> #t)

;;; --------------------------------------------------------------------

      (with-compensations	;lists rather than fd_sets
      	(let-values (((in ou) (px.pipe)))
      	  (push-compensation (px.close in))
      	  (push-compensation (px.close ou))

      	  (check ;write to pipe, list fdsets
	      (begin
		(px.write ou (string->cstring/c "ciao") 4)
		(receive (readable writable excepted)
		    (px.select* `(,in ,ou) `(,in ,ou) `(,in ,ou)
				(px.make-<timeval> 1 0))
		  (list readable writable excepted)))
      	    => `((,in) (,ou) ()))

      	  #f))

      (with-compensations	;lists rather than fd_sets
      	(let-values (((in ou) (px.pipe)))
      	  (push-compensation (px.close in))
      	  (push-compensation (px.close ou))

      	  (check ;write to pipe, list fdsets
	      (begin
		(px.write ou (string->cstring/c "ciao") 4)
		(receive (readable writable excepted)
		    (px.select*/interruptible `(,in ,ou) `(,in ,ou) `(,in ,ou)
					      (px.make-<timeval> 1 0))
		  (list readable writable excepted)))
      	    => `((,in) (,ou) ()))

      	  #f))

;;; --------------------------------------------------------------------

      (with-compensations	;single fd
      	(let-values (((in ou) (px.pipe)))
      	  (push-compensation (px.close in))
      	  (push-compensation (px.close ou))

      	  (check ;write to pipe, list fdsets
	      (begin
		(px.write ou (string->cstring/c "ciao") 4)
		(let-values (((readable-in writable-in excepted-in)
			      (px.select/fd in (px.make-<timeval> 1 0)))
			     ((readable-ou writable-ou excepted-ou)
			      (px.select/fd ou (px.make-<timeval> 1 0))))
		  (list readable-in writable-in excepted-in
			readable-ou writable-ou excepted-ou)))
      	    => '(#t #f #f
		    #f #t #f))

      	  #f))

      (with-compensations	;single fd
      	(let-values (((in ou) (px.pipe)))
      	  (push-compensation (px.close in))
      	  (push-compensation (px.close ou))

      	  (check ;write to pipe, list fdsets
	      (begin
		(px.write ou (string->cstring/c "ciao") 4)
		(let-values (((readable-in writable-in excepted-in)
			      (px.select/fd/interruptible in (px.make-<timeval> 1 0)))
			     ((readable-ou writable-ou excepted-ou)
			      (px.select/fd/interruptible ou (px.make-<timeval> 1 0))))
		  (list readable-in writable-in excepted-in
			readable-ou writable-ou excepted-ou)))
      	    => '(#t #f #f
		    #f #t #f))

      	  #f))

      #t)))


;;;; done

(check-report)

;;; end of file
