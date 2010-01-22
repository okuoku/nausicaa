;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: test for file descriptors library
;;;Date: Sun Dec  7, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008, 2009, 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (checks)
  (strings)
  (records)
  (foreign ffi)
  (foreign memory)
  (foreign cstrings)
  (foreign errno)
  (for (posix typedefs) expand run)
  (for (posix extensions) expand)
  (prefix (posix fd) posix:)
  (posix sizeof)
  (deferred-exceptions)
  (compensations))

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
			       (posix:open pathname
					   (bitwise-ior O_CREAT O_RDWR)
					   (bitwise-ior S_IRUSR S_IWUSR))
			     (with
			      (posix:close fd)))))
		(let* ((bufptr	(string->cstring/c the-string))
		       (buflen	(strlen bufptr))
		       (buflen2	buflen)
		       (bufptr2	(malloc-block/c buflen2)))
		  (posix:write fd bufptr buflen)
		  (posix:fdatasync fd)
		  (posix:lseek fd 0 SEEK_SET)
		  (posix:read fd bufptr2 buflen2)
		  (cstring->string bufptr2 buflen2)))))
	=> the-string)

      (check	;with enumerations: open, close, write, read, lseek, fdatasync
	  (with-compensations
	    (let ((pathname the-pathname))
	      (letrec ((fd (compensate
			       (posix:open pathname
					   (open-mode creat rdwr)
					   (access-permissions user-read user-write))
			     (with
			      (posix:close fd)))))
		(let* ((bufptr	(string->cstring/c the-string))
		       (buflen	(strlen bufptr))
		       (buflen2	buflen)
		       (bufptr2	(malloc-block/c buflen2)))
		  (posix:write fd bufptr buflen)
		  (posix:fdatasync fd)
		  (posix:lseek fd 0 SEEK_SET)
		  (posix:read fd bufptr2 buflen2)
		  (cstring->string bufptr2 buflen2)))))
	=> the-string)

      (check 	;open, close, pwrite, pread, lseek, fsync
	  (with-compensations
	    (let ((pathname the-pathname))
	      (letrec ((fd (compensate
			       (posix:open pathname
					   (open-mode creat rdwr)
					   (access-permissions user-read user-write))
			     (with
			      (posix:close fd)))))
		(let* ((bufptr	(string->cstring/c the-string))
		       (buflen	(strlen bufptr))
		       (buflen2	buflen)
		       (bufptr2	(malloc-block/c buflen2)))
		  (posix:pwrite fd bufptr buflen 0)
		  (posix:fsync fd)
		  (posix:pread fd bufptr2 buflen2 0)
		  (cstring->string bufptr2 buflen2)))))
	=> the-string)

      (check	;open, close, write, read, lseek, sync
	  (with-compensations
	    (let ((pathname the-pathname))
	      (letrec ((fd (compensate
			       (posix:open pathname
					   (bitwise-ior O_CREAT O_RDWR)
					   (bitwise-ior S_IRUSR S_IWUSR))
			     (with
			      (posix:close fd)))))
		(let* ((bufptr	(string->cstring/c the-string))
		       (buflen	(strlen bufptr))
		       (buflen2	buflen)
		       (bufptr2	(malloc-block/c buflen2))
		       (len		(string-length "Le Poete est semblable au prince des nuees\n")))
		  (posix:write fd bufptr buflen)
		  (posix:sync)
		  (posix:lseek fd len SEEK_SET)
		  (posix:read fd bufptr2 buflen2)
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
			       (posix:open pathname
					   (bitwise-ior O_CREAT O_RDWR)
					   (bitwise-ior S_IRUSR S_IWUSR))
			     (with
			      (posix:close fd)))))
		(let* ((bufptr	(string->cstring/c the-string))
		       (buflen	(strlen bufptr))
		       (buflen2	buflen)
		       (bufptr2	(malloc-block/c buflen2)))
		  (posix:write fd bufptr buflen)
		  (letrec ((fd2 (compensate
				    (posix:dup fd)
				  (with
				   (posix:close fd2)))))
		    (posix:lseek fd2 0 SEEK_SET)
		    (posix:read fd2 bufptr2 buflen2))
		  (cstring->string bufptr2 buflen2)))))
	=> the-string)

      (check
	  (with-compensations
	    (let ((pathname the-pathname))
	      (letrec ((fd (compensate
			       (posix:open pathname
					   (bitwise-ior O_CREAT O_RDWR)
					   (bitwise-ior S_IRUSR S_IWUSR))
			     (with
			      (posix:close fd)))))
		(let* ((bufptr	(string->cstring/c the-string))
		       (buflen	(strlen bufptr))
		       (buflen2	buflen)
		       (bufptr2	(malloc-block/c buflen2)))
		  (posix:write fd bufptr buflen)
		  (letrec ((fd2 (compensate
				    (posix:dup2 fd 123)
				  (with
				   (posix:close fd2)))))
		    (posix:lseek fd2 0 SEEK_SET)
		    (posix:read fd2 bufptr2 buflen2))
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
			       (posix:open pathname
					   (bitwise-ior O_CREAT O_RDWR)
					   (bitwise-ior S_IRUSR S_IWUSR))
			     (with
			      (posix:close fd)))))
		(let* ((bufptr	(string->cstring/c the-string))
		       (buflen	(strlen bufptr))
		       (buflen2	buflen)
		       (bufptr2	(malloc-block/c buflen2)))
		  (posix:write fd bufptr buflen)
		  (posix:lseek fd 0 SEEK_SET)
		  (let ((lock	(make-struct-flock malloc-block/c)))
		    (with-fields* (((type whence start len) struct-flock* lock))
		      (set! lock.type   F_WRLCK)
		      (set! lock.whence SEEK_SET)
		      (set! lock.start  0)
		      (set! lock.len    10)
		      (compensate
			  (posix:fcntl fd F_SETLK lock)
			(with
			 (posix:fcntl fd F_UNLCK lock)))
		      (posix:read fd bufptr2 buflen2)
		      (posix:fcntl fd F_GETLK lock)
		      (cstring->string bufptr2 buflen2)))))))
	=> the-string)

      #t)))


(parametrise ((check-test-name	'scatter/gather)
	      (debugging	#f))

  (check
      (with-compensations
	(let ((pathname the-pathname))
	  (letrec ((fd		(compensate
				    (posix:open pathname
						(bitwise-ior O_CREAT O_RDWR)
						(bitwise-ior S_IRUSR S_IWUSR))
				  (with
				   (posix:close fd)))))
	    (let* ((iovec-count	3)
		   (iovec**	(malloc-block/c (sizeof-iovec-array iovec-count))))

	      (let loop ((i	0)
			 (ell	'("ciao" "salut" "hello")))
		(unless (= i iovec-count)
		  (let ((iovec*	(array-ref-c-iovec iovec** i))
			(cstr	(string->cstring/c (car ell))))
		    (struct-iovec-iov_base-set! iovec* cstr)
		    (struct-iovec-iov_len-set!  iovec* (strlen cstr)))
		  (loop (+ 1 i) (cdr ell))))

	      (posix:writev fd iovec** iovec-count)
	      (posix:lseek  fd 0 SEEK_SET)
	      (posix:readv  fd iovec** iovec-count)

	      (let loop ((i	0)
			 (ell	'()))
		(if (= i iovec-count)
		    ell
		  (let ((iovec*	(array-ref-c-iovec iovec** i)))
		    (loop (+ 1 i)
			  (cons (cstring->string (struct-iovec-iov_base-ref iovec*)
						 (struct-iovec-iov_len-ref  iovec*))
				ell)))))))))
    => '("hello" "salut" "ciao"))

  #t)


(parametrise ((check-test-name	'pipe)
	      (debugging	#t))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in pipe" E))
    (lambda ()

      (check	;raw fd
	  (with-compensations
	    (let-values (((in ou) (posix:pipe)))
	      (push-compensation (posix:close in))
	      (push-compensation (posix:close ou))
	      (let ((s (string->cstring/c "ciao\n")))
		(posix:write ou s (strlen s)))
	      (let* ((p	(malloc 10))
		     (len	(posix:read in p 10)))
		(cstring->string p len))))
	=> "ciao\n")

      (check	;binary port
	  (with-compensations
	    (let-values (((in ou) (posix:pipe)))
	      (letrec ((inp	(compensate
				    (posix:fd->binary-input-port  in)
				  (with
				   (close-port inp))))
		       (oup	(compensate
				    (posix:fd->binary-output-port ou)
				  (with
				   (close-port oup)))))
		(put-bytevector oup (string->bytevector "ciao\n" (native-transcoder)))
		(flush-output-port oup)
		(bytevector->string (get-bytevector-n inp 5) (native-transcoder)))))
	=> "ciao\n")

      (check	;textual port
	  (with-compensations
	    (let-values (((in ou) (posix:pipe)))
	      (letrec ((inp	(compensate
				    (posix:fd->textual-input-port in)
				  (with
				   (close-port inp))))
		       (oup	(compensate
				    (posix:fd->textual-output-port ou)
				  (with
				   (close-port oup)))))
		(put-string oup "ciao\n")
		(flush-output-port oup)
		(get-string-n inp 5))))
	=> "ciao\n")

      (check	;pipe binary ports
	  (with-compensations
	    (let-values (((inp oup) (posix:pipe-binary-ports)))
	      (push-compensation (close-port inp))
	      (push-compensation (close-port oup))
	      (put-bytevector oup (string->bytevector "ciao\n" (native-transcoder)))
	      (flush-output-port oup)
	      (bytevector->string (get-bytevector-n inp 5) (native-transcoder))))
	=> "ciao\n")

      (check	;textual port
	  (with-compensations
	    (let-values (((inp oup) (posix:pipe-textual-ports)))
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
		  (posix:mkfifo pathname (bitwise-ior S_IRUSR S_IWUSR))
		(with
		 (delete-file pathname)))
;;;(debug  "created  fifo,   now  opening~%")
	    ;;These file  descriptors will be closed  when closing the
	    ;;Scheme ports below.
	    (let* ((in (posix:open pathname (bitwise-ior O_NONBLOCK O_RDONLY) 0))
		   (ou (posix:open pathname O_WRONLY 0)))
;;;(debug "opened writing port~%")
;;;(debug "making scheme port~%")
	      (letrec ((inp	(compensate
				    (posix:fd->binary-input-port  in)
				  (with
				   (close-port inp))))
		       (oup	(compensate
				    (posix:fd->binary-output-port ou)
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
		  (posix:mkfifo pathname #o600)
		(with
		 (delete-file pathname)))
	    ;;These file  descriptors will be closed  when closing the
	    ;;Scheme ports below.
	    (let* ((in (posix:open pathname (bitwise-ior O_NONBLOCK O_RDONLY) 0))
		   (ou (posix:open pathname O_WRONLY 0)))
	      (letrec ((inp	(compensate
				    (posix:fd->textual-input-port  in)
				  (with
				   (close-port inp))))
		       (oup	(compensate
				    (posix:fd->textual-output-port ou)
				  (with
				   (close-port oup)))))
		(put-string oup "ciao\n")
		(flush-output-port oup)
		(get-string-n inp 5))))
	=> "ciao\n")

      #f)))


(parametrise ((check-test-name	'mmap)
	      (debugging	#f))

  (check
      (with-compensations
	(let ((pathname the-pathname))
	  (letrec* ((fd		(compensate
				    (posix:open pathname
						(bitwise-ior O_CREAT O_RDWR)
						(bitwise-ior S_IRUSR S_IWUSR))
				  (with
				   (posix:close fd))))
		    (map-len	10)
		    (address	(compensate
				    (posix:mmap pointer-null map-len
						(bitwise-ior PROT_READ PROT_WRITE)
						MAP_SHARED fd 0)
				  (with
				   (posix:munmap address map-len)))))
	    (posix:write fd (string->cstring/c "0123456789") 10)
	    (posix:lseek fd 0 SEEK_SET)
	    (pointer-set-c-signed-char! address 3 (char->integer #\A))
	    (posix:msync address 5 MS_SYNC) ;just to verify that no error occurs
	    (list (integer->char (pointer-ref-c-signed-char address 3))
		  (integer->char (pointer-ref-c-signed-char address 5))))))
    => '(#\A #\5))

  #t)


(parametrise ((check-test-name	'select)
	      (debugging	#f))

  (check
      (with-compensations
	(let ((pathname the-pathname))
	  (letrec ((fd (compensate
			   (posix:open pathname
				       (bitwise-ior O_CREAT O_RDWR)
				       (bitwise-ior S_IRUSR S_IWUSR))
			 (with
			  (posix:close fd)))))

	    (posix:write fd (string->cstring/c "0123456789") 10)

	    (let ((rd-fdset	(make-fdset malloc-block/c))
		  (wr-fdset	(make-fdset malloc-block/c))
		  (ex-fdset	(make-fdset malloc-block/c))
		  (timeout	(make-struct-timeval malloc-block/c)))

	      (posix:FD_ZERO rd-fdset)
	      (posix:FD_ZERO wr-fdset)
	      (posix:FD_ZERO ex-fdset)

	      (posix:FD_SET fd rd-fdset)
	      (posix:FD_SET fd wr-fdset)
	      (posix:FD_SET fd ex-fdset)

	      (with-fields* (((sec usec) struct-timeval* timeout))
		(set! timeout.sec  1)
		(set! timeout.usec 0))

	      (posix:select fd rd-fdset wr-fdset ex-fdset timeout)

	      (list (posix:FD_ISSET fd rd-fdset)
		    (posix:FD_ISSET fd wr-fdset)
		    (posix:FD_ISSET fd ex-fdset))))))
    => '(#f #f #f))

  (check
      (with-compensations
	(let-values (((in ou) (posix:pipe)))
	  (push-compensation (posix:close in))
	  (push-compensation (posix:close ou))

	  (let ((rd-fdset	(make-fdset malloc-block/c))
		(wr-fdset	(make-fdset malloc-block/c))
		(ex-fdset	(make-fdset malloc-block/c))
		(timeout	(make-struct-timeval malloc-block/c)))

	    (with-fields* (((sec usec) struct-timeval* timeout))
	      (set! timeout.sec  1)
	      (set! timeout.usec 0)

	      (posix:FD_ZERO rd-fdset)
	      (posix:FD_ZERO wr-fdset)
	      (posix:FD_ZERO ex-fdset)

	      (posix:FD_SET in rd-fdset)

	      (posix:write ou (string->cstring/c "ciao") 4)

	      (posix:select FD_SETSIZE rd-fdset wr-fdset ex-fdset timeout)
	      (posix:FD_ISSET in rd-fdset)
	      ))))
    => #t)

  #t)



;;;; done

(check-report)

;;; end of file
