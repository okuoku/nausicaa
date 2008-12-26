;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: test for file descriptors library
;;;Date: Sun Dec  7, 2008
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



;;;; setup

(import (except (r6rs) read write)
  (rnrs files (6))
  (only (srfi strings) string-join)
  (uriel lang)
  (uriel foreign)
  (uriel test)
  (env-lib)
  (only (string-lib) string-join)
  (posix fd)
  (posix sizeof))

(check-set-mode! 'report-failed)

(define TMPDIR (get-environment-variable "TMPDIR"))

(define the-pathname (string-join (list TMPDIR "name.ext") "/"))

(define the-string "Le Poete est semblable au prince des nuees
Qui hante la tempete e se rit de l'archer;
Exile sul le sol au milieu des huees,
Ses ailes de geant l'empechent de marcher.")




;;;; basic fd operations

(parameterize ((testname 'basic))

  (check
      (with-compensations
	(let* ((pathname the-pathname)
	       (fd		(open pathname (bitwise-ior O_CREAT O_RDWR) #o600))
	       (bufptr		(string->cstring/c the-string))
	       (buflen		(strlen bufptr))
	       (buflen2		buflen)
	       (bufptr2		(malloc-block/c buflen2)))
	  (write fd bufptr buflen)
;;;	  (print #t "file desc ~s\n" fd)
	  (fdatasync fd)
	  (lseek fd 0 SEEK_SET)
	  (read fd bufptr2 buflen2)
	  (close fd)
	  (cstring->string/len bufptr2 buflen2)))
    => the-string)

  (check
      (with-compensations
	(let* ((pathname the-pathname)
	       (fd		(open pathname (bitwise-ior O_CREAT O_RDWR) #o600))
	       (bufptr		(string->cstring/c the-string))
	       (buflen		(strlen bufptr))
	       (buflen2		buflen)
	       (bufptr2		(malloc-block/c buflen2)))
	  (pwrite fd bufptr buflen 0)
	  (fsync fd)
	  (pread fd bufptr2 buflen2 0)
	  (close fd)
	  (cstring->string/len bufptr2 buflen2)))
    => the-string)

  (check
      (with-compensations
	(let* ((pathname the-pathname)
	       (fd		(letrec
				    ((fd (compensate
					     (open pathname (bitwise-ior O_CREAT O_RDWR) #o600)
					   (with (close fd)))))
				  fd))
	       (bufptr		(string->cstring/c the-string))
	       (buflen		(strlen bufptr))
	       (buflen2		buflen)
	       (bufptr2		(malloc-block/c buflen2))
	       (len		(string-length "Le Poete est semblable au prince des nuees\n")))
	  (write fd bufptr buflen)
	  (sync)
	  (lseek fd len SEEK_SET)
	  (read fd bufptr2 buflen2)
	  (cstring->string/len bufptr2 (- buflen2 len))))
    => "Qui hante la tempete e se rit de l'archer;
Exile sul le sol au milieu des huees,
Ses ailes de geant l'empechent de marcher.")

  )


;;;; duplication

(parameterize ((testname 'dup))

  (check
      (with-compensations
	(let* ((pathname the-pathname)
	       (fd		(open pathname (bitwise-ior O_CREAT O_RDWR) #o600))
	       (bufptr		(string->cstring/c the-string))
	       (buflen		(strlen bufptr))
	       (buflen2		buflen)
	       (bufptr2		(malloc-block/c buflen2)))
	  (write fd bufptr buflen)
  	  (let ((fd2	(dup fd)))
 	    (close fd)
 	    (lseek fd2 0 SEEK_SET)
 	    (read fd2 bufptr2 buflen2)
 	    (close fd2))
	  (cstring->string/len bufptr2 buflen2)))
    => the-string)

  (check
      (with-compensations
	(let* ((pathname the-pathname)
	       (fd		(open pathname (bitwise-ior O_CREAT O_RDWR) #o600))
	       (bufptr		(string->cstring/c the-string))
	       (buflen		(strlen bufptr))
	       (buflen2		buflen)
	       (bufptr2		(malloc-block/c buflen2)))
	  (write fd bufptr buflen)
  	  (let ((fd2	123))
	    (dup2 fd fd2)
 	    (close fd)
 	    (lseek fd2 0 SEEK_SET)
 	    (read fd2 bufptr2 buflen2)
 	    (close fd2))
	  (cstring->string/len bufptr2 buflen2)))
    => the-string)

#f)



(parameterize ((testname 'lock))

  (check
      (with-compensations
	(let* ((pathname the-pathname)
	       (fd		(open pathname (bitwise-ior O_CREAT O_RDWR) #o600))
	       (bufptr		(string->cstring/c the-string))
	       (buflen		(strlen bufptr))
	       (buflen2		buflen)
	       (bufptr2		(malloc-block/c buflen2)))
	  (write fd bufptr buflen)
	  (lseek fd 0 SEEK_SET)
	  (let ((lock	(malloc-block/c sizeof-struct-flock)))
	    (struct-flock-l_type-set! lock F_WRLCK)
	    (struct-flock-l_whence-set! lock SEEK_SET)
	    (struct-flock-l_start-set! lock 0)
	    (struct-flock-l_len-set! lock 10)
	    (fcntl fd F_SETLK (pointer->integer lock))
	    (read fd bufptr2 buflen2)
	    (fcntl fd F_GETLK (pointer->integer lock))
;;; 	    (display (list (struct-flock-l_type-ref lock)
;;; 			   (struct-flock-l_start-ref lock)))(newline)
	    (fcntl fd F_UNLCK (pointer->integer lock))
	    (close fd))
	  (cstring->string/len bufptr2 buflen2)))
    => the-string)

  )



(parameterize ((testname 'pipe))

  ;; raw fd
  (check
      (with-compensations
	(let-values (((in ou) (pipe)))
	  (push-compensation (close in))
	  (push-compensation (close ou))
	  (let ((s	(string->cstring/c "ciao\n")))
	    (write ou s (strlen s)))
	  (let* ((p	(malloc 10))
		 (len	(read in p 10)))
	    (cstring->string/len p len))))
    => "ciao\n")

  ;; binary port
  (check
      (with-compensations
	(let-values (((in ou)	(pipe)))
	  (letrec ((inp	(compensate
			    (fd->binary-input-port  in)
			  (with
			   (close-port inp))))
		   (oup	(compensate
			    (fd->binary-output-port ou)
			  (with
			   (close-port oup)))))
	    (put-bytevector oup
			    (string->bytevector "ciao\n"
						(native-transcoder)))
	    (flush-output-port oup)
	    (bytevector->string (get-bytevector-n inp 5)
				(native-transcoder)))))
    => "ciao\n")

  ;; pipe binary ports
  (check
      (with-compensations
	(let-values (((inp oup)	(pipe-binary-ports)))
	  (push-compensation (close-port inp))
	  (push-compensation (close-port oup))
	  (put-bytevector oup
			  (string->bytevector "ciao\n"
					      (native-transcoder)))
	  (flush-output-port oup)
	  (bytevector->string (get-bytevector-n inp 5)
			      (native-transcoder))))
    => "ciao\n")

  ;; textual port
  (check
      (with-compensations
	(let-values (((in ou)	(pipe)))
	  (letrec ((inp	(compensate
			    (fd->textual-input-port  in)
			  (with
			   (close-port inp))))
		   (oup	(compensate
			    (fd->textual-output-port ou)
			  (with
			   (close-port oup)))))
	    (put-string oup "ciao\n")
	    (flush-output-port oup)
	    (get-string-n inp 5))))
    => "ciao\n")

  ;; textual port
  (check
      (with-compensations
	(let-values (((inp oup)	(pipe-textual-ports)))
	  (push-compensation (close-port inp))
	  (push-compensation (close-port oup))
	  (put-string oup "ciao\n")
	  (flush-output-port oup)
	  (get-string-n inp 5)))
    => "ciao\n")

  )



(parameterize ((testname 'fifo))

  (define pathname
    (let ((p (string-join (list (get-environment-variable "TMPDIR") "fifo") "/")))
      (when (file-exists? p)
	(delete-file p))
      p))

  ;; binary port
  (check
      (with-compensations
	  (compensate
	      (mkfifo pathname #o600)
	    (with
	     (delete-file pathname)))
	(let ((in (open pathname (bitwise-ior O_NONBLOCK O_RDONLY) 0))
	      (ou (open pathname O_WRONLY 0)))
	  (letrec ((inp	(compensate
			    (fd->binary-input-port  in)
			  (with
			   (close-port inp))))
		   (oup	(compensate
			    (fd->binary-output-port ou)
			  (with
			   (close-port oup)))))
	    (put-bytevector oup
			    (string->bytevector "ciao\n"
						(native-transcoder)))
	    (flush-output-port oup)
	    (bytevector->string (get-bytevector-n inp 5)
				(native-transcoder)))))
    => "ciao\n")

  ;; textual port
  (check
      (with-compensations
	  (compensate
	      (mkfifo pathname #o600)
	    (with
	     (delete-file pathname)))
	(let ((in (open pathname (bitwise-ior O_NONBLOCK O_RDONLY) 0))
	      (ou (open pathname O_WRONLY 0)))
	  (letrec ((inp	(compensate
			    (fd->textual-input-port  in)
			  (with
			   (close-port inp))))
		   (oup	(compensate
			    (fd->textual-output-port ou)
			  (with
			   (close-port oup)))))
	    (put-string oup "ciao\n")
	    (flush-output-port oup)
	    (get-string-n inp 5))))
    => "ciao\n")

  )


;;;; done

(check-report)

;;; end of file
