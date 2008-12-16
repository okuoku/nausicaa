;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: test for file descriptors library
;;;Date: Sun Dec  7, 2008
;;;Time-stamp: <2008-12-16 10:23:44 marco>
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
  (uriel lang)
  (uriel printing)
  (uriel test)
  (uriel getenv)
  (rename (uriel ffi)
	  (string-or-symbol->cstring/compensated s->c))
  (srfi receive)
  (srfi parameters)
  (only (string-lib) string-join)
  (posix fd)
  (posix sizeof))

(check-set-mode! 'report-failed)

(define TMPDIR (getenv "TMPDIR"))

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
	       (bufptr		(s->c the-string))
	       (buflen		(strlen bufptr))
	       (buflen2		buflen)
	       (bufptr2		(compensate-malloc/block buflen2)))
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
	       (bufptr		(s->c the-string))
	       (buflen		(strlen bufptr))
	       (buflen2		buflen)
	       (bufptr2		(compensate-malloc/block buflen2)))
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
	       (bufptr		(s->c the-string))
	       (buflen		(strlen bufptr))
	       (buflen2		buflen)
	       (bufptr2		(compensate-malloc/block buflen2))
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
	       (bufptr		(s->c the-string))
	       (buflen		(strlen bufptr))
	       (buflen2		buflen)
	       (bufptr2		(compensate-malloc/block buflen2)))
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
	       (bufptr		(s->c the-string))
	       (buflen		(strlen bufptr))
	       (buflen2		buflen)
	       (bufptr2		(compensate-malloc/block buflen2)))
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


;;;; file locking

(parameterize ((testname 'lock))

  (check
      (with-compensations
	(let* ((pathname the-pathname)
	       (fd		(open pathname (bitwise-ior O_CREAT O_RDWR) #o600))
	       (bufptr		(s->c the-string))
	       (buflen		(strlen bufptr))
	       (buflen2		buflen)
	       (bufptr2		(compensate-malloc/block buflen2)))
	  (write fd bufptr buflen)
	  (lseek fd 0 SEEK_SET)
	  (let ((lock	(compensate-malloc/block sizeof-struct-flock)))
	    (struct-flock-l_type-set! lock F_WRLCK)
	    (struct-flock-l_whence-set! lock SEEK_SET)
	    (struct-flock-l_start-set! lock 0)
	    (struct-flock-l_len-set! lock 10)
	    (fcntl fd F_SETLK (pointer->integer lock))
	    (read fd bufptr2 buflen2)
	    (fcntl fd F_GETLK (pointer->integer lock))
;; 	    (display (list (struct-flock-l_type-ref lock)
;; 			   (struct-flock-l_start-ref lock)))(newline)
	    (fcntl fd F_UNLCK (pointer->integer lock))
	    (close fd))
	  (cstring->string/len bufptr2 buflen2)))
    => the-string)

#f)


;;;; done

(check-report)

;;; end of file
