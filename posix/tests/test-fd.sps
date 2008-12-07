;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: test for file descriptors library
;;;Date: Sun Dec  7, 2008
;;;Time-stamp: <2008-12-07 18:54:37 marco>
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

(import (except (rnrs) read write)
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
	       (fd		(open pathname (bitwise-ior O_RDWR) #o600))
	       (bufptr		(s->c the-string))
	       (buflen		(strlen bufptr))
	       (buflen2		buflen)
	       (bufptr2		(compensate-malloc/block buflen2)))
	  (write fd bufptr buflen)
	  (lseek fd 0 SEEK_SET)
	  (read fd bufptr2 buflen2)
	  (close fd)
	  (cstring->string/len bufptr2 buflen2)))
    => the-string)

  (check
      (with-compensations
	(let* ((pathname the-pathname)
	       (fd		(letrec
				    ((fd (compensate
					     (open pathname (bitwise-ior O_RDWR) #o600)
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
Ses ailes de geant l'empechent de marcher."))



;;;; done

(check-report)

;;; end of file
