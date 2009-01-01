;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: tests for the basic POSIX interface
;;;Date: Sun Nov 30, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008, 2009 Marco Maggi <marcomaggi@gna.org>
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

(import (r6rs)
  (uriel lang)
  (uriel test)
  (uriel foreign)
  (posix environment)
  (posix working-directory))

(check-set-mode! 'report-failed)



(parameterize ((testname 'env))

  (check
      (let ()
	(setenv 'CIAO 'pasta 1)
	(getenv 'CIAO))
    => "pasta")

  (check
      (let ()
	(setenv 'SALUT 'pasta 1)
	(setenv 'SALUT 'fusillo 0)
	(getenv 'CIAO))
    => "pasta")

  )



(parameterize ((testname 'chdir))
  (check
      (let ((dirname '/))
	(chdir dirname))
    => 0)

  (check
      (let ((dirname '/usr/local/bin))
	(chdir dirname))
    => 0)

;;;This guard is needed because  Larceny-5880 does not support access to
;;;errno values, so this test has to deal with a non-&errno condition.
  (guard (exc (else #f))
    (check
	(let ((dirname '/scrappy/dappy/doo))
	  (guard (exc (else
		       #;(format #t "msg ~s ~s ~s ~s~%"
			       (condition-message exc)
			       (errno-condition? exc)
			       (condition-who exc)
			       (condition-irritants exc))
		       (list (errno-condition? exc)
			     (condition-who exc)
			     (errno-symbolic-value exc)
;;;			   (condition-message exc)
			     )))
	    (chdir dirname)))
      => '(#t primitive-chdir ENOENT))))

(parameterize ((testname 'getcwd))
  (check
      (let ((dirname '/usr/local/bin))
	(chdir dirname)
	(getcwd))
    => "/usr/local/bin")

  (check
      (let ((dirname '/bin))
	(chdir dirname)
	(pwd))
    => "/bin"))



;;;; done

(check-report)

;;; end of file
