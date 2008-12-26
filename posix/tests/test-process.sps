;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: tests for the process related POSIX functions
;;;Date: Fri Dec 19, 2008
;;;Time-stamp: <2008-12-26 17:59:53 marco>
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

(import (r6rs)
  (uriel lang)
  (uriel test)
  (uriel errno)
  (posix process)
  (posix process stub))

(check-set-mode! 'report-failed)



(parameterize ((testname 'pid))

  (check
      (integer? (getpid))
    => #t)

  (check
      (integer? (getppid))
    => #t)

  (check
      (< (getppid) (getpid))
    => #t)

  )



(parameterize ((testname 'fork))

  (check
      (let ((pid (fork)))
	(if (= 0 pid)
	    (exit)
	  #t))
    => #t)

  (check
      (let ()
	(define (fake-fork)
	  (values -1 EINVAL))
	(parameterize ((primitive-fork-function fake-fork))
	  (guard (exc (else
		       (list (errno-condition? exc)
			     (errno-symbolic-value exc))))
	    (fork))))
    => '(#t EINVAL))

  )



(parameterize ((testname 'exec))

  (check
      (begin
	(when (= 0 (fork))
	  (execv '/bin/ls '(ls "-l" /bin/ls))
	  (exit))
	#t)
    => #t)

  (check
      (begin
	(when (= 0 (fork))
	  (execv '/usr/bin/du '(du /bin/ls))
	  (exit))
	#t)
    => #t)

  (check
      (parameterize ((primitive-execv-function
		      (lambda args (values -1 EINVAL))))
	(guard (exc (else
		     (list (errno-condition? exc)
			   (errno-symbolic-value exc))))
	  (execv '/bin/ls '(ls))))
    => '(#t EINVAL))

;;; --------------------------------------------------------------------

  (check
      (begin
	(when (= 0 (fork))
	  (execve '/usr/bin/du '(du /bin/ls) '("BLOCK_SIZE=human-readable"))
	  (exit))
	#t)
    => #t)

  (check
      (parameterize ((primitive-execve-function
		      (lambda args (values -1 EINVAL))))
	(guard (exc (else
		     (list (errno-condition? exc)
			   (errno-symbolic-value exc))))
	  (execve '/usr/bin/du '(du /bin/ls) '("BLOCK_SIZE=human-readable"))))
    => '(#t EINVAL))

;;; --------------------------------------------------------------------

  (check
      (begin
	(when (= 0 (fork))
	  (execvp 'ls '(ls "-l" /bin/ls))
	  (exit))
	#t)
    => #t)

  (check
      (let ()
	(define (fake-execvp . args)
	  (values -1 EINVAL))
	(parameterize ((primitive-execvp-function fake-execvp))
	  (guard (exc (else
		       (list (errno-condition? exc)
			     (errno-symbolic-value exc))))
	    (execvp 'ls '(ls)))))
    => '(#t EINVAL))

  )




(parameterize ((testname 'wait))

  (check
      (let ((pid (fork)))
	(if (= 0 pid)
	    (execv '/bin/ls '(ls "-l" /bin/ls))
	  (receive (result status)
	      (waitpid pid 0)
	    (= pid result))))
    => #t)

  (check
      (let ((pid (fork)))
	(if (= 0 pid)
	    (execv '/bin/ls '(ls "-l" /bin/ls))
	  (receive (result status)
	      (waitpid pid 0)
	    (let ((r (make-process-term-status status)))
	      (WIFEXITED? r)))))
    => #t)



  )


;;;; done

(check-report)

;;; end of file
