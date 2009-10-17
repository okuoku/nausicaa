;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: tests for the process related POSIX functions
;;;Date: Fri Dec 19, 2008
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


(import (nausicaa)
  (foreign ffi)
  (foreign memory)
  (foreign errno)
  (foreign cstrings)
  (checks)
  (foreign posix process)
  (foreign posix process stub))

(check-set-mode! 'report-failed)


(parameterize ((check-test-name 'pid))

  (check
      (integer? (getpid))
    => #t)

  (check
      (integer? (getppid))
    => #t)

  (check
      (< (getppid) (getpid))
    => #t)

  #t)


(parameterize ((check-test-name 'fork))

  (check
      (let ((pid (fork)))
	(if (= 0 pid)
	    (exit)
	  #t))
    => #t)

  (check
      (let ()
	(define (fake-fork)
	  (raise-errno-error 'fork EINVAL))
	(parameterize ((primitive-fork-function fake-fork))
	  (guard (exc (else
		       (list (errno-condition? exc)
			     (errno-symbolic-value exc))))
	    (fork))))
    => '(#t EINVAL))

  #t)


(parameterize ((check-test-name 'exec))

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
		      (lambda args
			(raise-errno-error 'execv EINVAL args))))
	(guard (exc (else
		     (list (errno-condition? exc)
			   (condition-who exc)
			   (errno-symbolic-value exc))))
	  (execv '/bin/ls '(ls))))
    => '(#t execv EINVAL))

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
		      (lambda args
			(raise-errno-error 'execve EINVAL args))))
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
	  (raise-errno-error 'execve EINVAL args))
	(parameterize ((primitive-execvp-function fake-execvp))
	  (guard (exc (else
		       (list (errno-condition? exc)
			     (errno-symbolic-value exc))))
	    (execvp 'ls '(ls)))))
    => '(#t EINVAL))

  #t)


(parameterize ((check-test-name 'wait))

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



  #t)


;;;; done

(check-report)

;;; end of file
