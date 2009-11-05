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
  (prefix (foreign posix process) posix:)
  (prefix (foreign posix process record-types) posix:))

(check-set-mode! 'report-failed)
(display "*** testing POSIX process\n")


(parametrise ((check-test-name 'pid))

  (check
      (integer? (posix:getpid))
    => #t)

  (check
      (integer? (posix:getppid))
    => #t)

  (check
      (< (posix:getppid) (posix:getpid))
    => #t)

  #t)


(parametrise ((check-test-name 'fork))

  (check
      (let ((pid (posix:fork)))
	(if (= 0 pid)
	    (exit)
	  #t))
    => #t)

  (check
      (let ()
	(define (fake-fork)
	  (raise-errno-error 'fork EINVAL))
	(parametrise ((posix:fork-function fake-fork))
	  (guard (E (else (list (errno-condition? E)
				(errno-symbolic-value E))))
	    (posix:fork))))
    => '(#t EINVAL))

  #t)


(parameterize ((check-test-name 'exec))

  (check
      (begin
	(when (= 0 (posix:fork))
	  (posix:execv '/bin/ls '(ls "-l" /bin/ls))
	  (exit))
	#t)
    => #t)

  (check
      (begin
	(when (= 0 (posix:fork))
	  (posix:execv '/usr/bin/du '(du /bin/ls))
	  (exit))
	#t)
    => #t)

  (check
      (parameterize ((posix:execv-function (lambda args
					     (raise-errno-error 'execv EINVAL args))))
	(guard (E (else (list (errno-condition? E)
			      (condition-who E)
			      (errno-symbolic-value E))))
	  (posix:execv '/bin/ls '(ls))))
    => '(#t execv EINVAL))

;;; --------------------------------------------------------------------

  (check
      (begin
	(when (= 0 (posix:fork))
	  (posix:execve '/usr/bin/du '(du /bin/ls) '("BLOCK_SIZE=human-readable"))
	  (exit))
	#t)
    => #t)

  (check
      (parametrise ((posix:execve-function (lambda args
					     (raise-errno-error 'execve EINVAL args))))
	(guard (E (else (list (errno-condition? E)
			      (errno-symbolic-value E))))
	  (posix:execve '/usr/bin/du '(du /bin/ls) '("BLOCK_SIZE=human-readable"))))
    => '(#t EINVAL))

;;; --------------------------------------------------------------------

  (check
      (begin
	(when (= 0 (posix:fork))
	  (posix:execvp 'ls '(ls "-l" /bin/ls))
	  (exit))
	#t)
    => #t)

  (check
      (parametrise ((posix:execvp-function (lambda args
					     (raise-errno-error 'execve EINVAL args))))
	(guard (E (else (list (errno-condition? E)
			      (errno-symbolic-value E))))
	  (posix:execvp 'ls '(ls))))
    => '(#t EINVAL))

  #t)


(parameterize ((check-test-name 'wait))

  (check
      (let ((pid (posix:fork)))
	(if (= 0 pid)
	    (posix:execv '/bin/ls '(ls "-l" /bin/ls))
	  (receive (result status)
	      (posix:waitpid pid 0)
	    (= pid result))))
    => #t)

  (check
      (let ((pid (posix:fork)))
	(if (= 0 pid)
	    (posix:execv '/bin/ls '(ls "-l" /bin/ls))
	  (receive (result status)
	      (posix:waitpid pid 0)
	    (let ((r (posix:process-term-status->record status)))
	      (posix:WIFEXITED? r)))))
    => #t)

  #t)


;;;; done

(check-report)

;;; end of file
