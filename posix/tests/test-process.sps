;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: tests for the process related POSIX functions
;;;Date: Fri Dec 19, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008, 2009 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (posix typedefs)
  (prefix (posix process) posix:))

(check-set-mode! 'report-failed)
(display "*** testing POSIX process\n")


(parametrise ((check-test-name 'pid))

  (check
      (pid? (posix:getpid))
    => #t)

  (check
      (pid? (posix:getppid))
    => #t)

  (check
      (< (pid->integer (posix:getppid))
	 (pid->integer (posix:getpid)))
    => #t)

  #t)


(parametrise ((check-test-name 'fork))

  (check
      (let ((pid (posix:fork)))
	(if pid
	    #t
	  (exit)))
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
	(unless (posix:fork)
	  (posix:execv '/bin/ls '(ls "-l" /bin/ls))
	  (exit))
	#t)
    => #t)

  (check
      (begin
	(unless (posix:fork)
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
	(unless (posix:fork)
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
	(unless (posix:fork)
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
	(if pid
	    (receive (result status)
		(posix:waitpid pid 0)
	      (= (pid->integer pid) result))
	  (posix:execv '/bin/ls '(ls "-l" /bin/ls))))
    => #t)

  (check
      (let ((pid (posix:fork)))
	(if pid
	    (receive (result status)
		(posix:waitpid pid 0)
	      (let ((r (posix:integer-><process-term-status> status)))
		(WIFEXITED? r)))
	  (posix:execv '/bin/ls '(ls "-l" /bin/ls))))
    => #t)

  #t)


;;;; done

(check-report)

;;; end of file
