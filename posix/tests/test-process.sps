;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: tests for the process related POSIX functions
;;;Date: Fri Dec 19, 2008
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
  (nausicaa ffi)
  (nausicaa ffi memory)
  (nausicaa ffi errno)
  (nausicaa ffi cstrings)
  (nausicaa checks)
  (nausicaa posix typedefs)
  (prefix (nausicaa posix process) px.))

(check-set-mode! 'report-failed)
(display "*** testing POSIX process\n")


(parametrise ((check-test-name 'pid))

  (check
      (pid? (px.getpid))
    => #t)

  (check
      (pid? (px.getppid))
    => #t)

  (check
      (< (pid->integer (px.getppid))
	 (pid->integer (px.getpid)))
    => #t)

  #t)


(parametrise ((check-test-name 'fork))

  (check
      (let ((pid (px.fork)))
	(if pid
	    #t
	  (exit)))
    => #t)

  (check
      (let ()
	(define (fake-fork)
	  (raise-errno-error 'fork EINVAL))
	(parametrise ((px.fork-function fake-fork))
	  (guard (E (else (list (errno-condition? E)
				(errno-symbolic-value E))))
	    (px.fork))))
    => '(#t EINVAL))

  #t)


(parameterize ((check-test-name 'exec))

  (check
      (begin
	(unless (px.fork)
	  (px.execv '/bin/ls '(ls "-l" /bin/ls))
	  (exit))
	#t)
    => #t)

  (check
      (begin
	(unless (px.fork)
	  (px.execv '/usr/bin/du '(du /bin/ls))
	  (exit))
	#t)
    => #t)

  (check
      (parameterize ((px.execv-function (lambda args
					     (raise-errno-error 'execv EINVAL args))))
	(guard (E (else (list (errno-condition? E)
			      (condition-who E)
			      (errno-symbolic-value E))))
	  (px.execv '/bin/ls '(ls))))
    => '(#t execv EINVAL))

;;; --------------------------------------------------------------------

  (check
      (begin
	(unless (px.fork)
	  (px.execve '/usr/bin/du '(du /bin/ls) '("BLOCK_SIZE=human-readable"))
	  (exit))
	#t)
    => #t)

  (check
      (parametrise ((px.execve-function (lambda args
					     (raise-errno-error 'execve EINVAL args))))
	(guard (E (else (list (errno-condition? E)
			      (errno-symbolic-value E))))
	  (px.execve '/usr/bin/du '(du /bin/ls) '("BLOCK_SIZE=human-readable"))))
    => '(#t EINVAL))

;;; --------------------------------------------------------------------

  (check
      (begin
	(unless (px.fork)
	  (px.execvp 'ls '(ls "-l" /bin/ls))
	  (exit))
	#t)
    => #t)

  (check
      (parametrise ((px.execvp-function (lambda args
					     (raise-errno-error 'execve EINVAL args))))
	(guard (E (else (list (errno-condition? E)
			      (errno-symbolic-value E))))
	  (px.execvp 'ls '(ls))))
    => '(#t EINVAL))

  #t)


(parameterize ((check-test-name 'wait))

  (check
      (let ((pid (px.fork)))
	(if pid
	    (receive (result status)
		(px.waitpid pid 0)
	      (pid=? pid result))
	  (exit 0)))
    => #t)

  (check
      (let ((pid (px.fork)))
	(if pid
	    (receive (result status)
		(px.waitpid pid 0)
	      (let ((r (px.integer-><process-term-status> status)))
		(WIFEXITED? r)))
	  (exit 0)))
    => #t)

  ;; (check
  ;;     (let ((pid (px.fork)))
  ;; 	(if pid
  ;; 	    (receive (result status)
  ;; 		(px.waitpid/any 0)
  ;; 	      (write (list pid result))(newline)
  ;; 	      (pid=? pid result))
  ;; 	  (exit 0)))
  ;;   => #t)

  ;; (check
  ;;     (let ((pid (px.fork)))
  ;; 	(if pid
  ;; 	    (receive (result status)
  ;; 		(px.waitpid/any-my-group 0)
  ;; 	      ;;(write (list pid result))(newline)
  ;; 	      (pid=? pid result))
  ;; 	  (exit 0)))
  ;;   => #t)

  ;; (check
  ;;     (let ((pid (px.fork)))
  ;; 	(if pid
  ;; 	    (receive (result status)
  ;; 		(px.waitpid/group (px.getppid) 0)
  ;; 	      ;;(write (list pid result))(newline)
  ;; 	      (pid=? pid result))
  ;; 	  (exit 0)))
  ;;   => #t)

  #t)


;;;; done

(check-report)

;;; end of file
