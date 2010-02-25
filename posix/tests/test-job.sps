;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: tests for job control functions
;;;Date: Fri Dec 19, 2008
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
  (posix typedefs)
  (prefix (posix process) posix:))

(check-set-mode! 'report-failed)
(display "*** testing POSIX job control\n")


(parametrise ((check-test-name	'terminal-id)
	      (debugging	#t))

  (check
      (posix:ctermid)
    => "/dev/tty")

  #t)


(parametrise ((check-test-name 'group))

  (check
      (let ((pid (posix:fork)))
	(unless pid
	  (posix:setsid)
	  (exit))
	#t)
    => #t)

  (check
      (pid? (posix:getsid (posix:getpid)))
    => #t)

  (check
      (pid? (posix:getpgrp))
    => #t)

  (check
      (posix:setpgid (integer->pid 0) (integer->pid 0))
    => 0)

  #t)


(parametrise ((check-test-name 'access))

  (check
      (pid? (posix:tcgetpgrp (integer->fd 1)))
    => #t)

;;; Is there a way to test this without losing control of the terminal?
;;   (check
;;       (let ((pid (posix:fork)))
;; 	(unless pid
;; 	  (posix:tcsetpgrp (integer->fd 1) (posix:getpgrp))
;; 	  (exit))
;; 	#t)
;;     => #t)

  (check
      (pid? (posix:tcgetsid (integer->fd 1)))
    => #t)

  #t)


;;;; done

(check-report)

;;; end of file
