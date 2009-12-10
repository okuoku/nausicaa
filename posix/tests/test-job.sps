;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: tests for job control functions
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
  (checks)
  (prefix (foreign posix job) posix:)
  (prefix (foreign posix process) posix:))

(check-set-mode! 'report-failed)
(display "*** testing POSIX job\n")


(parametrise ((check-test-name	'terminal-id)
	      (debugging	#t))

  (check
      (posix:ctermid)
    => "/dev/tty")

  #t)


(parametrise ((check-test-name 'group))

  (check
      (let ((pid (posix:fork)))
	(when (= 0 pid)
	  (posix:setsid)
	  (exit))
	#t)
    => #t)

  (check
      (integer? (posix:getsid (posix:getpid)))
    => #t)

  (check
      (integer? (posix:getpgrp))
    => #t)

  (check
      (integer? (posix:setpgid 0 0))
    => #t)

  #t)


(parametrise ((check-test-name 'access))

  (check
      (integer? (posix:tcgetpgrp 1))
    => #t)

;;; Is there a way to test this without losing control of the terminal?
;;   (check
;;       (let ((pid (posix:fork)))
;; 	(when (= 0 pid)
;; 	  (posix:tcsetpgrp 1 (posix:getpgrp))
;; 	  (exit))
;; 	#t)
;;     => #t)

  (check
      (integer? (posix:tcgetsid 1))
    => #t)

  #t)


;;;; done

(check-report)

;;; end of file
