;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: tests for job control functions
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
  (nausicaa checks)
  (nausicaa posix typedefs)
  (prefix (nausicaa posix process) px.))

(check-set-mode! 'report-failed)
(display "*** testing POSIX job control\n")


(parametrise ((check-test-name	'terminal-id)
	      (debugging	#t))

  (check
      (px.ctermid)
    => "/dev/tty")

  #t)


(parametrise ((check-test-name 'group))

  (check
      (let ((pid (px.fork)))
	(unless pid
	  (px.setsid)
	  (exit))
	#t)
    => #t)

  (check
      (pid? (px.getsid (px.getpid)))
    => #t)

  (check
      (pid? (px.getpgrp))
    => #t)

  (check
      (px.setpgid (integer->pid 0) (integer->pid 0))
    => 0)

  #t)


(parametrise ((check-test-name 'access))

  (check
      (pid? (px.tcgetpgrp (integer->fd 1)))
    => #t)

;;; Is there a way to test this without losing control of the terminal?
;;   (check
;;       (let ((pid (px.fork)))
;; 	(unless pid
;; 	  (px.tcsetpgrp (integer->fd 1) (px.getpgrp))
;; 	  (exit))
;; 	#t)
;;     => #t)

  (check
      (pid? (px.tcgetsid (integer->fd 1)))
    => #t)

  #t)


;;;; done

(check-report)

;;; end of file
