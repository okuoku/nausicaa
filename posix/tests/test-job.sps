;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: tests for job control functions
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
  (checks)
  (foreign)
  (posix job)
  (posix process))

(check-set-mode! 'report-failed)



(debugging #t)


(parameterize ((check-test-name 'terminal-id))

  (check
      (ctermid)
    => "/dev/tty")

  )



(parameterize ((check-test-name 'group))

  (check
      (let ((pid (fork)))
	(when (= 0 pid)
	  (setsid)
	  (exit))
	#t)
    => #t)

  (check
      (integer? (getsid (getpid)))
    => #t)

  (check
      (integer? (getpgrp))
    => #t)

  (check
      (integer? (setpgid 0 0))
    => #t)

  )




(parameterize ((check-test-name 'access))

  (check
      (integer? (tcgetpgrp 1))
    => #t)

;;; Is there a way to test this without losing control of the terminal?
;;   (check
;;       (let ((pid (fork)))
;; 	(when (= 0 pid)
;; 	  (tcsetpgrp 1 (getpgrp))
;; 	  (exit))
;; 	#t)
;;     => #t)

  (check
      (integer? (tcgetsid 1))
    => #t)

  )


;;;; done

(check-report)

;;; end of file
