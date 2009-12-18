;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: tests for interprocess signals
;;;Date: Fri Dec 18, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (compensations)
  (posix sizeof)
  (posix typedefs)
  (prefix (posix signals) posix:)
  (prefix (posix process) posix:)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing POSIX interprocess signals\n")


(parametrise ((check-test-name	'enum))

  (check
      (interprocess-signal->symbol SIGSTOP)
    => 'SIGSTOP)

  (check
      (interprocess-signal->symbol SIGKILL)
    => 'SIGKILL)

;;; --------------------------------------------------------------------

  (check
      (symbol->interprocess-signal 'SIGSTOP)
    => SIGSTOP)

  (check
      (symbol->interprocess-signal 'SIGKILL)
    => SIGKILL)

  #t)


(parametrise ((check-test-name	'bub))

;;; use signal-raise

  (check
      (with-compensations
	(compensate
	    (posix:signal-bub-init)
	  (with
	   (posix:signal-bub-final)))
	(posix:signal-raise SIGUSR1)
	(posix:signal-bub-acquire)
	(list (posix:signal-bub-delivered? SIGUSR1)
	      (posix:signal-bub-delivered? SIGUSR2)))
    => '(#t #f))

  (check
      (with-compensations
	(compensate
	    (posix:signal-bub-init)
	  (with
	   (posix:signal-bub-final)))
	(posix:signal-bub-acquire)
	(list (posix:signal-bub-delivered? SIGUSR1)
	      (posix:signal-bub-delivered? SIGUSR2)))
    => '(#f #f))

  (check
      (with-compensations
	(compensate
	    (posix:signal-bub-init)
	  (with
	   (posix:signal-bub-final)))
	(posix:signal-raise SIGUSR2)
	(posix:signal-bub-acquire)
	(list (posix:signal-bub-delivered? SIGUSR1)
	      (posix:signal-bub-delivered? SIGUSR2)))
    => '(#f #t))

  (check
      (with-compensations
	(compensate
	    (posix:signal-bub-init)
	  (with
	   (posix:signal-bub-final)))
	(posix:signal-raise SIGUSR1)
	(posix:signal-raise SIGUSR2)
	(posix:signal-bub-acquire)
	(list (posix:signal-bub-delivered? SIGUSR1)
	      (posix:signal-bub-delivered? SIGUSR2)))
    => '(#t #t))

;;; --------------------------------------------------------------------
;;; use kill

  (check
      (with-compensations
	(compensate
	    (posix:signal-bub-init)
	  (with
	   (posix:signal-bub-final)))
	(posix:kill (posix:getpid) SIGUSR1)
	(posix:signal-bub-acquire)
	(list (posix:signal-bub-delivered? SIGUSR1)
	      (posix:signal-bub-delivered? SIGUSR2)))
    => '(#t #f))

  (check
      (with-compensations
	(compensate
	    (posix:signal-bub-init)
	  (with
	   (posix:signal-bub-final)))
	(posix:signal-bub-acquire)
	(list (posix:signal-bub-delivered? SIGUSR1)
	      (posix:signal-bub-delivered? SIGUSR2)))
    => '(#f #f))

  (check
      (with-compensations
	(compensate
	    (posix:signal-bub-init)
	  (with
	   (posix:signal-bub-final)))
	(posix:kill (posix:getpid) SIGUSR2)
	(posix:signal-bub-acquire)
	(list (posix:signal-bub-delivered? SIGUSR1)
	      (posix:signal-bub-delivered? SIGUSR2)))
    => '(#f #t))

  (check
      (with-compensations
	(compensate
	    (posix:signal-bub-init)
	  (with
	   (posix:signal-bub-final)))
	(posix:kill (posix:getpid) SIGUSR1)
	(posix:kill (posix:getpid) SIGUSR2)
	(posix:signal-bub-acquire)
	(list (posix:signal-bub-delivered? SIGUSR1)
	      (posix:signal-bub-delivered? SIGUSR2)))
    => '(#t #t))

;;; --------------------------------------------------------------------

  (check	;test clearing the flag
      (with-compensations
	(compensate
	    (posix:signal-bub-init)
	  (with
	   (posix:signal-bub-final)))
	(posix:signal-raise SIGUSR1)
	(posix:signal-bub-acquire)
	(let ((res (posix:signal-bub-delivered? SIGUSR1)))
	  (list res (posix:signal-bub-delivered? SIGUSR2))))
    => '(#t #f))

;;; --------------------------------------------------------------------

  (check
      (with-compensations
	(compensate
	    (posix:signal-bub-init)
	  (with
	   (posix:signal-bub-final)))
	(posix:signal-raise SIGUSR1)
	(posix:signal-bub-acquire)
	(posix:signal-bub-all-delivered))
    (=> enum-set=?) (interprocess-signals SIGUSR1))

  (check
      (with-compensations
	(compensate
	    (posix:signal-bub-init)
	  (with
	   (posix:signal-bub-final)))
	(posix:signal-raise SIGUSR1)
	(posix:signal-raise SIGUSR2)
	(posix:signal-bub-acquire)
	(posix:signal-bub-all-delivered))
    (=> enum-set=?) (interprocess-signals SIGUSR1 SIGUSR2))

  #t)


;;;; done

(check-report)

;;; end of file
