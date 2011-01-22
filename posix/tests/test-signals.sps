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
;;;Copyright (c) 2009-2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (posix sizeof)
  (nausicaa posix typedefs)
  (prefix (nausicaa posix signals) px.)
  (prefix (nausicaa posix process) px.)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing POSIX interprocess signals\n")


(parametrise ((check-test-name	'enum))

  (check
      (value->unix-signal-symbol SIGSTOP)
    => 'SIGSTOP)

  (check
      (value->unix-signal-symbol SIGKILL)
    => 'SIGKILL)

;;; --------------------------------------------------------------------

  (check
      (unix-signal-symbol->value 'SIGSTOP)
    => SIGSTOP)

  (check
      (unix-signal-symbol->value 'SIGKILL)
    => SIGKILL)

;;; --------------------------------------------------------------------

  (check
      (value->unix-signal SIGSTOP)
    (=> enum-set=?) (unix-signal SIGSTOP))

  (check
      (value->unix-signal SIGKILL)
    (=> enum-set=?) (unix-signal SIGKILL))

;;; --------------------------------------------------------------------

  (check
      (unix-signal->value (unix-signal SIGSTOP))
    => SIGSTOP)

  (check
      (unix-signal->value (unix-signal SIGKILL))
    => SIGKILL)

  #t)


(parametrise ((check-test-name	'bub))

;;; use signal-raise

  (check
      (with-compensations
	(compensate
	    (px.signal-bub-init)
	  (with
	   (px.signal-bub-final)))
	(px.signal-raise* (unix-signal SIGUSR1))
	(px.signal-bub-acquire)
	(list (px.signal-bub-delivered*? (unix-signal SIGUSR1))
	      (px.signal-bub-delivered*? (unix-signal SIGUSR2))))
    => '(#t #f))

  (check
      (with-compensations
	(compensate
	    (px.signal-bub-init)
	  (with
	   (px.signal-bub-final)))
	(px.signal-bub-acquire)
	(list (px.signal-bub-delivered? SIGUSR1)
	      (px.signal-bub-delivered? SIGUSR2)))
    => '(#f #f))

  (check
      (with-compensations
	(compensate
	    (px.signal-bub-init)
	  (with
	   (px.signal-bub-final)))
	(px.signal-raise* (unix-signal SIGUSR2))
	(px.signal-bub-acquire)
	(list (px.signal-bub-delivered? SIGUSR1)
	      (px.signal-bub-delivered? SIGUSR2)))
    => '(#f #t))

  (check
      (with-compensations
	(compensate
	    (px.signal-bub-init)
	  (with
	   (px.signal-bub-final)))
	(px.signal-raise SIGUSR1)
	(px.signal-raise SIGUSR2)
	(px.signal-bub-acquire)
	(list (px.signal-bub-delivered? SIGUSR1)
	      (px.signal-bub-delivered? SIGUSR2)))
    => '(#t #t))

;;; --------------------------------------------------------------------
;;; use kill

  (check
      (with-compensations
	(compensate
	    (px.signal-bub-init)
	  (with
	   (px.signal-bub-final)))
	(px.kill* (px.getpid) (unix-signal SIGUSR1))
	(px.signal-bub-acquire)
	(list (px.signal-bub-delivered? SIGUSR1)
	      (px.signal-bub-delivered? SIGUSR2)))
    => '(#t #f))

  (check
      (with-compensations
	(compensate
	    (px.signal-bub-init)
	  (with
	   (px.signal-bub-final)))
	(px.signal-bub-acquire)
	(list (px.signal-bub-delivered? SIGUSR1)
	      (px.signal-bub-delivered? SIGUSR2)))
    => '(#f #f))

  (check
      (with-compensations
	(compensate
	    (px.signal-bub-init)
	  (with
	   (px.signal-bub-final)))
	(px.kill (px.getpid) SIGUSR2)
	(px.signal-bub-acquire)
	(list (px.signal-bub-delivered? SIGUSR1)
	      (px.signal-bub-delivered? SIGUSR2)))
    => '(#f #t))

  (check
      (with-compensations
	(compensate
	    (px.signal-bub-init)
	  (with
	   (px.signal-bub-final)))
	(px.kill* (px.getpid) (unix-signal SIGUSR1))
	(px.kill* (px.getpid) (unix-signal SIGUSR2))
	(px.signal-bub-acquire)
	(list (px.signal-bub-delivered? SIGUSR1)
	      (px.signal-bub-delivered? SIGUSR2)))
    => '(#t #t))

;;; --------------------------------------------------------------------

  (check	;test clearing the flag
      (with-compensations
	(compensate
	    (px.signal-bub-init)
	  (with
	   (px.signal-bub-final)))
	(px.signal-raise SIGUSR1)
	(px.signal-bub-acquire)
	(let ((res (px.signal-bub-delivered? SIGUSR1)))
	  (list res (px.signal-bub-delivered? SIGUSR2))))
    => '(#t #f))

;;; --------------------------------------------------------------------

  (check
      (with-compensations
	(compensate
	    (px.signal-bub-init)
	  (with
	   (px.signal-bub-final)))
	(px.signal-raise SIGUSR1)
	(px.signal-bub-acquire)
	(px.signal-bub-all-delivered))
    (=> enum-set=?) (unix-signals SIGUSR1))

  (check
      (with-compensations
	(compensate
	    (px.signal-bub-init)
	  (with
	   (px.signal-bub-final)))
	(px.signal-raise SIGUSR1)
	(px.signal-raise SIGUSR2)
	(px.signal-bub-acquire)
	(px.signal-bub-all-delivered))
    (=> enum-set=?) (unix-signals SIGUSR1 SIGUSR2))

  #t)


;;;; done

(check-report)

;;; end of file
