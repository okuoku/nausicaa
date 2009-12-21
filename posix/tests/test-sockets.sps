;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: tests for sockets functions
;;;Date: Mon Dec 21, 2009
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
  (pretty-print)
  (posix sizeof)
  (posix typedefs)
  (prefix (posix sockets) posix:)
  (prefix (posix fd) posix:)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing POSIX socket functions\n")


(parametrise ((check-test-name	'if-name))

  (check
      (posix:if-indextoname (posix:if-nametoindex "eth0"))
    => "eth0")

  (check
      (posix:if-indextoname (posix:if-nametoindex "lo"))
    => "lo")

  (check
      (let ((l (posix:if-nameindex)))
;;;	(pretty-print l)(newline)
	(for-all <struct-if-nameindex>? l))
    => #t)

  #t)


(parametrise ((check-test-name	'shutdown-modes))

  (check
      (socket-shutdown-mode->value (shutdown-mode read))
    => SHUT_RD)

  (check
      (socket-shutdown-mode->value (shutdown-mode write))
    => SHUT_WR)

  (check
      (socket-shutdown-mode->value (shutdown-mode both))
    => SHUT_RDWR)

;;; --------------------------------------------------------------------

  (check
      (value->socket-shutdown-mode SHUT_RD)
    (=> enum-set=?)
    (shutdown-mode read))

  (check
      (value->socket-shutdown-mode SHUT_WR)
    (=> enum-set=?)
    (shutdown-mode write))

  (check
      (value->socket-shutdown-mode SHUT_RDWR)
    (=> enum-set=?)
    (shutdown-mode both))

  #t)


;;;; done

(check-report)

;;; end of file
