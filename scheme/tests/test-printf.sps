;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for printf
;;;Date: Mon Jan 31, 2011
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!r6rs
(import (nausicaa)
  (nausicaa checks))

(check-set-mode! 'report-failed)
(display "*** testing printf\n")


(parametrise ((check-test-name	'base))

  (check
      (printf "")
    => "")

  (check
      (printf "ciao")
    => "ciao")

;;; --------------------------------------------------------------------

  (check
      (printf #f "")
    => "")

  (check
      (printf #f "ciao")
    => "ciao")

;;; --------------------------------------------------------------------

  (check
      (printf #t "output from printf, current output port\n")
    => "output from printf, current output port\n")

  (check
      (printf (current-error-port) "output from printf, current error port\n")
    => "output from printf, current error port\n")

  #t)


(parametrise ((check-test-name	'format))

  (check
      (printf "ciao ~~")
    => "ciao ~")

  (check
      (printf "ciao ~%")
    => "ciao \n")

  (check
      (printf "ciao ~a" "mamma")
    => "ciao mamma")

  (check
      (printf "ciao ~s" "mamma")
    => "ciao \"mamma\"")

  (check
      (printf "ciao ~ama" "mam")
    => "ciao mamma")

  (check
      (printf "ciao ~sma" "mam")
    => "ciao \"mam\"ma")

;;; --------------------------------------------------------------------

  (check
      (printf "ciao ~a" 'mamma)
    => "ciao mamma")

  (check
      (printf "ciao ~s" 'mamma)
    => "ciao mamma")

;;; --------------------------------------------------------------------

  (check
      (printf "ciao ~x mamma" 255)
    => "ciao ff mamma")

  (check
      (printf "ciao ~X mamma" 255)
    => "ciao FF mamma")

;;; --------------------------------------------------------------------

  (check
      (printf "ciao ~b mamma" 255)
    => "ciao 11111111 mamma")

  (check
      (printf "ciao ~B mamma" 255)
    => "ciao 11111111 mamma")

;;; --------------------------------------------------------------------

  (check
      (printf "ciao ~o mamma" 8)
    => "ciao 10 mamma")

  (check
      (printf "ciao ~O mamma" 8)
    => "ciao 10 mamma")

  #t)


(parametrise ((check-test-name	'format))

  (check
      (guard (E ((error? E)
;;;(write (condition-message E))(newline)
		 #t)
		(else E))
	(printf "ciao" 123))
    => #t)

  (check
      (guard (E ((error? E)
;;;(write (condition-message E))(newline)
		 #t)
		(else E))
	(printf "ciao ~a"))
    => #t)

  (check
      (guard (E ((error? E)
;;;(write (condition-message E))(newline)
		 #t)
		(else E))
	(printf "ciao ~x" 'c))
    => #t)

  (check
      (guard (E ((error? E)
;;;(write (condition-message E))(newline)
		 #t)
		(else E))
	(printf "ciao ~b" 'c))
    => #t)

  (check
      (guard (E ((error? E)
;;;(write (condition-message E))(newline)
		 #t)
		(else E))
	(printf "ciao ~O" 'c))
    => #t)

  (check
      (guard (E ((error? E)
;;;(write (condition-message E))(newline)
		 #t)
		(else E))
	(printf "ciao ~Z" 'c))
    => #t)

  #t)


;;;; done

(check-report)

;;; end of file
