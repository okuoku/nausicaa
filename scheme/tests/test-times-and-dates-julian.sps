;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for julian day functions
;;;Date: Thu Jul 29, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (times-and-dates julian-day)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing times-and-dates julian day number\n")


(parametrise ((check-test-name	'date))

;;;The  JDN results can  be computed  with the  calculator at  (URL last
;;;verified Thu Jul 29, 2010):
;;;
;;; <http://www.imcce.fr/en/grandpublic/temps/jour_julien.php>
;;;

  (check (julian-day-encode-number 2010 1 1)	=> 2455198)
  (check (julian-day-encode-number 2010 2 1)	=> 2455229)
  (check (julian-day-encode-number 2010 3 1)	=> 2455257)
  (check (julian-day-encode-number 2010 4 1)	=> 2455288)
  (check (julian-day-encode-number 2010 5 1)	=> 2455318)
  (check (julian-day-encode-number 2010 6 1)	=> 2455349)
  (check (julian-day-encode-number 2010 7 1)	=> 2455379)
  (check (julian-day-encode-number 2010 8 1)	=> 2455410)
  (check (julian-day-encode-number 2010 9 1)	=> 2455441)
  (check (julian-day-encode-number 2010 10 1)	=> 2455471)
  (check (julian-day-encode-number 2010 11 1)	=> 2455502)
  (check (julian-day-encode-number 2010 12 1)	=> 2455532)

  (check (julian-day-encode-number 2000 1 1)	=> 2451545)
  (check (julian-day-encode-number 2000 2 1)	=> 2451576)
  (check (julian-day-encode-number 2000 3 1)	=> 2451605)
  (check (julian-day-encode-number 2000 4 1)	=> 2451636)
  (check (julian-day-encode-number 2000 5 1)	=> 2451666)
  (check (julian-day-encode-number 2000 6 1)	=> 2451697)
  (check (julian-day-encode-number 2000 7 1)	=> 2451727)
  (check (julian-day-encode-number 2000 8 1)	=> 2451758)
  (check (julian-day-encode-number 2000 9 1)	=> 2451789)
  (check (julian-day-encode-number 2000 10 1)	=> 2451819)
  (check (julian-day-encode-number 2000 11 1)	=> 2451850)
  (check (julian-day-encode-number 2000 12 1)	=> 2451880)

;;; --------------------------------------------------------------------

  (check (julian-day->modified-julian-day (julian-day-encode-number 2010 1 1))	=> #e55197.5)
  (check (julian-day->modified-julian-day (julian-day-encode-number 2010 2 1))	=> #e55228.5)
  (check (julian-day->modified-julian-day (julian-day-encode-number 2010 3 1))	=> #e55256.5)
  (check (julian-day->modified-julian-day (julian-day-encode-number 2010 4 1))	=> #e55287.5)
  (check (julian-day->modified-julian-day (julian-day-encode-number 2010 5 1))	=> #e55317.5)
  (check (julian-day->modified-julian-day (julian-day-encode-number 2010 6 1))	=> #e55348.5)
  (check (julian-day->modified-julian-day (julian-day-encode-number 2010 7 1))	=> #e55378.5)
  (check (julian-day->modified-julian-day (julian-day-encode-number 2010 8 1))	=> #e55409.5)
  (check (julian-day->modified-julian-day (julian-day-encode-number 2010 9 1))	=> #e55440.5)
  (check (julian-day->modified-julian-day (julian-day-encode-number 2010 10 1))	=> #e55470.5)
  (check (julian-day->modified-julian-day (julian-day-encode-number 2010 11 1))	=> #e55501.5)
  (check (julian-day->modified-julian-day (julian-day-encode-number 2010 12 1))	=> #e55531.5)

  #t)


;;;; done

(check-report)

;;; end of file
