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

;;;SRFI-19: Time Data Types and Procedures.
;;;
;;;Modified by Derick Eddington to be included into the (srfi time) R6RS
;;;library.
;;;
;;;Copyright (C) I/NET, Inc. (2000, 2002, 2003). All Rights Reserved.
;;;
;;;This document and  translations of it may be  copied and furnished to
;;;others, and derivative works that  comment on or otherwise explain it
;;;or assist  in its implementation  may be prepared,  copied, published
;;;and  distributed, in  whole or  in part,  without restriction  of any
;;;kind, provided that the above copyright notice and this paragraph are
;;;included  on all  such copies  and derivative  works.   However, this
;;;document itself may  not be modified in any way,  such as by removing
;;;the  copyright  notice  or  references  to  the  Scheme  Request  For
;;;Implementation process  or editors, except as needed  for the purpose
;;;of  developing SRFIs  in  which case  the  procedures for  copyrights
;;;defined  in the  SRFI process  must be  followed, or  as  required to
;;;translate it into languages other than English.
;;;
;;;The limited permissions  granted above are perpetual and  will not be
;;;revoked by the authors or their successors or assigns.
;;;
;;;This document and the information  contained herein is provided on an
;;;"AS  IS" basis  and  THE AUTHOR  AND  THE SRFI  EDITORS DISCLAIM  ALL
;;;WARRANTIES,  EXPRESS OR  IMPLIED, INCLUDING  BUT NOT  LIMITED  TO ANY
;;;WARRANTY THAT THE USE OF THE INFORMATION HEREIN WILL NOT INFRINGE ANY
;;;RIGHTS OR ANY IMPLIED WARRANTIES  OF MERCHANTABILITY OR FITNESS FOR A
;;;PARTICULAR PURPOSE.


(import (nausicaa)
  (times-and-dates julian-calendar)
  (only (times-and-dates seconds-and-subseconds)
	$number-of-nanoseconds-in-one-second)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing times-and-dates julian day number\n")

(define epsilon 1e-3)
(define (eq=? a b)
  (< (abs (- a b)) epsilon))


(parametrise ((check-test-name	'number))

  (define-syntax check-jdn
    (syntax-rules ()
      ((_ ?year ?month ?day ?jdn)
       (receive (year month day)
	   (julian-day-decode-number ?jdn)
	 (check year	=> ?year)
	 (check month	=> ?month)
	 (check day	=> ?day)))))

;;; --------------------------------------------------------------------

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

  (check (julian-day-encode-number 2010  9 20) => 2455460)

;;; --------------------------------------------------------------------

  (check-jdn 2010 1 1 2455198)

  (check-jdn 2010 2 1 2455229)
  (check-jdn 2010 3 1 2455257)
  (check-jdn 2010 4 1 2455288)
  (check-jdn 2010 5 1 2455318)
  (check-jdn 2010 6 1 2455349)
  (check-jdn 2010 7 1 2455379)
  (check-jdn 2010 8 1 2455410)
  (check-jdn 2010 9 1 2455441)
  (check-jdn 2010 10 1 2455471)
  (check-jdn 2010 11 1 2455502)
  (check-jdn 2010 12 1 2455532)

  (check-jdn 2000 1 1 2451545)
  (check-jdn 2000 2 1 2451576)
  (check-jdn 2000 3 1 2451605)
  (check-jdn 2000 4 1 2451636)
  (check-jdn 2000 5 1 2451666)
  (check-jdn 2000 6 1 2451697)
  (check-jdn 2000 7 1 2451727)
  (check-jdn 2000 8 1 2451758)
  (check-jdn 2000 9 1 2451789)
  (check-jdn 2000 10 1 2451819)
  (check-jdn 2000 11 1 2451850)
  (check-jdn 2000 12 1 2451880)

  (check-jdn 2010  9 20 2455460)

  #t)


(parametrise ((check-test-name	'modified-number))

  (define-syntax check-mjdn
    (syntax-rules ()
      ((_ ?year ?month ?day ?mjdn)
       (check
	   (julian-day->modified-julian-day (julian-day-encode-number ?year ?month ?day))
	 => ?mjdn))))

  (define-syntax check-date
    (syntax-rules ()
      ((_ ?year ?month ?day ?mjdn)
       (check
	   (call-with-values
	       (lambda ()
		 (julian-day-decode-number (modified-julian-day->julian-day ?mjdn)))
	     list)
	 => '(?year ?month ?day)))))

;;; --------------------------------------------------------------------

  (check-mjdn 2010 1 1	 #e55197.5)
  (check-mjdn 2010 2 1	 #e55228.5)
  (check-mjdn 2010 3 1	 #e55256.5)
  (check-mjdn 2010 4 1	 #e55287.5)
  (check-mjdn 2010 5 1	 #e55317.5)
  (check-mjdn 2010 6 1	 #e55348.5)
  (check-mjdn 2010 7 1	 #e55378.5)
  (check-mjdn 2010 8 1	 #e55409.5)
  (check-mjdn 2010 9 1	 #e55440.5)
  (check-mjdn 2010 10 1	 #e55470.5)
  (check-mjdn 2010 11 1	 #e55501.5)
  (check-mjdn 2010 12 1	 #e55531.5)

;;; --------------------------------------------------------------------

  (check-date 2010 1 1	 #e55197.5)
  (check-date 2010 2 1	 #e55228.5)
  (check-date 2010 3 1	 #e55256.5)
  (check-date 2010 4 1	 #e55287.5)
  (check-date 2010 5 1	 #e55317.5)
  (check-date 2010 6 1	 #e55348.5)
  (check-date 2010 7 1	 #e55378.5)
  (check-date 2010 8 1	 #e55409.5)
  (check-date 2010 9 1	 #e55440.5)
  (check-date 2010 10 1	 #e55470.5)
  (check-date 2010 11 1	 #e55501.5)
  (check-date 2010 12 1	 #e55531.5)

  #t)


(parametrise ((check-test-name	'date))

  (define-syntax check-julian-date
    (syntax-rules ()
      ((_ ?year ?month ?day ?hours ?minutes ?seconds ?jd)
       (check
	   (inexact (time-point->julian-date ?year ?month ?day ?hours ?minutes ?seconds 0))
	 => ?jd))))

  (define-syntax check-time-point
    (syntax-rules ()
      ((_ ?date ?year ?month ?day ?hours ?minutes ?seconds)
       (receive (year month day hours minutes seconds nanoseconds)
	   (julian-date->time-point ?date)
	 (check year	=> ?year)
	 (check month	=> ?month)
	 (check day	=> ?day)
	 (check hours	=> ?hours)
	 (check minutes	=> ?minutes)
	 (check
	     (+ seconds	(/ nanoseconds $number-of-nanoseconds-in-one-second))
	   (=> eq=?) ?seconds)
	 ))))

;;; --------------------------------------------------------------------

;;;                  year month day hours minutes seconds
  (check-julian-date 2010  9    20  12    42      57		2455460.0298263887)
  (check-julian-date 2010  8    20  12    42      57		2455429.0298263887)
  (check-julian-date 2010 10    20  12    42      57		2455490.0298263887)
  (check-julian-date 1582 10    20  12    42      57		2299166.0298263887)
  (check-julian-date 1581 10    20  12    42      57		2298811.0298263887)

;;; --------------------------------------------------------------------

  (check-time-point 2455460.0298263887
;;;                 year month day hours minutes seconds
		    2010     9  20    12      42      57)

  (check-time-point 2455429.0298263887
;;;                 year month day hours minutes seconds
		    2010     8  20    12      42      57)

  (check-time-point 2455490.0298263887
;;;                 year month day hours minutes seconds
		    2010    10  20    12      42      57)


  (check-time-point 2299166.0298263887
;;;                 year month day hours minutes seconds
		    1582    10  20    12      42      57)

  (check-time-point 2298811.0298263887
;;;                 year month day hours minutes seconds
		    1581    10  20    12      42      57)

  #t)


;;;; done

(check-report)

;;; end of file
