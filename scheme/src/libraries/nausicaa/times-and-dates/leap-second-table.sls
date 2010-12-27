;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: table of leap seconds
;;;Date: Fri Sep 17, 2010
;;;
;;;Abstract
;;;
;;;	This table can be automatically generated with the script in the
;;;	Nausicaa/Scheme sources "src/script/read-leap-seconds-table.sps"
;;;	applied to  the data file  "src/data/tai-utc.dat".  Updated data
;;;	files can be downloaded from:
;;;
;;;		<ftp://maia.usno.navy.mil/ser7/tai-utc.dat>
;;;
;;;	it may be required to edit  this data file, for example to break
;;;	the  occurrences of  "0.0011232S"  into "0.0011232  S"; this  is
;;;	because the script does not implement a proper parser.
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


#!r6rs
(library (times-and-dates leap-second-table)
  (export $leap-second-table)
  (import (rnrs))
  (define $leap-second-table
    ;;Each entry is:
    ;;
    ;;    ( <UTC seconds since Unix Epoch> .
    ;;      <number of seconds to add to UTC to compute TAI> )
    ;;
    ;;note they  go higher to  lower and end  in 1972.
    ;;
    '((1230768000 . 34)
      (1136073600 . 33)
      (915148800 . 32)
      (867715200 . 31)
      (820454400 . 30)
      (773020800 . 29)
      (741484800 . 28)
      (709948800 . 27)
      (662688000 . 26)
      (631152000 . 25)
      (567993600 . 24)
      (489024000 . 23)
      (425865600 . 22)
      (394329600 . 21)
      (362793600 . 20)
      (315532800 . 19)
      (283996800 . 18)
      (252460800 . 17)
      (220924800 . 16)
      (189302400 . 15)
      (157766400 . 14)
      (126230400 . 13)
      (94694400 . 12)
      (78796800 . 11)
      (63072000 . 10))))

;;; end of file
