;;;
;;;Part of: Uriel libraries for R6RS Scheme
;;;Contents: size of platform C language types
;;;Date: Mon Nov 24, 2008
;;;Time-stamp: <2008-11-27 17:08:16 marco>
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
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

(library (uriel ffi sizeof)
  (export
    sizeof-short-int sizeof-int sizeof-long sizeof-pointer
    sizeof-long-long

    valueof-char-max		valueof-char-min
    valueof-schar-max		valueof-schar-min	valueof-uchar-max
    valueof-shrt-max		valueof-shrt-min	valueof-ushrt-max
    valueof-int-max		valueof-int-min		valueof-uint-max
    valueof-long-max		valueof-long-min	valueof-ulong-max
    valueof-long-long-max	valueof-long-long-min	valueof-ulong-long-max
    valueof-wchar-max		valueof-ssize-max
    words-bigendian		on-64-bits-system	on-32-bits-system)
  (import (rnrs))

  (define sizeof-short-int 2)
  (define sizeof-int 4)
  (define sizeof-long 4)
  (define sizeof-long-long 8)
  (define sizeof-pointer 4)

  (define valueof-char-max 127)
  (define valueof-char-min -128)
  (define valueof-schar-max 127)
  (define valueof-schar-min -128)
  (define valueof-uchar-max 255)
  (define valueof-shrt-max 32767)
  (define valueof-shrt-min -32768)
  (define valueof-ushrt-max 65535)
  (define valueof-int-max 2147483647)
  (define valueof-int-min -2147483648)
  (define valueof-uint-max 4294967295)
  (define valueof-long-max 2147483647)
  (define valueof-long-min -2147483648)
  (define valueof-ulong-max 4294967295)
  (define valueof-long-long-max #f)
  (define valueof-long-long-min #f)
  (define valueof-ulong-long-max #f)
  (define valueof-wchar-max 2147483647)
  (define valueof-ssize-max 2147483647)

  (define words-bigendian (= 1 0))

  (define on-32-bits-system (= sizeof-int 4))
  (define on-64-bits-system (= sizeof-int 8))
  )

;;; end of file
