;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: direct bindings for system configuration inspection
;;;Date: Wed Dec  9, 2009
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


(library (foreign posix system platform)
  (export
    sysconf		pathconf
    fpathconf		confstr)
  (import (rnrs)
    (foreign ffi)
    (foreign ffi sizeof)
    (foreign posix shared-object))
  (define-c-functions/with-errno libc-shared-object
    (sysconf		(long sysconf (int)))
    (pathconf		(long pathconf (char* int)))
    (fpathconf		(long fpathconf (int int)))
    (confstr		(size_t confstr (int char* size_t)))))

;;; end of file
