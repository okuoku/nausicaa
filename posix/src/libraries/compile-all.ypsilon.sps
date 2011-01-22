;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: compile script for Ypsilon Scheme
;;;Date: Tue Nov  3, 2009
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

(import
  (only (nausicaa posix fd))
  (only (nausicaa posix file))
  (only (nausicaa posix process))
  (only (nausicaa posix signals))
  (only (nausicaa posix sockets))
  (only (nausicaa posix system))
  (only (nausicaa posix time))

  (only (nausicaa glibc file))
  (only (nausicaa glibc signals))
  (only (nausicaa glibc sockets))
  (only (nausicaa glibc streams))
  (only (nausicaa glibc system))
  (only (nausicaa glibc time))

  (only (nausicaa linux fd))
  )

;;; end of file
